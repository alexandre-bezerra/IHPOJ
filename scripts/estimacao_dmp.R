################################################################################
# Projeto: Impactos da Pandemia sobre a Ocupação de Jovens no Brasil
# Script: Modelagem Econométrica do modelo de Search & Matching (DMP)
# Autor: Alexandre Bezerra dos Santos
# Instituição: Universidade Federal de Pernambuco
# Orientador: Cristiano da Costa da Silva
# Curso: Graduação em Ciências Econômicas
# Data: 22/05/2025
# Versão: 1.0
#
# Descrição:
# Este script parte de uma abordagem diferente da inicial, onde ele faz as
# as estimativas da tightness, no período de 2012-2023, para a população
# em geral, e depois, para o jovens, para fazer comprações. Depois parte
# para estimação do modelo estrutural.
#
# Uso dos Dados:
# Dados públicos do IBGE, obtidos via PNAD Contínua Trimestral (2012–2023), e
# dados de admissões do Novo Caged.
# Reproduzido com fins acadêmicos, em conformidade com as diretrizes do IBGE.
#
# Requisitos:
# - R versão >= 4.0
# - Pacotes: haven, dplyr, stringr, lubridate, ggplot2, tidyr e fmsb
#
# Licença:
# Este código está licenciado sob os termos da licença MIT.
# Você pode reutilizá-lo, modificá-lo e distribuí-lo, com os devidos créditos.
########################################################################################

########################################################################################
# 1 - Carregamento dos pacotes necessários #############################################
########################################################################################
library(haven)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(fmsb)
library(ggeffects)
library(patchwork)
library(zoo)
library(tidyverse)


#########################################################################################
# 2 - Importação dos microdados filtrados ###############################################
#########################################################################################
microdados <- readRDS("C:/Users/alexa/PNADC/Dados/pnadcontinua/microdados_amplo.RDS")


#########################################################################################
# 3 - Preparação dos microdados e criação de novas variáveis ############################
#########################################################################################
microdados <- microdados %>%
  arrange(hous_id, id_pessoa, Ano, Trimestre) %>%
  group_by(hous_id, id_pessoa) %>%
  mutate(
    estado_t1 = lead(estado_ocupacional),
    ano_t1 = lead(Ano),
    trimestre_t1 = lead(Trimestre)
  ) %>%
  ungroup() %>%
  filter(!is.na(estado_t1)) %>%
  filter((ano_t1 == Ano & trimestre_t1 == Trimestre + 1) |
           (ano_t1 == Ano + 1 & trimestre_t1 == 1 & Trimestre == 4)) %>%
  mutate(
    transicao_emprego = ifelse(estado_ocupacional == 0 & estado_t1 == 1, 1, 0),
    transicao_desemprego = ifelse(estado_ocupacional == 1 & estado_t1 == 0, 1, 0),
    periodo = paste0(Ano, "T", Trimestre)
  )


##################################################################################
# 4. Agregar dados populacionais e estimar theta global #########################
#################################################################################
painel_geral <- microdados %>%
  filter(estado_ocupacional %in% c(0, 1), estado_t1 %in% c(0, 1)) %>%
  group_by(periodo) %>%
  summarise(
    U = sum(estado_ocupacional == 0),
    E = sum(estado_ocupacional == 1),
    T_UE = sum(transicao_emprego),
    T_EU = sum(transicao_desemprego),
    .groups = "drop"
  ) %>%
  mutate(
    f_t = T_UE / U,
    s_t = T_EU / E,
    u_t = U / (U + E)
  )


# Importação e Tratamento de dados de admissões do Caged
caged <- read.csv("../dados/admissoes_caged.csv", sep=";") # === ETAPA 2: Calcular tightness geral e estimar função de matching ===

# Agregação do CAGED total por trimestre
caged_geral <- caged %>%
  mutate(ano = as.integer(ano), mes = as.integer(mes)) %>%
  mutate(trimestre = case_when(
    mes %in% 1:3 ~ 1,
    mes %in% 4:6 ~ 2,
    mes %in% 7:9 ~ 3,
    mes %in% 10:12 ~ 4
  )) %>%
  group_by(ano, trimestre) %>%
  summarise(vagas = sum(admissoes, na.rm = TRUE), .groups = "drop") %>%
  mutate(periodo = paste0(ano, "T", trimestre)) %>%
  select(periodo, vagas)

# Junção ao painel geral
painel_matching <- painel_geral %>%
  inner_join(caged_geral, by = "periodo") %>%
  mutate(
    theta = vagas / U,
    log_f = log(f_t),
    log_theta = log(theta)
  )

# Estimação da função log-log
modelo_matching <- lm(log_f ~ log_theta, data = painel_matching)
alpha_geral <- 1 - coef(modelo_matching)[["log_theta"]]


################################################################################
# 5. Criação de painel jovem e estimação de eficiência #########################
################################################################################

# Filtro de jovens (14–29 anos)
painel_jovem <- microdados %>%
  filter(estado_ocupacional %in% c(0, 1),
         estado_t1 %in% c(0, 1),
         V2009 >= 14, V2009 <= 29) %>%
  group_by(periodo) %>%
  summarise(
    U = sum(estado_ocupacional == 0),
    E = sum(estado_ocupacional == 1),
    T_UE = sum(transicao_emprego),
    T_EU = sum(transicao_desemprego),
    .groups = "drop"
  ) %>%
  mutate(
    f_t = T_UE / U,
    s_t = T_EU / E,
    u_t = U / (U + E)
  )

# Agregação de admissões jovens por trimestre
caged_jovem_trimestral <- caged %>%
  mutate(ano = as.integer(ano), mes = as.integer(mes)) %>%
  mutate(trimestre = case_when(
    mes %in% 1:3 ~ 1,
    mes %in% 4:6 ~ 2,
    mes %in% 7:9 ~ 3,
    mes %in% 10:12 ~ 4
  )) %>%
  mutate(adm_jovens = `até.17` + `X18.a.24` + `X25.a.29`) %>%
  group_by(ano, trimestre) %>%
  summarise(admissoes_jovens = sum(adm_jovens, na.rm = TRUE), .groups = "drop") %>%
  mutate(periodo = paste0(ano, "T", trimestre)) %>%
  select(periodo, admissoes_jovens)

# Calculo da eficiência de matching dos jovens
painel_jovem_eff <- painel_jovem %>%
  inner_join(caged_jovem_trimestral, by = "periodo") %>%
  mutate(
    theta_jovem = admissoes_jovens / U,
    eficiencia = f_t / (theta_jovem^(1 - alpha_geral))
  )

# Exportação dos dados trimestrais de evolução de ocupação juvenil
saveRDS(painel_jovem_eff, "../dados/painel_trimestral_jovem.RDS")


################################################################################
# 6. Gráficos ##################################################################
################################################################################

# Garantia de que o período seja fator ordenado corretamente
painel_jovem_eff <- painel_jovem_eff %>%
  arrange(periodo) %>%
  mutate(periodo = factor(periodo, levels = unique(periodo)))

# Gráfico de linha
ggplot(painel_jovem_eff, aes(x = periodo, y = eficiencia, group = 1)) +
  geom_line(color = 'blue', size = 1.1) +
  geom_point(color = 'black', size = 1.5) +
  labs(
    title = 'Eficiência de Matching dos Jovens no Mercado de Trabalho',
    subtitle = 'Brasil – Trimestral (2012–2019)',
    x = 'Trimestre',
    y = 'Eficiência relativa (f_t / θ_t^{1 - α})'
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
