################################################################################
# Projeto:Impactos Heterogêneos da Pandemia sobre a Ocupação de Jovens no Brasil
# Script: Modelagem Econométrica (Search & Matching)
# Autor: Alexandre Bezerra dos Santos
# Instituição: Universidade Federal de Pernambuco
# Orientador: Cristiano da Costa da Silva
# Curso: Graduação em Ciências Econômicas
# Data: 20/05/2025
# Versão: 1.0
#
# Descrição:
# Este script tem como objetivo modelar a probabilidade de um jovem
# conseguir emprego, condicional às suas características de raça,
# escolaridade, experiência e região.
#
# Uso dos Dados:
# Dados públicos do IBGE, obtidos via PNAD Contínua Trimestral (2019–2022).
# Reproduzido com fins acadêmicos, em conformidade com as diretrizes do IBGE.
#
# Requisitos:
# - R versão >= 4.0
# - Pacotes: haven, dplyr, stringr, lubridate, ggplot2, tidyr e fmsb
#
# Licença:
# Este código está licenciado sob os termos da licença MIT.
# Você pode reutilizá-lo, modificá-lo e distribuí-lo, com os devidos créditos.
################################################################################

################################################################################
# 1 - Carregamento dos pacotes necessários #####################################
################################################################################

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

################################################################################
# 2 - Importação dos microdados filtrados ######################################
################################################################################

microdados <- readRDS("microdados_filtrados.RDS")


################################################################################
# 3 - Preparação dos microdados e criação de novas variáveis ###################
################################################################################

# Ordenação dos dados por indivíduo e tempo
microdados <- microdados %>%
  arrange(id_pessoa, Ano, Trimestre)

# Criação da variável de estado no próximo trimestre
microdados <- microdados %>%
  group_by(id_pessoa) %>%
  mutate(estado_t1 = lead(estado_ocupacional), Ano_t1 = lead(Ano),
         Trimestre_t1 = lead(Trimestre)) %>%
  ungroup()

# Filtro apenas de transições consecutivas (diferença de 1 trimestre)
microdados_transicao <- microdados %>%
  filter(!is.na(estado_ocupacional), !is.na(estado_t1)) %>%
  mutate(distancia_temporal = (Ano_t1 - Ano) * 4 + (Trimestre_t1 - Trimestre)
  ) %>%
  filter(distancia_temporal == 1)

# Separação dos períodos de análise
microdados_transicao <- microdados_transicao %>%
  mutate(periodo = case_when(
    Ano < 2020 ~ "Pré-pandemia",
    Ano >= 2020 & Ano <= 2021 ~ "Durante pandemia",
    Ano > 2021 ~ "Pós-pandemia"
  ))

# Explicitação das variáveis pessoais
microdados_transicao <- microdados_transicao %>%
  mutate(
    # Agrupar sexo
    sexo = case_when(
      V2007 == 1 ~ "Homem",
      V2007 == 2 ~ "Mulher",
      TRUE ~ NA_character_
    ),
    
    # Agrupar raça
    raca = case_when(
      V2010 == 1 ~ "Branca",
      V2010 == 2 ~ "Preta",
      V2010 == 4 ~ "Parda",
      TRUE ~ "Outros"
    ),
    
    # Agrupar escolaridade (nível mais elevado)
    escolaridade = case_when(
      V3009A %in% 1:3 ~ "Fundamental ou menos",
      V3009A %in% 4:5 ~ "Médio",
      V3009A >= 6 ~ "Superior",
      TRUE ~ NA_character_
    ),
    
    # Agrupar idade como proxy de experiência
    experiencia = case_when(
      V2009 <= 17 ~ "Adolescente (14-17)",
      V2009 <= 24 ~ "Jovem (18-24)",
      V2009 <= 29 ~ "Jovem adulto (25-29)",
      TRUE ~ NA_character_
    ),
    # Agrupar localidade (capital ou interior)
    localidade = case_when(
      V1022 == 1 ~ "Capital",
      V1022 == 2 ~ "Interior",
      TRUE ~ NA_character_
    )
  )

# Criar variável resposta: 1 se saiu do desemprego para emprego
microdados_transicao <- microdados_transicao %>%
  mutate(transicao_emprego = ifelse(estado_ocupacional == 0 &
                                      estado_t1 == 1, 1, 0)) %>%
  filter(!is.na(transicao_emprego))

# Preparando variáveis dependentes

# criar variável binária para mulher
microdados_transicao$mulher <- ifelse(microdados_transicao$V2007 == 2, 1, 0)

microdados_transicao$raca <- factor(microdados_transicao$raca, 
                            levels = c("Branca", "Preta", "Parda","Outros"))

microdados_transicao$escolaridade <- factor(microdados_transicao$escolaridade, 
                        levels = c("Fundamental ou menos","Médio", "Superior"))

microdados_transicao$localidade <- factor(microdados_transicao$localidade,
                                          levels = c("Interior", "Capital"))

microdados_transicao$periodo <- factor(microdados_transicao$periodo,
                levels = c("Pré-pandemia", "Durante pandemia", "Pós-pandemia"))

microdados_transicao$experiencia <- factor(microdados_transicao$experiencia,
    levels= c("Adolescente (14-17)", "Jovem (18-24)", "Jovem adulto (25-29)"))

################################################################################
# 4 - Especificação e Estimação de Modelos de regressão logística ##############
################################################################################

# Modelo Sem Interações
modelo_logit <- glm(transicao_emprego ~ mulher + raca +
                      escolaridade + localidade + experiencia + periodo,
                    data = microdados_transicao,
                    family = binomial)

summary(modelo_logit)          # Coeficientes em log-odds
exp(coef(modelo_logit))        # Odds-ratios

# Modelo com Interação
modelo_interacoes <- glm(transicao_emprego ~ mulher * periodo + raca * periodo +
                           escolaridade * periodo + localidade * periodo +
                           experiencia * periodo,
                         data = microdados_transicao,
                         family = binomial)

summary(modelo_interacoes)
exp(coef(modelo_interacoes))

################################################################################
# 5 - Estimação de Modelo DIF-IN-DIF com Heterogeneidades ######################
################################################################################

microdados_transicao$pandemia_fase <- factor(
  microdados_transicao$periodo,
  levels = c("Pré-pandemia", "Durante pandemia", "Pós-pandemia")
)

modelo_3fases <- glm(
  transicao_emprego ~ pandemia_fase * (mulher + raca + escolaridade +
                                         localidade + experiencia),
  data = microdados_transicao,
  family = binomial
)

summary(modelo_3fases)
exp(coef(modelo_3fases))


################################################################################
# 6 - Geração de gráficos com efeitos marginais ################################
################################################################################

# Gráfico: Sexo
g1 <- ggpredict(modelo_3fases, terms = c("pandemia_fase", "mulher")) %>%
  plot() +
  labs(title = "Efeito da pandemia por sexo") +
  theme_minimal()

# Gráfico: Raça
g2 <- ggpredict(modelo_3fases, terms = c("pandemia_fase", "raca")) %>%
  plot() +
  labs(title = "Efeito da pandemia por raça") +
  theme_minimal()

# Gráfico: Escolaridade
g3 <- ggpredict(modelo_3fases, terms = c("pandemia_fase", "escolaridade")) %>%
  plot() +
  labs(title = "Efeito da pandemia por escolaridade") +
  theme_minimal()

# Gráfico: Localidade
g4 <- ggpredict(modelo_3fases, terms = c("pandemia_fase", "localidade")) %>%
  plot() +
  labs(title = "Efeito da pandemia por localidade") +
  theme_minimal()

# Gráfico: Faixa etária (experiência)
g5 <- ggpredict(modelo_3fases, terms = c("pandemia_fase", "experiencia")) %>%
  plot() +
  labs(title = "Efeito da pandemia por faixa etária") +
  theme_minimal()

# Montar quadro com patchwork
quadro <- (g1 | g2) / (g3 | g4)
quadro + plot_annotation(title = paste0("Efeitos marginais da pandemia por",
" grupo social (Período x Probabilidade prevista de conseguir emprego)"))

g5

################################################################################
# 7 - Aplicação de Modelo de Search & Matching #################################
################################################################################

################## CONSTRUÇÃO DE PAINEL TRIMESTRAL INTEGRADO ###################

# Criação de identificação de transição para o desemprego
microdados_transicao <- microdados_transicao %>%
  mutate(transicao_desemprego = ifelse(estado_ocupacional == 1 & estado_t1 == 0,
                                       1, 0)) %>%
  filter(!is.na(transicao_desemprego))

# Construção de variável de período
microdados_transicao <- microdados_transicao %>%
  mutate(periodo = paste(Ano, Trimestre, sep = "."))


# Obtenção de dados de ocupação geral
caged <- read.csv("admissoes_caged.csv", sep=";") %>% 
  mutate(data = as.Date(paste0(ano, "-", mes, "-01")))

admissoes_trimestre <- caged %>%
  mutate(Ano = year(data),
         Trimestre = quarter(data)) %>%
  group_by(Ano, Trimestre) %>%
  summarise(vagas_formais = sum(admissao), .groups = "drop") %>%
  mutate(periodo = paste(Ano, Trimestre, sep = "."))

# Criação de Painel
painel_geral <- microdados_transicao %>%
  mutate(periodo = paste(Ano, Trimestre, sep = ".")) %>%
  filter(estado_ocupacional %in% c(0, 1), !is.na(estado_t1)) %>%
  group_by(periodo) %>%
  summarise(
    U = sum(estado_ocupacional == 0),
    E = sum(estado_ocupacional == 1),
    T_UE = sum(transicao_emprego == 1, na.rm = TRUE)
  ) %>%
  mutate(f_t = T_UE / U)

# União ao painel de jovens
painel_geral <- painel_geral %>%
  left_join(admissoes_trimestre, by = "periodo") %>%
  mutate(theta_caged = vagas_formais / U)


###################### REGRESSÃO ##############################################
modelo_matching <- lm(log(f_t) ~ log(theta_caged), data = painel_geral)
summary(modelo_matching)

ggplot(painel_geral, aes(x = log(theta_caged), y = log(f_t))) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", formula = y ~ x) +
  labs(title = "Função de Matching Estimada com Tightness Nacional (jovens)",
x = "log(θₜ) - Proxy nacional de tightnes )",
       y = "log(fₜ) - Taxa de encontro dos jovens") +
  theme_minimal()



#Regressão por período e grupo (experiencia neste exemplo)
painel_grupos <- microdados_transicao %>%
  filter(estado_ocupacional %in% c(0, 1), !is.na(estado_t1)) %>%
  group_by(periodo, experiencia) %>%
  summarise(
    U = sum(estado_ocupacional == 0),
    T_UE = sum(transicao_emprego == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(f_t = T_UE / U)

painel_grupos <- painel_grupos %>%
  left_join(admissoes_trimestre, by = "periodo") %>%
  mutate(theta_caged = vagas_formais / U)

resultados_por_experiencia <- painel_grupos %>%
  filter(!is.na(f_t), !is.na(theta_caged)) %>%
  group_by(experiencia) %>%
  group_nest() %>%
  mutate(modelo = map(data, ~ lm(log(f_t) ~ log(theta_caged), data = .x)),
         resumo = map(modelo, tidy)) %>%
  unnest(resumo)

resultados_por_experiencia %>%
  filter(term == "log(theta_caged)") %>%
  select(experiencia, estimate, std.error, statistic, p.value)
