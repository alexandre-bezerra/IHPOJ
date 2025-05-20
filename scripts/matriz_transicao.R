################################################################################
# Projeto:Impactos Heterogêneos da Pandemia sobre a Ocupação de Jovens no Brasil
# Script: Estimação de Matriz de Transição com Microdados da PNAD Contínua 
# (2019–2022)
# Autor: Alexandre Bezerra dos Santos
# Instituição: Universidade Federal de Pernambuco
# Orientador: Cristiano da Costa da Silva
# Curso: Graduação em Ciências Econômicas
# Data: 17/05/2025
# Versão: 1.0
#
# Descrição:
# Este script tem como objetivo a construção de matrizes de transição
# ocupacional e aplicação de modelos de matching e search, com foco em 
# heterogeneidades por raça, escolaridade, experiência e região.
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


################################################################################
# 2 - Importação dos dados filtrados ###########################################
################################################################################

microdados <- readRDS("microdados_filtrados_variáveis.RDS")


################################################################################
# 3 - Ordenação dos dados, criação das variáveis de estado #####################
#     e identificação dos períodos de análise              #####################
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

# Seleção apenas das transições consecutivas (diferença de 1 trimestre)
microdados_transicao <- microdados %>%
  filter(!is.na(estado_ocupacional), !is.na(estado_t1)) %>%
  mutate(distancia_temporal = (Ano_t1 - Ano) * 4 +
           (Trimestre_t1 - Trimestre)) %>%
  filter(distancia_temporal == 1)

# Separação dos períodos de análise
microdados_transicao <- microdados_transicao %>%
  mutate(periodo = case_when(Ano < 2020 ~ "Pré-pandemia", 
                             Ano >= 2020 & Ano <= 2021 ~ "Durante pandemia",
    Ano > 2021 ~ "Pós-pandemia"))


################################################################################
# 4 - Geração de Matriz de Transição por Período ###############################
################################################################################

# Função para calcular matriz de transição por período
matriz_por_periodo <- function(df, nome_periodo) {
  tab <- table(df$estado_ocupacional, df$estado_t1)
  mat <- prop.table(tab, margin = 1)
  cat("###", nome_periodo, "\n")
  print(round(mat, 3))
  return(as.data.frame.matrix(mat))
}

# Geração de Matrizes de Transição para os Jovens nos três períodos

matriz_pre <- matriz_por_periodo(filter(microdados_transicao, 
                                        periodo == "Pré-pandemia"), 
                                 "Pré-pandemia")
matriz_dur <- matriz_por_periodo(filter(microdados_transicao,
                                        periodo == "Durante pandemia"),
                                 "Durante pandemia")
matriz_pos <- matriz_por_periodo(filter(microdados_transicao,
                                        periodo == "Pós-pandemia"),
                                 "Pós-pandemia")

# Geração de Matrizes de Transição em cada período por Sub-grupo

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
    # Agrupar localidade (Urbana ou Rural)
    localidade = case_when(
      V1022 == 1 ~ "Urbana",
      V1022 == 2 ~ "Rural",
      TRUE ~ NA_character_
    )
  )


# Função para gerar matrizes por sub-grupo
matriz_por_grupo <- function(df, grupo_nome, grupo_var) {
  grupos <- unique(na.omit(df[[grupo_var]]))
  lista_resultados <- list()
  
  for (g in grupos) {
    df_filtrado <- df %>% filter(get(grupo_var) == g)
    tab <- table(df_filtrado$estado_ocupacional, df_filtrado$estado_t1)
    mat <- prop.table(tab, margin = 1)
    cat("\n###", grupo_nome, ":", g, "\n")
    print(round(mat, 3))
    
    # Guardar para gráfico
    df_mat <- as.data.frame.matrix(mat)
    df_mat$grupo <- g
    lista_resultados[[g]] <- df_mat
  }
  
  return(bind_rows(lista_resultados, .id = "grupo"))
}

# Matrizes para o período Pré-pandemia

# Sexo
matriz_pre_mulher <- matriz_por_periodo(filter(microdados_transicao,
                                               periodo == "Pré-pandemia" &
                                                 sexo == "Mulher"),
                                        "Pré-pandemia: Mulher")

matriz_pre_homem <- matriz_por_periodo(filter(microdados_transicao,
                                              periodo == "Pré-pandemia" &
                                                sexo == "Homem"),
                                       "Pré-pandemia: Homem")
# Raça
matriz_pre_branca <- matriz_por_periodo(filter(microdados_transicao,
                                               periodo == "Pré-pandemia" &
                                                 raca == "Branca"),
                                        "Pré-pandemia: Branca")

matriz_pre_preta <- matriz_por_periodo(filter(microdados_transicao,
                                              periodo == "Pré-pandemia" &
                                                raca == "Preta"),
                                       "Pré-pandemia: Preta")

matriz_pre_parda <- matriz_por_periodo(filter(microdados_transicao,
                                              periodo == "Pré-pandemia" &
                                                raca == "Parda"),
                                       "Pré-pandemia: Parda")

matriz_pre_outros <- matriz_por_periodo(filter(microdados_transicao,
                                               periodo == "Pré-pandemia" &
                                                 raca == "Outros"),
                                        "Pré-pandemia: Outros")

# Escolaridade
matriz_pre_fundamental <- matriz_por_periodo(filter(microdados_transicao,
  periodo == "Pré-pandemia" & escolaridade == "Fundamental ou menos"),
                                      "Pré-pandemia: Fundamental ou menos")

matriz_pre_medio <- matriz_por_periodo(filter(microdados_transicao,
                                              periodo == "Pré-pandemia" &
                                                escolaridade == "Médio"),
                                       "Pré-pandemia: Médio")

matriz_pre_superior <- matriz_por_periodo(filter(microdados_transicao,
                                                 periodo == "Pré-pandemia" &
                                                   escolaridade == "Superior"),
                                          "Pré-pandemia: Superior")

# Experiência
matriz_pre_adolescente <- matriz_por_periodo(filter(microdados_transicao,
                                                    periodo == "Pré-pandemia" &
                                        experiencia == "Adolescente (14-17)"),
                                        "Pré-pandemia: Adolescente (14-17)")

matriz_pre_jovem <- matriz_por_periodo(filter(microdados_transicao,
                                              periodo == "Pré-pandemia" &
                                            experiencia == "Jovem (18-24)"),
                                       "Pré-pandemia: Jovem (18-24)")

matriz_pre_jovemadulto <- matriz_por_periodo(filter(microdados_transicao,
                                                    periodo == "Pré-pandemia" &
                                        experiencia == "Jovem adulto (25-29)"),
                                         "Pré-pandemia: Jovem adulto (25-29)")

# Localidade
matriz_pre_Urbana <- matriz_por_periodo(filter(microdados_transicao,
                                               periodo == "Pré-pandemia" &
                                                 localidade == "Urbana"),
                                        "Pré-pandemia: Urbana")

matriz_pre_Rural<- matriz_por_periodo(filter(microdados_transicao,
                                             periodo == "Pré-pandemia" &
                                               localidade == "Rural"),
                                      "Pré-pandemia: Rural")


# Matrizes para o período durante a pandemia

# Sexo
matriz_durante_mulher <- matriz_por_periodo(filter(microdados_transicao,
                            periodo == "Durante pandemia" & sexo == "Mulher"),
                                            "Durante pandemia: Mulher")

matriz_durante_homem <- matriz_por_periodo(filter(microdados_transicao,
                             periodo == "Durante pandemia" & sexo == "Homem"),
                                           "Durante pandemia: Homem")
# Raça
matriz_durante_branca <- matriz_por_periodo(filter(microdados_transicao,
                             periodo == "Durante pandemia" & raca == "Branca"),
                                            "Durante pandemia: Branca")

matriz_durante_preta <- matriz_por_periodo(filter(microdados_transicao,
                             periodo == "Durante pandemia" & raca == "Preta"),
                                           "Durante pandemia: Preta")

matriz_durante_parda <- matriz_por_periodo(filter(microdados_transicao,
                             periodo == "Durante pandemia" & raca == "Parda"),
                                           "Durante pandemia: Parda")

matriz_durante_outros <- matriz_por_periodo(filter(microdados_transicao,
                             periodo == "Durante pandemia" & raca == "Outros"),
                                            "Durante pandemia: Outros")

# Escolaridade
matriz_durante_fundamental <- matriz_por_periodo(filter(microdados_transicao,
       periodo == "Durante pandemia" & escolaridade == "Fundamental ou menos"),
                                      "Durante pandemia: Fundamental ou menos")

matriz_durante_medio <- matriz_por_periodo(filter(microdados_transicao,
                      periodo == "Durante pandemia" & escolaridade == "Médio"),
                                           "Durante pandemia: Médio")

matriz_durante_superior <- matriz_por_periodo(filter(microdados_transicao,
                  periodo == "Durante pandemia" & escolaridade == "Superior"),
                                              "Durante pandemia: Superior")

# Experiência
matriz_durante_adolescente <- matriz_por_periodo(filter(microdados_transicao,
        periodo == "Durante pandemia" & experiencia == "Adolescente (14-17)"),
                                  "Durante pandemia: Adolescente (14-17)")

matriz_durante_jovem <- matriz_por_periodo(filter(microdados_transicao,
              periodo == "Durante pandemia" & experiencia == "Jovem (18-24)"),
                                           "Durante pandemia: Jovem (18-24)")

matriz_durante_jovemadulto <- matriz_por_periodo(filter(microdados_transicao,
      periodo == "Durante pandemia" & experiencia == "Jovem adulto (25-29)"),
                                    "Durante pandemia: Jovem adulto (25-29)")

# Localidade
matriz_durante_Urbana <- matriz_por_periodo(filter(microdados_transicao,
                      periodo == "Durante pandemia" & localidade == "Urbana"),
                                            "Durante pandemia: Urbana")

matriz_durante_Rural<- matriz_por_periodo(filter(microdados_transicao,
                      periodo == "Durante pandemia" & localidade == "Rural"),
                                          "Durante pandemia: Rural")

# Matrizes para o período pós-pandemia

# Sexo
matriz_pos_mulher <- matriz_por_periodo(filter(microdados_transicao,
                                  periodo == "Pós-pandemia" & sexo == "Mulher"),
                                        "Pós-pandemia: Mulher")

matriz_pos_homem <- matriz_por_periodo(filter(microdados_transicao,
                                   periodo == "Pós-pandemia" & sexo == "Homem"),
                                       "Pós-pandemia: Homem")
# Raça
matriz_pos_branca <- matriz_por_periodo(filter(microdados_transicao,
                                 periodo == "Pós-pandemia" & raca == "Branca"),
                                        "Pós-pandemia: Branca")

matriz_pos_preta <- matriz_por_periodo(filter(microdados_transicao,
                                  periodo == "Pós-pandemia" & raca == "Preta"),
                                       "Pós-pandemia: Preta")

matriz_pos_parda <- matriz_por_periodo(filter(microdados_transicao,
                                  periodo == "Pós-pandemia" & raca == "Parda"),
                                       "Pós-pandemia: Parda")

matriz_pos_outros <- matriz_por_periodo(filter(microdados_transicao,
                                  periodo == "Pós-pandemia" & raca == "Outros"),
                                        "Pós-pandemia: Outros")

# Escolaridade
matriz_pos_fundamental <- matriz_por_periodo(filter(microdados_transicao,
            periodo == "Pós-pandemia" & escolaridade == "Fundamental ou menos"),
                                          "Pós-pandemia: Fundamental ou menos")

matriz_pos_medio <- matriz_por_periodo(filter(microdados_transicao,
                           periodo == "Pós-pandemia" & escolaridade == "Médio"),
                                       "Pós-pandemia: Médio")

matriz_pos_superior <- matriz_por_periodo(filter(microdados_transicao,
                        periodo == "Pós-pandemia" & escolaridade == "Superior"),
                                          "Pós-pandemia: Superior")

# Experiência
matriz_pos_adolescente <- matriz_por_periodo(filter(microdados_transicao,
             periodo == "Pós-pandemia" & experiencia == "Adolescente (14-17)"),
                                           "Pós-pandemia: Adolescente (14-17)")

matriz_pos_jovem <- matriz_por_periodo(filter(microdados_transicao,
                   periodo == "Pós-pandemia" & experiencia == "Jovem (18-24)"),
                                       "Pós-pandemia: Jovem (18-24)")

matriz_pos_jovemadulto <- matriz_por_periodo(filter(microdados_transicao,
             periodo == "Pós-pandemia" & experiencia == "Jovem adulto (25-29)"),
                                          "Pós-pandemia: Jovem adulto (25-29)")

# Localidade
matriz_pos_Urbana <- matriz_por_periodo(filter(microdados_transicao,
                            periodo == "Pós-pandemia" & localidade == "Urbana"),
                                        "Pós-pandemia: Urbana")

matriz_pos_Rural<- matriz_por_periodo(filter(microdados_transicao,
                            periodo == "Pós-pandemia" & localidade == "Rural"),
                                      "Pós-pandemia: Rural")


# Junção em um único dataframe
matrizes_sub <- data.frame(`Período` = c("Pré-pandemia", "Pré-pandemia",
"Pré-pandemia", "Pré-pandemia", "Pré-pandemia", "Pré-pandemia", "Pré-pandemia",
"Pré-pandemia", "Pré-pandemia", "Pré-pandemia", "Pré-pandemia", "Pré-pandemia",
"Pré-pandemia", "Pré-pandemia", "Durante pandemia", "Durante pandemia",
"Durante pandemia", "Durante pandemia", "Durante pandemia", "Durante pandemia",
"Durante pandemia", "Durante pandemia", "Durante pandemia", "Durante pandemia",
"Durante pandemia", "Durante pandemia", "Durante pandemia", "Durante pandemia",
"Pós-pandemia", "Pós-pandemia", "Pós-pandemia", "Pós-pandemia", "Pós-pandemia",
"Pós-pandemia", "Pós-pandemia", "Pós-pandemia", "Pós-pandemia", "Pós-pandemia",
"Pós-pandemia", "Pós-pandemia", "Pós-pandemia", "Pós-pandemia"),
`Condição`=c("Mulher", "Homem", "Branca", "Preta", "Parda", "Outros",
"Fundamental ou menos", "Médio", "Superior", "Adolescente (14-17)",
"Jovem (18-24)", "Jovem adulto (25-29)", "Urbana", "Rural", "Mulher", "Homem",
"Branca", "Preta", "Parda", "Outros", "Fundamental ou menos", "Médio",
"Superior", "Adolescente (14-17)", "Jovem (18-24)", "Jovem adulto (25-29)",
"Urbana", "Rural", "Mulher", "Homem", "Branca", "Preta", "Parda", "Outros",
"Fundamental ou menos", "Médio", "Superior", "Adolescente (14-17)", 
"Jovem (18-24)", "Jovem adulto (25-29)", "Urbana", "Rural"),
`Continuar desempregado` = c(matriz_pre_mulher[1,1],
matriz_pre_homem[1,1], matriz_pre_branca[1,1], matriz_pre_preta[1,1],
matriz_pre_parda[1,1], matriz_pre_outros[1,1], matriz_pre_fundamental[1,1],
matriz_pre_medio[1,1], matriz_pre_superior[1,1], matriz_pre_adolescente[1,1],
matriz_pre_jovem[1,1], matriz_pre_jovemadulto[1,1], matriz_pre_Urbana[1,1],
matriz_pre_Rural[1,1], matriz_durante_mulher[1,1], matriz_durante_homem[1,1],
matriz_durante_branca[1,1], matriz_durante_preta[1,1], 
matriz_durante_parda[1,1], matriz_durante_outros[1,1], 
matriz_durante_fundamental[1,1], matriz_durante_medio[1,1], 
matriz_durante_superior[1,1], matriz_durante_adolescente[1,1], 
matriz_durante_jovem[1,1], matriz_durante_jovemadulto[1,1], 
matriz_durante_Urbana[1,1], matriz_durante_Rural[1,1], matriz_pos_mulher[1,1],
matriz_pos_homem[1,1], matriz_pos_branca[1,1], matriz_pos_preta[1,1], 
matriz_pos_parda[1,1], matriz_pos_outros[1,1], matriz_pos_fundamental[1,1], 
matriz_pos_medio[1,1], matriz_pos_superior[1,1], matriz_pos_adolescente[1,1], 
matriz_pos_jovem[1,1], matriz_pos_jovemadulto[1,1], matriz_pos_Urbana[1,1], 
matriz_pos_Rural[1,1]), `Conseguir emprego` = c(matriz_pre_mulher[1,2], 
matriz_pre_homem[1,2], matriz_pre_branca[1,2], matriz_pre_preta[1,2], 
matriz_pre_parda[1,2], matriz_pre_outros[1,2], matriz_pre_fundamental[1,2], 
matriz_pre_medio[1,2], matriz_pre_superior[1,2], matriz_pre_adolescente[1,2], 
matriz_pre_jovem[1,2], matriz_pre_jovemadulto[1,2], matriz_pre_Urbana[1,2], 
matriz_pre_Rural[1,2], matriz_durante_mulher[1,2], matriz_durante_homem[1,2], 
matriz_durante_branca[1,2], matriz_durante_preta[1,2], 
matriz_durante_parda[1,2], matriz_durante_outros[1,2],
matriz_durante_fundamental[1,2], matriz_durante_medio[1,2],
matriz_durante_superior[1,2], matriz_durante_adolescente[1,2],
matriz_durante_jovem[1,2], matriz_durante_jovemadulto[1,2],
matriz_durante_Urbana[1,2], matriz_durante_Rural[1,2], matriz_pos_mulher[1,2],
matriz_pos_homem[1,2], matriz_pos_branca[1,2], matriz_pos_preta[1,2],
matriz_pos_parda[1,2], matriz_pos_outros[1,2], matriz_pos_fundamental[1,2],
matriz_pos_medio[1,2], matriz_pos_superior[1,2], matriz_pos_adolescente[1,2],
matriz_pos_jovem[1,2], matriz_pos_jovemadulto[1,2], matriz_pos_Urbana[1,2],
matriz_pos_Rural[1,2]), `Perder emprego` = c(matriz_pre_mulher[2,1],
matriz_pre_homem[2,1], matriz_pre_branca[2,1], matriz_pre_preta[2,1],
matriz_pre_parda[2,1], matriz_pre_outros[2,1], matriz_pre_fundamental[2,1],
matriz_pre_medio[2,1], matriz_pre_superior[2,1], matriz_pre_adolescente[2,1],
matriz_pre_jovem[2,1], matriz_pre_jovemadulto[2,1], matriz_pre_Urbana[2,1],
matriz_pre_Rural[2,1], matriz_durante_mulher[2,1], matriz_durante_homem[2,1],
matriz_durante_branca[2,1], matriz_durante_preta[2,1],
matriz_durante_parda[2,1], matriz_durante_outros[2,1], 
matriz_durante_fundamental[2,1], matriz_durante_medio[2,1],
matriz_durante_superior[2,1],matriz_durante_adolescente[2,1],
matriz_durante_jovem[2,1], matriz_durante_jovemadulto[2,1],
matriz_durante_Urbana[2,1], matriz_durante_Rural[2,1], matriz_pos_mulher[2,1],
matriz_pos_homem[2,1], matriz_pos_branca[2,1], matriz_pos_preta[2,1],
matriz_pos_parda[2,1], matriz_pos_outros[2,1], matriz_pos_fundamental[2,1], 
matriz_pos_medio[2,1], matriz_pos_superior[2,1], matriz_pos_adolescente[2,1],
matriz_pos_jovem[2,1], matriz_pos_jovemadulto[2,1], matriz_pos_Urbana[2,1],
matriz_pos_Rural[2,1]), `Permanecer empregado` = c(matriz_pre_mulher[2,2],
matriz_pre_homem[2,2], matriz_pre_branca[2,2], matriz_pre_preta[2,2], 
matriz_pre_parda[2,2], matriz_pre_outros[2,2], matriz_pre_fundamental[2,2], 
matriz_pre_medio[2,2], matriz_pre_superior[2,2], matriz_pre_adolescente[2,2],
matriz_pre_jovem[2,2], matriz_pre_jovemadulto[2,2], matriz_pre_Urbana[2,2],  
matriz_pre_Rural[2,2], matriz_durante_mulher[2,2], matriz_durante_homem[2,2],
matriz_durante_branca[2,2], matriz_durante_preta[2,2],
matriz_durante_parda[2,2], matriz_durante_outros[2,2],
matriz_durante_fundamental[2,2], matriz_durante_medio[2,2],
matriz_durante_superior[2,2], matriz_durante_adolescente[2,2],
matriz_durante_jovem[2,2], matriz_durante_jovemadulto[2,2],
matriz_durante_Urbana[2,2], matriz_durante_Rural[2,2], matriz_pos_mulher[2,2],
matriz_pos_homem[2,2], matriz_pos_branca[2,2], matriz_pos_preta[2,2],
matriz_pos_parda[2,2], matriz_pos_outros[2,2], matriz_pos_fundamental[2,2],
matriz_pos_medio[2,2], matriz_pos_superior[2,2], matriz_pos_adolescente[2,2],
matriz_pos_jovem[2,2], matriz_pos_jovemadulto[2,2], matriz_pos_Urbana[2,2],
matriz_pos_Rural[2,2]))


################################################################################
# 5 - Geração de Gráficos ######################################################
################################################################################

# Juntar as três matrizes em formato longo
matriz_pre$de <- rownames(matriz_pre)
matriz_dur$de <- rownames(matriz_dur)
matriz_pos$de <- rownames(matriz_pos)

matrizes_long <- bind_rows(
  matriz_pre %>% mutate(periodo = "Pré-pandemia"),
  matriz_dur %>% mutate(periodo = "Durante pandemia"),
  matriz_pos %>% mutate(periodo = "Pós-pandemia")
) %>%
  pivot_longer(cols = c("0", "1"), names_to = "para", 
               values_to = "probabilidade")

# Conversão para fator ordenado
matrizes_long$periodo <- factor(matrizes_long$periodo, 
levels = c("Pré-pandemia", "Durante pandemia", "Pós-pandemia"))

# Gráfico Global
ggplot(matrizes_long, aes(x = interaction(de, para), y = probabilidade, 
fill = periodo)) + geom_bar(stat = "identity", position = "dodge") +
labs(title = "Probabilidades de Transição Ocupacional por Período", 
x = "Transição (de → para)", y = "Probabilidade condicional", fill = "Período")
+ scale_x_discrete(labels = c("0.0" = "Desemp → Desemp", 
"0.1" = "Desemp → Empreg", "1.0" = "Empreg → Desemp", 
"1.1" = "Empreg → Empreg")) + theme_minimal()

# Gráficos com heterogeneidades
df_long <- matrizes_sub %>%
  pivot_longer(cols = 3:6, names_to = "Transição", values_to = "Probabilidade")

df_long$Período <- factor(df_long$Período,
 levels = c("Pré-pandemia", "Durante pandemia", "Pós-pandemia"))

df_long$Condição <- factor(df_long$Condição, levels = c("Adolescente (14-17)",
       "Jovem (18-24)", "Jovem adulto (25-29)", "Homem", "Mulher",
       "Fundamental ou menos", "Médio", "Superior", "Branca", "Preta",
       "Parda", "Outros", "Urbana", "Rural"))

ggplot(df_long, aes(x = Condição, y = Probabilidade, fill = Transição)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Período) +
  labs(title = "Probabilidades de Transição por Grupo e Período",
       x = "Grupo", y = "Probabilidade (%)", fill = "Tipo de Transição") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Evolução da Inserção do Mercado de Trabalho
trans_conseguir <- df_long %>%
  filter(Transição == "Conseguir.emprego")

ggplot(trans_conseguir, aes(x = Período, y = Probabilidade, color = Condição, 
                            group = Condição)) + geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Probabilidade de Conseguir Emprego por Período",
       x = "Período", y = "Probabilidade (%)", color = "Grupo") +
  theme_minimal()

# Evolução das Demissões do Mercado de Trabalho
trans_perder <- df_long %>%
  filter(Transição == "Perder.emprego")

ggplot(trans_perder, aes(x = Período, y = Probabilidade, color = Condição, 
                         group = Condição)) +
  geom_line(size = 1.2) + geom_point(size = 2) +
  labs(title = "Probabilidade de Perder Emprego por Período",
       x = "Período", y = "Probabilidade (%)", color = "Grupo") +
  theme_minimal()

# Mapa de probabilidades associadas as mulheres
# No Pré-pandemia
valores <- c("Continuar desempregada" = matrizes_sub[1,3]*100,
             "Conseguir emprego" = matrizes_sub[1,4]*100,
             "Perder emprego" = matrizes_sub[1,5]*100,
             "Permanecer empregada" = matrizes_sub[1,6]*100)

radar_data <- as.data.frame(rbind(
  max = rep(100, 4),    # máximos
  min = rep(0, 4),      # mínimos
  valores               # seus dados reais
))

# Gráfico
radarchart(radar_data,
           axistype = 1,
           pcol = "darkblue",
           pfcol = rgb(0.1, 0.2, 0.5, 0.4),
           plwd = 3,
           cglcol = "grey", cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 100, 25),
           vlcex = 0.9,
           title = "Padrão de Transição - Mulher (Pré-pandemia)")

# Durante pandemia
valores <- c("Continuar desempregada" = matrizes_sub[15,3]*100,
             "Conseguir emprego" = matrizes_sub[15,4]*100,
             "Perder emprego" = matrizes_sub[15,5]*100,
             "Permanecer empregada" = matrizes_sub[15,6]*100)

radar_data <- as.data.frame(rbind(
  max = rep(100, 4),   
  min = rep(0, 4),     
  valores              
))

# Gráfico
radarchart(radar_data,
           axistype = 1,
           pcol = "darkblue",
           pfcol = rgb(0.1, 0.2, 0.5, 0.4),
           plwd = 3,
           cglcol = "grey", cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 100, 25),
           vlcex = 0.9, 
           title = "Padrão de Transição - Mulher (Durante pandemia)")

# Pós-pandemia
valores <- c("Continuar desempregada" = matrizes_sub[29,3]*100,
             "Conseguir emprego" = matrizes_sub[29,4]*100,
             "Perder emprego" = matrizes_sub[29,5]*100,
             "Permanecer empregada" = matrizes_sub[29,6]*100)

radar_data <- as.data.frame(rbind(
  max = rep(100, 4),    
  min = rep(0, 4),      
  valores               
))

# Gráfico
radarchart(radar_data,
           axistype = 1,
           pcol = "darkblue",
           pfcol = rgb(0.1, 0.2, 0.5, 0.4),
           plwd = 3,
           cglcol = "grey", cglty = 1,
           axislabcol = "grey",
           caxislabels = seq(0, 100, 25),
           vlcex = 0.9, 
           title = "Padrão de Transição - Mulher (Pós-pandemia)")

################################################################################
# 6 - Exportando Dados de Matriz de Transição ##################################
################################################################################
saveRDS(matrizes_sub, "matrizes_transicao.RDS")
