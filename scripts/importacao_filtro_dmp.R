################################################################################
# Projeto:Impactos da Pandemia sobre a Ocupação de Jovens no Brasil
# Script: Importação e Filtragem de Microdados da PNAD Contínua (2019–2022)
# Autor: Alexandre Bezerra dos Santos
# Instituição: Universidade Federal de Pernambuco
# Orientador: Cristiano da Costa da Silva
# Curso: Graduação em Ciências Econômicas
# Data: 23/05/2025
# Versão: 1.0
#
# Descrição:
# Este script tem como objetivo importar os microdados da PNAD Contínua 
# Trimestral de 2012 a 2023, e preparar os dados para a aplicação de modelos de
# matching e search.
#
# Uso dos Dados:
# Dados públicos do IBGE, obtidos via PNAD Contínua Trimestral (2019–2022).
# Reproduzido com fins acadêmicos, em conformidade com as diretrizes do IBGE.
#
# Requisitos:
# - R versão >= 4.0
# - Pacotes: haven, dplyr, stringr, lubridate, PNADcIBGE e tidyverse
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
library(PNADcIBGE)
library(tidyverse)

################################################################################
# 2 - Importação e união dos microdados anuais #################################
################################################################################

# Função para importar e empilhar os arquivos .dta gerados pelo
# datazoom_pnadcontinua no Stata
importar_e_empilhar <- function(lista_arquivos) {
  lista_dataframes <- lapply(lista_arquivos, function(arquivo) {
    read_dta(arquivo) %>% 
      select(c("hous_id", "ind_id", "Ano", "Trimestre", "V1022",
               "UF", "V2007", "V2009","V2010","VD4001","VD4002","V3009A"))
  })
  df_final <- bind_rows(lista_dataframes)
  return(df_final)
}


# Importar e junção
setwd("C:/Users/alexa/PNADC/Dados/pnadcontinua")
pnad_completo <- importar_e_empilhar(c("PNADC2012.dta","PNADC2013.dta","PNADC2014.dta"))
pnad_completo2 <- importar_e_empilhar(c("PNADC2015.dta","PNADC2016.dta","PNADC2017.dta"))
pnad_completo3 <- importar_e_empilhar(c("PNADC2018.dta","PNADC2019.dta","PNADC2020.dta"))
pnad_completo4 <- importar_e_empilhar(c("PNADC2021.dta","PNADC2022.dta","PNADC2023.dta"))

pnad_completo <- bind_rows(pnad_completo, pnad_completo2)
#rm(pnad_completo2)
pnad_completo <- bind_rows(pnad_completo, pnad_completo3)
#rm(pnad_completo3)
pnad_completo <- bind_rows(pnad_completo, pnad_completo4)
#rm(pnad_completo4)
microdados <- pnad_completo
#rm(pnad_completo)

#############################################################################################
# 3 - Seleção das variáveis relevantes para a pesquisa ######################################
#############################################################################################
microdados <- microdados %>% 
  mutate(Ano = as.numeric(Ano),
         Trimestre = as.numeric(Trimestre),
         id_pessoa = as.character(ind_id)) %>%
  select(c("hous_id", "id_pessoa", "Ano", "Trimestre", "V1022",
           "UF", "V2007", "V2009","V2010","VD4001","VD4002","V3009A"))

# Criação de variável ocupacional
microdados <- microdados %>%
  mutate(estado_ocupacional = case_when(
    VD4001 == 1 & VD4002 == 1 ~ 1,  # Ocupado
    VD4001 == 1 & VD4002 == 2 ~ 0,  # Desocupado
    TRUE ~ NA_real_
  ))


################################################################################
# 5 - Exportação de dados filtrados ############################################
################################################################################
saveRDS(microdados, 
        "C:/Users/alexa/PNADC/Dados/pnadcontinua/microdados_amplo.RDS")
