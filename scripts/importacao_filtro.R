################################################################################
# Projeto: Impactos da Pandemia sobre a Ocupação de Jovens no Brasil
# Script: Importação e Filtragem de Microdados da PNAD Contínua (2019–2022)
# Autor: Alexandre Bezerra dos Santos
# Instituição: Universidade Federal de Pernambuco
# Orientador: Cristiano da Costa da Silva
# Curso: Graduação em Ciências Econômicas
# Data: 17/05/2025
# Versão: 1.0
#
# Descrição:
# Este script tem como objetivo importar os microdados da PNAD Contínua 
# Trimestral de 2019 a 2022, filtrar os indivíduos jovens (14 a 29 anos) e 
# preparar os dados para a construção de matrizes de transição ocupacional e
# aplicação de modelos de matching e search, com foco em heterogeneidades por 
# raça, escolaridade, experiência e região.
#
# Uso dos Dados:
# Dados públicos do IBGE, obtidos via PNAD Contínua Trimestral (2019–2022).
# Reproduzido com fins acadêmicos, em conformidade com as diretrizes do IBGE.
#
# Requisitos:
# - R versão >= 4.0
# - Pacotes: haven, dplyr, stringr, lubridate
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


################################################################################
# 2 - Importação e união dos microdados anuais #################################
################################################################################

# Função para importar e empilhar os arquivos .dta gerados pelo
# PNAD_SOCIAL no Stata
importar_e_empilhar <- function(lista_arquivos) {
  lista_dataframes <- lapply(lista_arquivos, function(arquivo) {
    read_dta(arquivo)
  })
  df_final <- bind_rows(lista_dataframes)
  return(df_final)
}

# Lista de arquivos
arquivos <- c("../dados/PNADC2019.dta", "../dados/PNADC2020.dta",
              "../dados/PNADC2021.dta", "../dados/PNADC2022.dta")

# Importar e junção
pnad_completo <- importar_e_empilhar(arquivos)


################################################################################
# 3 - Filtragem do público-alvo (14-29 anos) - V2009 ###########################
################################################################################

# Criando um novo dataframe com o filtro
microdados <- pnad_completo %>%
  filter(V2009 >= 14, V2009 <= 29)  # variável de idade

rm(pnad_completo) # apagando dataframe antigo


################################################################################
# 4 - Seleção das variáveis relevantes para a pesquisa##########################
################################################################################

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

saveRDS(microdados, "../dados/microdados_filtrados.RDS")
