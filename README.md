# 📊 Pesquisa: Juventude, Desemprego e Pandemia no Brasil (2019–2022)

Este repositório contém os códigos, dados tratados e resultados da pesquisa empírica que investiga os **impactos da pandemia de COVID-19 sobre a inserção ocupacional de jovens no mercado de trabalho brasileiro**, utilizando microdados da PNAD Contínua Trimestral (IBGE) e dados administrativos do Novo CAGED (Ministério do Trabalho).

---

## 🎯 Objetivo

Aplicar técnicas de análise de transição ocupacional, modelos logísticos com interações, e estimação da função de matching (modelo DMP) para:

* Medir as probabilidades de entrada e saída do emprego para jovens (14–29 anos) nos períodos pré, durante e pós-pandemia.
* Analisar os efeitos diferenciados por sexo, raça, escolaridade, experiência e localização (urbana/rural).
* Estimar a função de matching e a eficiência do emparelhamento no mercado de trabalho juvenil.

---

## 🧾 Estrutura do Projeto

```bash
📁 dados/
│   ├── microdados_filtrados.RDS     # Dados da PNAD Contínua filtrados para Jovens de 14-29 anos, e para as variáveis relevantes
│   ├── painel_trimestral_jovem.RDS  # Dados trimestrais de mercado de trabalho para os jovens
│   └── admissoes_caged.csv          # Números mensais de admissões obtidos do CAGED (2019-2023)
📁 scripts/
│   ├── importacao_filtro_matriztransicao.R          # Importação das bases anuais geradas pelo datazoom_pnadcontinua no STATA, para os anos de 2019 a 2022, filtro de jovens e criação de variáveis de estado
│   ├── matriz_transicao.R                           # Construção das Matrizes de transição de Markov para os jovens
|   ├── importacao_filtro_dmp                        # Importação dos dados anuais de 2012-2023, sem filtro de idade, e criação de variável de estado
|   ├── estimacao_dmp                                # Estimação da tightness para a população em geral, para os jovens, estimação da eficiência relativa de matching para os jovens
│   └── modelagem_econometrica.R                     # Estimação de regressões logit e dif-in-dif para as chances de emprego dos jovens controladas por suas heterogeneidades
📁 resultados/
│   ├── Matrizes de Transição.csv                    # Planilha com as Matrizes de Transição
│   ├── Sumários.csv                                 # Planilha com os sumários das regressões logit e dif-in-dif
│   ├── graficos                                     # Gráficos comparativos
│   ├── Tabela_Mercado_de_Trabalho_juvenil.csv       # Planilha com os dados do mercado de trabalho juvenil
│   └── Relatorio.pdf                                # Relatório provisório da pesquisa
README.md
```

---

## 📚 Dados utilizados

* **PNAD Contínua Trimestral** (IBGE): 2012.1 a 2023.4
* **CAGED** (MTE): Admissões formais mensais 2012–2023
* **IPEADATA**: Séries auxiliares de ocupação total

---

## 🛠️ Requisitos

Projeto desenvolvido em **R** a partir de dados obtidos pelo módulo PNAD_Social do Data Zoom (PUC-Rio). As principais bibliotecas utilizadas incluem:

```r
tidyverse
readr
lubridate
broom
ggplot2
ipeadatar
```

Para instalar todas elas:

```r
install.packages(c("tidyverse", "readr", "lubridate", "broom", "ggplot2", "ipeadatar"))
```

---

## 🧠 Principais Resultados

* Matrizes de transição de emprego mostram aumento da persistência no desemprego durante a pandemia.
* Mulheres e negros apresentaram menor probabilidade de transição para o emprego.
* O modelo logit com interações revelou que o efeito da pandemia foi **heterogêneo**, especialmente entre os menos escolarizados.
* A função de matching estimada com admissões do Novo CAGED mostrou **elasticidade positiva, e significativa** estatisticamente.
* A eficiência de matching juvenil apresentou sinais de deterioração durante a pandemia.

---

## 📝 Orientação e Incentivo

Esta pesquisa está sendo coordenada pelo Prof. Dr. Cristiano da Costa da Silva, e financiada por Bolsa de Iniciação Científica PROPESQI-UFPE.

---


## 📫 Contato

Alexandre Bezerra dos Santos.
Universidade Federal de Pernambuco – Graduando em Ciências Econômicas.
Email: [alexandre.bezerras@ufpe.br](mailto:alexandre.bezerras@ufpe.br).
Lattes: [http://lattes.cnpq.br/1196997304232080](http://lattes.cnpq.br/1196997304232080).
