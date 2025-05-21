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
│   └── admissoes_caged.csv          # Números mensais de admissões obtidos do CAGED (2019-2022)
📁 scripts/
│   ├── importacao_filtro.R          # Importação das bases anuais geradas pelo PNAD_Social no STATA, filtro de jovens e criação de variáveis de estado
│   ├── matriz_transicao.R           # Construção das Matrizes de transição de Markov
│   └── modelagem_econometrica.R     # Estimação de regressões logísticas e dif-in-dif, construção do painel de matching e estimação da função
📁 resultados/
│   ├── tabelas/                     # Saídas tabulares
│   ├── graficos/                    # Gráficos comparativos
│   └── Relatorio.pdf                # Relatório provisório da pesquisa
README.md
```

---

## 📚 Dados utilizados

* **PNAD Contínua Trimestral** (IBGE): 2019.1 a 2022.4
* **Novo CAGED** (MTE): Admissões formais mensais 2019–2022
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
* A função de matching estimada com admissões do Novo CAGED mostrou **elasticidade positiva, porém não significativa** estatisticamente.
* A eficiência de matching juvenil apresentou sinais de deterioração durante a pandemia.

---

## 📝 Orientação e Incentivo

Esta pesquisa está sendo coordenada pelo Prof. Dr. Cristiano da Costa da Silva, e financiada por Bolsa de Iniciação Científica PROPESQI-UFPE.

---


## 📫 Contato

Alexandre Bezerra
Universidade Federal de Pernambuco – Curso de Ciências Econômicas
Email: [alexandrebezerra@ufpe.br](mailto:alexandrebezerra@ufpe.br)
Lattes: [http://lattes.cnpq.br/1196997304232080](http://lattes.cnpq.br/1196997304232080)
