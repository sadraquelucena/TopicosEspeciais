# -------------------------------------------------
# Regressão Logística
# -------------------------------------------------

# Nosso conjunto de dados provém de uma organização
# nacional de veteranos que solicita frequentemente
# doações por meio de campanhas de correspondência
# direta para seu banco de dados de doadores atuais
# e potenciais. A organização enviou um envio de
# teste a um grupo de potenciais doadores e coletou
# informações sobre a resposta a esse envio de teste.

# Variáveis:
#
# age: idade do doador em anos
# numberChildren: número de crianças na casa do doador
# incomeRating: renda anual do doador, em uma
#         escala de 1 a 7 (sendo 7 a mais alta)
# wealthRating: medida semelhante da riqueza total
#         do doador usando uma escala de 1 a 9
# mailOrderPurchases: é o número de compras
#         conhecidas que o doador fez por meio de
#         fontes de pedidos pelo correio
# state: nome do estado dos EUA onde o doador reside
# urbanicity: região onde o doador mora (rural,
#         suburb, town, urban, city)
# socioEconomicStatus: classe socioeconômica do doador
#         (highest, average, lowest)

# -------------------------------------------------
# Importando os dados
# -------------------------------------------------

# Pacotes
library(tidyverse)
library(gtsummary)

# Importando os dados
bd <- read_csv("dados/income.csv")

# -------------------------------------------------
# Explorando e preparando os dados
# -------------------------------------------------

bd %>% tbl_summary()

# Substituindo "?" por "UNK"
bd <- bd %>%
  mutate(workClassification = recode(workClassification, "?" = "UNK")) %>%
  mutate(nativeCountry = recode(nativeCountry, "?" = "UNK")) %>%
  mutate(occupation = recode(occupation, "?" = "UNK"))

bd %>% tbl_summary()

# Transformando a variável resposta
bd <- bd %>%
  mutate(income = dplyr::recode(income, "<=50K" = "0")) %>%
  mutate(income = dplyr::recode(income, ">50K" = "1")) %>%
  mutate(income = as.factor(income))

bd %>% select(income) %>% table()

# Convertendo as variáveis do tipo caractere
# em fator
bd <- bd %>%
  mutate_if(is.character, as_factor)

# Dividindo os dados em treino e teste
set.seed(1234)
amostra <- sample(nrow(bd),
                  round(nrow(bd)*.75),
                  replace = FALSE)
bd_treino <- bd[amostra, ]
bd_teste <- bd[-amostra, ]

# Verificando se as proporções se mantém
bd_treino %>%
  select(income) %>% # seleciona a variável income
  table() %>%  # calcula a frequência das categorias
  prop.table() %>% # obtém as proporções
  round(2) # arredonda para duas casas decimais

# Verificando se as proporções se mantém
bd_teste %>%
  select(income) %>% # seleciona a variável income
  table() %>%  # calcula a frequência das categorias
  prop.table() %>% # obtém as proporções
  round(2) # arredonda para duas casas decimais

# Verificando se as proporções se mantém
bd %>%
  select(income) %>% # seleciona a variável income
  table() %>%  # calcula a frequência das categorias
  prop.table() %>% # obtém as proporções
  round(2) # arredonda para duas casas decimais

# Como há desbalanceamento na variável resposta,
# vamos usar a função "ROSE()" do pacote ROSE
# para balancearmos os dados de treino
set.seed(1234)
bd_treino <- ROSE::ROSE(income ~ ., data = bd_treino)$data

bd_treino %>%
  select(income) %>% # seleciona a variável income
  table() %>%  # calcula a frequência das categorias
  prop.table() %>% # obtém as proporções
  round(2) # arredonda para duas casas decimais

# -------------------------------------------------
# Treinando o modelo
# -------------------------------------------------

mod1 <- glm(income ~ ., data = bd_treino,
            family = binomial)
summary(mod1)

# R2 do modelo
round( 1 - ( mod1$deviance / mod1$null.deviance ), 2 )


# -------------------------------------------------
# Avaliando e melhorando o modelo
# -------------------------------------------------


