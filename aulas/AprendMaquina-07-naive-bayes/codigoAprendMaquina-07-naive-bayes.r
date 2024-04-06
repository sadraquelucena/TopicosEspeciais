rm(list=ls()) # remove o que tiver na memória

# Configuração para evitar notação científica
options(scipen = 999)

# ----------------------------------------------------
# Nayve Bayes
# ----------------------------------------------------

# Pacotes
library(tidyverse) # para manipulação dos dados
library(e1071)     # para usar naive bayes

## Importando os dados
# Conjunto de dados sobre emails de spam
email <- read_csv("dados/email.csv")

head(email)

# A variável "message_index" identifica o email
# A variável "message_label" possui a classificação
# "spam" ou "ham"

# *****
# Explorando e preparando os dados

# Converetendo "message_label" em fator
email <- email %>%
  mutate(message_label = as.factor(message_label))

# Use gather() para transformar as colunas dos dados em linhas
email %>%
  gather(word, count,-message_index, -message_label)

# Verificando as 10 palavras que mais aparecem nos dados
email %>%
  gather(word, count,-message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

# Palavras mais frequente entre não spam ("ham")
email %>%
  filter(message_label=='ham') %>%
  gather(word, count,-message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

# Palavras mais frequente entre spam
email %>%
  filter(message_label=='spam') %>%
  gather(word, count,-message_index, -message_label) %>%
  group_by(word) %>%
  summarize(occurrence = sum(count)) %>%
  arrange(desc(occurrence)) %>%
  slice(1:10)

# *****
# Separando os dados em treino (75%) e teste (25%)
set.seed(1234)
indices <- caret::createDataPartition(email$message_label,
                               p = .75, list = F)
email_treino <- email[indices, ]
email_teste <- email[-indices, ]

# Verificando se há desbalanceamento nos dados de treino
janitor::tabyl(email_treino$message_label) %>%
  janitor::adorn_pct_formatting()

# *****
# Ajustando o modelo

# Vamos usar a função naiveBayes() do pacote e1071 package.
# Vamos usar todas as variáveis, exceto "message_index"para predizer "message_label".
# O argumento "laplace" é o valor adicionado pelo estimador de Laplace
email_mod <-
  naiveBayes(message_label ~ . - message_index,
             data = email_treino,
             laplace = 1)


# *****
# Avaliando a performance do modelo

# Estimativas das probabilidades de spam e ham
email_prob <- predict(email_mod, email_teste, type = "raw")

email_pred <- predict(email_mod, email_teste, type = "class")


# Matriz de confusão
caret::confusionMatrix(email_pred, email_teste$message_label, positive = "spam")

# Interpretando a saída
# Acurácia: taxa geral de acerto do modelo
# No Information Rate: acurácia se o modelo
#     prever sempre a classe mais frequente
# P-Value: avalia se o modelo é estatisticamente significante
#     melhor que prever sempre a classe majoritária
# Kappa: medida que leva em consideração a probabilidade
#     de concordância acidental. Quanto mais próximo de 1,
#     melhor.
# Sensitivity: taxa de verdadeiros positivos
# Especificidade: taxa de verdadeiros negativos
