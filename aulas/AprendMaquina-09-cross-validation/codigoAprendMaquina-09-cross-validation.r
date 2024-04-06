rm(list=ls()) # remove o que tiver na memória

# ----------------------------------------------------
# Cross-Validation
# ----------------------------------------------------

# Pacotes
library(tidyverse)
library(caret) # para cross-validatoin
library(rpart) # para algoritmo CART

## Importando os dados
# Conjunto de dados sobre doença cardíaca
dados <- read_csv("dados/heart.csv", col_types = "nffnnffnfnfnff")
glimpse(dados)

# Esses dados já foram explorados anteriormente

# *****
# Organizando os dados
dados <- dados %>%
  janitor::clean_names() %>% # Limpa e padroniza os nomes das colunas
  janitor::remove_empty(which = c("rows", "cols")) # remove todas as linhas e colunas completamente vazias

DataExplorer::plot_missing(dados) # % de NAs de cada variável

# *****
# Separando os dados em treino (80%) e teste (20%)
# A variável resposta é heart_disease
set.seed(1234)
indices <- caret::createDataPartition(dados$heart_disease,
                                      p = .80, list = F)
dados_treino <- dados[indices, ]
dados_teste <- dados[-indices, ]

# Verificando se há desbalanceamento nos dados de treino
janitor::tabyl(dados_treino$heart_disease) %>%
  janitor::adorn_pct_formatting()

# Balanceando os dados
dados_treino_balanceado <- ROSE::ovun.sample(formula = heart_disease ~ .,
                                             data = dados_treino,
                                             method = "over")$data

# Verificando se os dados estão balanceados
janitor::tabyl(dados_treino_balanceado$heart_disease) %>%
  janitor::adorn_pct_formatting()

# *****
# k-fold cross-validation

# Podemos usar a função 'train()' do pacote 'caret'
mod_cv <- train(
  heart_disease ~ ., # indicando a variável resposta
  data = dados_treino_balanceado, # dados de treino
  metric = "Accuracy", # métrica de avaliação
  method = "rpart",  # árvore de decisão CART
  trControl = trainControl(method = "cv", number = 5) # cross-validation com 5 subconjuntos (folds)
  )

# Ver a acurácia em cada subconjunto (fold)
mod_cv$resample %>%
  arrange(Resample)

# Acurácia geral
mod_cv$resample %>%
  arrange(Resample) %>%
  summarise("Acurácia média" = mean(Accuracy))


# *****
# leave-one-out cross-validation
# basta usarmos o argumento "trControl = trainControl(method = "LOOCV")"
mod_l1o <- train(
  heart_disease ~ ., # indicando a variável resposta
  data = dados_treino_balanceado, # dados de treino
  metric = "Accuracy", # métrica de avaliação
  method = "rpart",  # árvore de decisão CART
  trControl = trainControl(method = "LOOCV") # leave-one-out
)

# Acurácia
mod_l1o$results$Accuracy


# *****
# Random cross-validation
# basta usarmos o argumento "trControl = trainControl(method = "LOOCV")"
mod_random <- train(
  heart_disease ~ ., # indicando a variável resposta
  data = dados_treino_balanceado, # dados de treino
  metric = "Accuracy", # métrica de avaliação
  method = "rpart",  # árvore de decisão CART
  trControl = trainControl(method = "LGOCV", # randon cross-validation
                           p = .1, # use 10% dos dados para validação
                           number = 10) # 10 iterações diferentes
)

# Ver a acurácia em cada subconjunto (fold)
mod_random$resample %>%
  arrange(Resample)

# Acurácia geral
mod_random$resample %>%
  arrange(Resample) %>%
  summarise("Acurácia média" = mean(Accuracy))


# *****
# Exemplo com knn
# Note que os dados deveriam ser normalizados antes de usá-los
mod_knn <- train(
  heart_disease ~ ., # indicando a variável resposta
  data = dados_treino_balanceado, # dados de treino
  metric = "Accuracy", # métrica de avaliação
  method = "knn",  # árvore de decisão CART
  trControl = trainControl(method = "cv", number = 10), # cross-validation com 5 subconjuntos (folds)
  tuneGrid = data.frame(k = seq(1, 51, by = 2))
)

plot(mod_knn)
