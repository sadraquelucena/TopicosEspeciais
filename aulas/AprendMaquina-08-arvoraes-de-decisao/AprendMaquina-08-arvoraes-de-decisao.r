rm(list=ls()) # remove o que tiver na memória

# ----------------------------------------------------
# Árvores de Decisão
# ----------------------------------------------------

# Pacotes
library(tidyverse)

## Importando os dados
# Conjunto de dados sobre doença cardíaca
dados <- read_csv("dados/heart.csv", col_types = "nffnnffnfnfnff")

# Esses dados já foram explorados anteriormente

# *****
# Organizando os dados
dados <- dados %>%
  janitor::clean_names() %>% # Limpa e padroniza os nomes das colunas
  janitor::remove_empty(which = c("rows", "cols")) # remove todas as linhas e colunas completamente vazias

DataExplorer::plot_missing(dados) # % de NAs de cada variável

# *****
# Separando os dados em treino (75%) e teste (25%)
# A variável resposta é heart_disease
set.seed(1234)
indices <- caret::createDataPartition(dados$heart_disease,
                                      p = .75, list = F)
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
# Algoritmo CART
library(rpart)
mod_cart <- rpart(
  heart_disease ~ .,
  method = "class",
  data = dados_treino_balanceado
  )

# Avaliando o modelo
library(rpart.plot)
rpart.plot(mod_cart)
rpart.rules(mod_cart, style = "tall")
summary(mod_cart)

# Avaliando a performance do modelo
pred_cart <- predict(mod_cart, dados_teste, type = "class")

# Matriz de confusão
caret::confusionMatrix(data = pred_cart,
                       reference = dados_teste$heart_disease,
                       positive = "TRUE")

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


# *****
# Algoritmo C5.0
library(C50)
mod_c5.0 <- C5.0(
  x = dados_treino_balanceado[,-14], # retirando a coluna de respostas
  y = dados_treino_balanceado[,14],  # coluna de respostas
)
plot(mod_c5.0)
summary(mod_c5.0)

# Avaliando a performance do modelo
pred_c5.0 <- predict(mod_c5.0, dados_teste, type = "class")

# Matriz de confusão
caret::confusionMatrix(data = pred_c5.0,
                       reference = dados_teste$heart_disease,
                       positive = "TRUE")


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


# *****************************************************************************

# ----------------------------------------------------
# Árvores de Regressão
# ----------------------------------------------------

# Dados: Boston Housing
# Contém informações sobre o mercado imobiliário em Boston, Massachusetts, na década de 1970

# Variáveis:
# crim: Taxa de crime per capita por cidade
# zn: Proporção de terrenos residenciais zoneados para lotes com mais de 25.000 pés quadrados
# indus: Proporção de acres de negócios não varejistas por cidade
# chas: Variável dummy do Rio Charles (= 1 se o lote faz limite com o rio; 0 caso contrário)
# nox: Concentração de óxido nítrico (partes por 10 milhões)
# rm: Número médio de quartos por moradia
# age: Proporção de unidades ocupadas pelo proprietário construídas antes de 1940
# dis: Distâncias ponderadas para cinco centros de emprego de Boston
# rad: Índice de acessibilidade às rodovias radiais
# tax: Taxa de imposto sobre a propriedade de valor total por $10.000
# ptratio: Proporção aluno-professor por cidade
# black: Proporção de pessoas negras por cidade
# lstat: Porcentagem do status social mais baixo da população
# medv: Valor mediano das casas em milhares de dólares (variável resposta)

boston <- MASS::Boston

#library(MASS)
#boston <- data(Boston)

# *****
# Separando os dados em treino (85%) e teste (15%)
# A variável resposta é heart_disease
set.seed(1234)
indices <- caret::createDataPartition(boston$medv, p = .85, list = F)
boston_treino <- boston[indices, ]
boston_teste <- boston[-indices, ]


# Algoritmo CART
boston_cart <- rpart::rpart(
  medv ~ .,
  method = "anova",
  data = boston_treino
)

# Acurácia do modelo
pred_boston <- predict(boston_cart, boston_teste)
mse <- mean((boston_teste$medv - pred_boston)^2)    # erro quadrático médio
mae <- caret::MAE(boston_teste$medv, pred_boston)   # erro absoluto médio
rmse <- caret::RMSE(boston_teste$medv, pred_boston) # raiz do erro quadrático médio
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

# Compreendendo o modelo
rpart.plot::rpart.plot(boston_cart)
rpart.rules(boston_cart, style = "tall")
summary(boston_cart)

# Visualizando os dados originais e estimados
df <- data.frame(x = 1:nrow(boston_teste),
                 y = boston_teste$medv,
                 y_est = pred_boston)

ggplot(df, aes(x = x)) +
  geom_line(aes(y = y, color = "medv original")) +
  geom_line(aes(y = y_est, color = "medv predito")) +
  labs(title = "Predição de de valor de imóveis em Boston") +
  guides(color = guide_legend(title = NULL)) +
  theme_minimal()

