rm(list=ls()) # remove o que tiver na memória

# ----------------------------------------------------
# k-NN para Classificação
# ----------------------------------------------------

# Pacotes
library(tidyverse)
library(caret)

## Importando os dados
# Conjunto de dados sobre doença cardíaca
dados <- read_csv("dados/heart.csv", col_types = "nffnnffnfnfnff")
glimpse(dados)

# *****
# Explorando e preparando os dados
summary(dados)

# Lidando com NAs
dados <- dados %>%
  janitor::clean_names() %>% # Limpa e padroniza os nomes das colunas
  janitor::remove_empty(which = c("rows", "cols")) # remove todas as linhas e colunas completamente vazias

DataExplorer::plot_missing(dados) # % de NAs de cada variável

completo <- complete.cases(dados) # identificando linhas completas
dados2 <- dados[completo,] # usando apenas linhas completas

DataExplorer::plot_missing(dados2) # % de NAs de cada variável

## Normalizando os dados numéricos
# Normalização min-max
norm_min_max <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
dados2 <- dados2 %>%
  mutate(age = norm_min_max(age),
         resting_bp = norm_min_max(resting_bp),
         cholesterol = norm_min_max(cholesterol),
         resting_hr = norm_min_max(resting_hr),
         s_tdepression = norm_min_max(s_tdepression),
         colored_vessels = norm_min_max(colored_vessels))

glimpse(dados2)

## Tratando as variáveis categóricas
# Separando a variável resposta
resp <- dados2 %>% select(heart_disease)
dados2 <- dados2 %>% select(-heart_disease)

dadosknn <- fastDummies::dummy_cols(dados2,  # criando dummies
                                    remove_selected_columns = TRUE) %>%
  janitor::clean_names()# remove variáveis originais



# *****
# Dados de treino e teste 75% / 25%
set.seed(1234)
dadosknn <- cbind(dadosknn, resp) # Juntando a variável resposta ao conjunto de dados
indices <- createDataPartition(dadosknn$heart_disease,
                               p = .75, list = F)

# Para usar a função knn do pacote class a variável resposta deve ficar separada
treino_x <- dadosknn[indices,-26] # sem a coluna com as respostas
treino_y <- dadosknn[indices,26] # coluna de respostas
teste_x <- dadosknn[-indices,-26] # sem a coluna com as respostas
teste_y <- dadosknn[-indices,26] # coluna de respostas

# Verificando se há desbalanceamento nos dados de treino
janitor::tabyl(treino_y) %>%
  janitor::adorn_pct_formatting()

# *****
# Modelo k-NN

# testando diferentes valores de k
k <- seq(from=1, to=51, by=2)
acuracia <- vector()
for (i in 1:length(k)) {
  pred <- class::knn(train = treino_x,
                     test = teste_x,
                     cl = treino_y,
                     k = k[i])
  tabela_pred <- table(teste_y, pred)
  acuracia[i] <- sum(diag(tabela_pred)) / length(teste_y)
}

# Gráfico
ggplot(data = data.frame(k,acuracia)) +
  geom_point(mapping = aes(x = k, y = acuracia),
             color = "orange", size = 3) +
  geom_line(mapping = aes(x = k, y = acuracia)) +
  geom_text(mapping = aes(x = k, y = acuracia, label = k), vjust = -0.8) +  # Adiciona os valores de k acima dos pontos
  ylab("Acurácia") +
  ggtitle("Número de Vizinhos vs. Acurácia Preditiva") +
  theme_minimal()

# Melhor: k = 13
modknn <- class::knn(train = treino_x,
                     test = teste_x,
                     cl = treino_y,
                     k = 13)

# Matriz de confusão
confusionMatrix(modknn, teste_y, positive = "TRUE")

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
# Prevendo uma nova instância
xnovo <- data.frame(age = 60, sex = "female", pain_type = "Non-Anginal Pain",
                    resting_bp = 140, cholesterol = 313, high_blood_sugar = FALSE,
                    resting_ecg = "Normal", resting_hr = 133, exercise_angina = FALSE,
                    s_tdepression = .2, s_tslope = "Upsloping", colored_vessels = 0,
                    defect_type = "ReversibleDefect")
xnovo_trasf <- xnovo %>%
  mutate(age = (age-min(dados2$age))/(max(dados2$age)-min(dados2$age)),
         resting_bp = (resting_bp-mean(dados2$resting_bp))/sd(dados2$resting_bp),
         cholesterol = (cholesterol-mean(dados2$cholesterol))/sd(dados2$cholesterol),
         resting_hr = (resting_hr-mean(dados2$resting_hr))/sd(dados2$resting_hr),
         s_tdepression = (s_tdepression-mean(dados2$s_tdepression))/sd(dados2$s_tdepression),
         colored_vessels = (colored_vessels-mean(dados2$colored_vessels))/sd(dados2$colored_vessels),
         sex_male = 0, sex_female = 1, pain_type_typical_angina = 0, pain_type_asymptomatic = 0,
         pain_type_non_anginal_pain = 1, pain_type_atypical_angina = 0, high_blood_sugar_true = 1,
         high_blood_sugar_false = 1, resting_ecg_hypertrophy = 0, resting_ecg_normal = 1,
         resting_ecg_wave_abnormality = 0, exercise_angina_false = 1, exercise_angina_true = 0,
         s_tslope_downsloping = 0, s_tslope_flat = 0, s_tslope_upsloping = 1, defect_type_fixed_defect = 0,
         defect_type_normal = 0, defect_type_reversible_defect = 1,
         .keep = "none")


class::knn(train = treino_x,
           test = xnovo_trasf,
           cl = treino_y,
           k = 19)

# -------------------------------------------------
# Regressão k-NN
# -------------------------------------------------

# Modelagem do valor médio das casas
boston <- MASS::Boston
glimpse(boston)

# *****
# Preparando os dados

# Verificando NAs e padronizando nomes das variáveis
boston <- boston %>%
  janitor::clean_names() %>% # Limpa e padroniza os nomes das colunas
  janitor::remove_empty(which = c("rows", "cols")) # remove todas as linhas e colunas completamente vazias

DataExplorer::plot_missing(boston) # % de NAs de cada variável

## Normalizando os dados numéricos
# Normalização z-score
boston2 <- boston %>%
  mutate(crim = scale(crim),
         zn = scale(zn),
         indus = scale(indus),
         nox = scale(nox),
         rm = scale(rm),
         age = scale(age),
         dis = scale(dis),
         tax = scale(tax),
         ptratio = scale(ptratio),
         black = scale(black),
         lstat = scale(lstat),
         medv = medv,
         chas = factor(chas),
         rad = factor(rad),
         .keep = "none")

boston2 <- fastDummies::dummy_cols(boston2,  # criando dummies
                        remove_selected_columns = TRUE) %>%
  janitor::clean_names()# remove variáveis originais

# *****
# Particionando os dados em treino e teste 85 / 15
set.seed(123)
indices_boston <- createDataPartition(boston2$medv, p = .85, list = F)
treino_boston <- boston2[indices_boston, ]
teste_boston <- boston2[-indices_boston, ]

treino_boston_x <- treino_boston[, -12] # removendo resposta
treino_boston_y <- treino_boston[,12] # resposta

teste_boston_x <- teste_boston[,-12] # removendo resposta
teste_boston_y <- teste_boston[,12] # resposta


# *****
# Modelo k-NN

# testando diferentes valores de k pelo RMSE
k <- 1:50
RMSE <- vector()
for (i in 1:length(k)) {
  mod_knnreg <- knnreg(x = treino_boston_x,
                 y = treino_boston_y,
                 k = k[i])
  pred <- predict(mod_knnreg, teste_boston_x)
  RMSE[i] <- caret::RMSE(teste_boston_y, pred)
}

# Gráfico
ggplot(data = data.frame(k,RMSE)) +
  geom_point(mapping = aes(x = k, y = RMSE),
             color = "orange", size = 3) +
  geom_line(mapping = aes(x = k, y = RMSE)) +
  geom_text(mapping = aes(x = k, y = RMSE, label = k), vjust = -0.8) +  # Adiciona os valores de k acima dos pontos
  ylab("RMSE") +
  ggtitle("Número de Vizinhos vs. RMSE") +
  theme_minimal()

# Melhor: k = 2
# Ajustando o modelo
modeloknn <- knnreg(x = treino_boston_x,
                   y = treino_boston_y,
                   k = 2)

pred_y <- predict(modeloknn, newdata = teste_boston_x)

# Métricas MSE, MAE e RMSE
mse <- mean((teste_boston_y - pred_y)^2)
mae <- caret::MAE(teste_boston_y, pred_y)
rmse <- caret::RMSE(teste_boston_y, pred_y)
cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)


# Gráfico dos valores reais e previstos
df <- data.frame(x = 1:length(teste_boston_y), teste_boston_y, pred_y)

# Criar o gráfico usando ggplot
ggplot(df, aes(x = x)) +
  geom_line(aes(y = teste_boston_y, color = "original-medv"), size = 1) +
  geom_line(aes(y = pred_y, color = "predicted-medv"), size = 1) +
  labs(title = "Predição de de valor de imóveis em Boston") +
  theme_minimal()
