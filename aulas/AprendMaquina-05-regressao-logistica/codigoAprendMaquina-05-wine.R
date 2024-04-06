rm(list=ls()) # remove o que tiver na memória

# Configuração para evitar notação científica
# Define 'scipen' como 999 para exibir números em formato decimal padrão
options(scipen = 999)

# Pacotes básicos necessários em todo o código
library(tidyverse)    # para todas as operações organizadas e eficientes
library(ggthemes)     # para visualização personalizada
library(broom)        # para organizar a saída resumida do modelo em um dataframe

# -------------------------------------------------
# Importando os dados
# -------------------------------------------------

# Os dados estão disponíveis em https://archive.ics.uci.edu/dataset/186/wine+quality

# Importando
dados <- read.csv("dados/winequality-white.csv", sep=";")

# Informações úteis sobre os conjuntos de dados
glimpse(dados)


# -------------------------------------------------
# Explorando e limpando os dados
# -------------------------------------------------

# Vamos usar funções do pacote janitor para limpeza de dados e exploração tabular

# Primeiro vamos fazer algumas limpezas obrigatórias/preventivas
# Organizando os nomes das variáveis e removendo quaisquer linhas/colunas inúteis

dados2 <- dados %>%
  janitor::clean_names() %>% # Limpa e padroniza os nomes das colunas
  janitor::remove_empty(which = c("rows", "cols")) # remove todas as linhas e colunas que estão completamente vazias

# Mesmo que tenhamos removido quaisquer linhas/colunas que sejam totalmente nulas,
# precisamos verificar problemas com NA

# O pacote DataExplorer permite a criação de um mapa de valores ausentes
DataExplorer::plot_missing(dados2)  # mostra a % de NAs dentro de cada variável

# Nos dados não há NAs.
# Se houvesse, poderíamos usar a função complete.cases() para remover linhas incompletas

# Convertendo a variável quality em fator
dados2 <- dados2 %>%
  mutate(quality = factor(quality))

# Histograma de todas as variáveis
dados2 %>%
  DataExplorer::plot_histogram(title = "Histograma das variáveis contínuas")

# Gráfico de densidade
dados2 %>%
  DataExplorer::plot_density(title = "Gráfico de densidade das variáveis contínuas")

# A única variável categórica em nossos dados neste caso é aquela que usaremos para criar nossa bandeira de baixa qualidade
# Se tivéssemos muitas variáveis categóricas, faria sentido usar order_bar = TRUE
# A ordem seria então em ordem decrescente de prevalência, o que é útil à primeira vista
DataExplorer::plot_bar(data = dados2,
                       order_bar = FALSE,
                       title = "Variável categórica")

# extraindo as frequências e proporções em cada classe
janitor::tabyl(dados2$quality) %>%
  janitor::adorn_pct_formatting()

# Categorizando a variável resposta em quality < 5 (baixa qualidade)
# e quality >= 5 (boa qualidade)
dados2 <- dados2 %>%
  mutate(low_qual_flag = factor(ifelse(as.numeric(as.character(quality)) < 5,1,0))) %>%
  select(-quality)

glimpse(dados2) # verificando os dados

# Como ficou a variável resposta
janitor::tabyl(dados2$low_qual_flag) %>%
  janitor::adorn_pct_formatting()

## Normalizando os dados
# Normalização utilizada: (x - mean(x))/sd(x)
dados2[,-12] <- scale(dados2[,-12], center = TRUE, scale = TRUE)
glimpse(dados2)

## Checando a correlação entre as variáveis
corr_matrix <- cor(dados2[, sapply(dados2, is.numeric)])
corrplot::corrplot.mixed(corr_matrix)

# Há uma correlação entre density e residual_sugar e entre density e alcohol
# Mas nada preocupante da perspectiva do graus de multicolinearidade

# -------------------------------------------------
# Separando os dados em treino e teste
# -------------------------------------------------

# Partição dos dados: 80% treino / 20% teste
set.seed(777) # fixando a semente para reprodutibilidade

# Definindo os dados para partição
obs_treino <- caret::createDataPartition(y=dados2$low_qual_flag,
                                       p=0.80, list=FALSE)

# Separando os dados em treino e teste
treino <- dados2[obs_treino,]
teste <- dados2[-obs_treino,]

# shows the row count and column count of the training and test sets, to check that all worked as planned
dim(treino)
dim(teste)

# dados de treinamento
janitor::tabyl(treino$low_qual_flag) %>%
  janitor::adorn_pct_formatting()

# dados de teste
janitor::tabyl(teste$low_qual_flag) %>%
  janitor::adorn_pct_formatting()

# Note que há um desbalanceamento nos dados.
# Precisamos resolver esse problema

# Criando um conjunto de dados de treino com as classes balanceadas
set.seed(1234)
treino_balanceado <- ROSE::ROSE(low_qual_flag ~ ., data = treino)$data

# Conferindo
janitor::tabyl(treino_balanceado$low_qual_flag) %>%
  janitor::adorn_pct_formatting()

# -------------------------------------------------
# Treinando o modelo logístico
# -------------------------------------------------

# *****
# Modelagem com dados desbalanceados

# simple logistic regression
# models using all variables in the training dataset (hence ~ .)
mod1 <- glm(low_qual_flag ~ .,
                        data = treino,
                        family = binomial)

summary(mod1)

# Seleção de variáveis via BIC
mod2 <- step(mod1, k = log(nrow(treino)))
summary(mod2)

# Usando o teste de Wald para avaliar as variáveis removidas
lmtest::waldtest(mod1, mod2) # p-valor > .05 pode retirar a variável


# *****
# Modelagem com dados balanceados

mod1_b <- glm(low_qual_flag ~ .,
              data = treino_balanceado,
              family = binomial)

summary(mod1_b)

# Seleção de modelo via BIC
mod2_b <- step(mod1_b, k = log(nrow(treino_balanceado)))
summary(mod2_b)

# Conferindo se a retirada de todas as variáveis removidas é significativa
lmtest::waldtest(mod1_b, mod2_b)


# -------------------------------------------------
# Avaliando o modelo
# -------------------------------------------------

# *****
# Modelo com dados desbalanceados

modelo <- mod2
dados_teste <- teste

# R2
round( 1 - ( modelo$deviance / modelo$null.deviance ), 2 )
AIC(modelo);BIC(modelo)

# Predições nos dados de teste
predicao_test <- predict(modelo, newdata = dados_teste, type = "response" )
predicao_test_completo <- data.frame(prediction = predicao_test, low_qual_flag = dados_teste$low_qual_flag)

library(pROC)

roc <- roc(dados_teste$low_qual_flag, predicao_test)
ponto_corte <- as.numeric(coords(roc, "best", ret = "threshold"))
ponto_corte

auc(roc)

ggplot(mapping = aes(x = 1-roc$specificities, y = roc$sensitivities)) +
  geom_line(color = "#1f78b4") +
  geom_abline(intercept = 0, slope = 1, color = "grey") +  # Adiciona a linha diagonal
  annotate("text", x = 0.95, y = 0.05, label = paste("AUC =", round(auc(roc), 4)), size = 4, color = "#333333") +  # Adiciona o valor da AUC
  theme_minimal() +
  labs(title = "Curva ROC",
       x = "Taxa de falsos positivos (1 - Especificidade)",
       y = "Taxa de verdadeiros positivos (Sensibilidade)") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5))

# Matriz de confusão
predicao_teste01 <- factor(ifelse(predicao_test > ponto_corte, 1, 0),levels=c('0','1'))
caret::confusionMatrix(predicao_teste01, dados_teste$low_qual_flag, positive='1')

# *****
# Modelo com dados balanceados

modelo_b <- mod2_b
dados_teste <- teste

# R2
round( 1 - ( modelo_b$deviance / modelo_b$null.deviance ), 2 )
AIC(modelo_b);BIC(modelo_b)

# Predições nos dados de treino
predicao_test_b <- predict(modelo_b, newdata = dados_teste, type = "response" )

roc_b <- roc(dados_teste$low_qual_flag, predicao_test_b)
ponto_corte_b <- as.numeric(coords(roc_b, "best", ret = "threshold"))

auc(roc_b)

ggplot(mapping = aes(x = 1-roc_b$specificities, y = roc_b$sensitivities)) +
  geom_line(color = "#1f78b4") +
  geom_abline(intercept = 0, slope = 1, color = "grey") +  # Adiciona a linha diagonal
  annotate("text", x = 0.95, y = 0.05, label = paste("AUC =", round(auc(roc_b), 4)), size = 4, color = "#333333") +  # Adiciona o valor da AUC
  theme_minimal() +
  labs(title = "Curva ROC",
       x = "Taxa de falsos positivos (1 - Especificidade)",
       y = "Taxa de verdadeiros positivos (Sensibilidade)") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 14, hjust = 0.5))

# Matriz de confusão
predicao_test01_b <- factor(ifelse(predicao_test_b > ponto_corte_b, 1, 0),
                              levels=c('0','1'))
caret::confusionMatrix(predicao_test01_b,
                       dados_teste$low_qual_flag,
                       positive='1')


# -------------------------------------------------
# Predizendo uma nova instância
# -------------------------------------------------

xnovo <- data.frame(volatile_acidity = .8,
                    residual_sugar = 17.0,
                    free_sulfur_dioxide = 14,
                    density = .98)
pred <- predict(mod2, newdata = xnovo, type="response")
classificacao <- ifelse(pred > ponto_corte,1,0)
classificacao

