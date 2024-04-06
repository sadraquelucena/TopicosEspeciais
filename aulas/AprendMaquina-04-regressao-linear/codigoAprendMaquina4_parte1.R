#----------------------------------------------------------------
# Regressão Linear
#----------------------------------------------------------------

# Variáveis contidas nos dados:
#
# Bsal: salário anual no momento da contratação
# Sal77: Salário em março de 1975
# Sex: gênero
# Senior: número de meses desde a contratação
# Educ: Educação em anos
# Exper: experência de trabalho em meses antes da contratação


#*****

# pacotes
library(tidyverse)

# dados
salarios <- Sleuth3::case1202
glimpse(salarios)


#*****
# Correlação

# removendo as variáveis nao numéricas
salariosnumerico <- salarios %>%
  select_if(is.numeric)

# calculando a correlação de Pearson
salarios_correlacoes <- cor(salariosnumerico)

# visualizando a correlação
corrplot::corrplot(salarios_correlacoes)

# outra forma
corrplot::corrplot.mixed(salarios_correlacoes)


#*****
# Regressão Linear Múltipla

# Estimando o modelo
mod1 <- lm(Bsal ~ Sal77 + Sex + Senior + Age +
             Educ + Exper, data = salarios)
summary(mod1)

# A estatística F foi significativa (p-valor < 0.05),
# indicando que pelo meno uma variável explicativa do
# modelo tem efeito significativo na variável resposta.

# O R2 indica que o modelo explica 56,59% da variabilidade
# da variável resposta.

# O R2 ajustado é de 53,56%, mas só serve para comparar
# com outros modelos.

# O erro padrão residual (RSE) é de 483.5, mas só serve
# para comparar com outros modelos.

# Como a variável Exper tem maior p-valor, vamos retirá-la
mod2 <- lm(Bsal ~ Sal77 + Sex + Senior + Age +
             Educ, data = salarios)
summary(mod2)

#*****
# Verificando as suposições do modelo

## Média zero dos resíduos
mean(mod2$residuals)

## Normalidade dos resíduos
library(olsrr)
ols_plot_resid_hist(mod2)
ols_plot_resid_qq(mod2)  # resíduos normais se pontos estiverem sobre a linha
ols_test_normality(mod2) # resíduos normais se p-valor > .05

## Homoscedasticidade dos resíduos
ols_plot_resid_fit(mod2)

# Teste de heteroscedascidade de Breusch-Pagan
# Variância constante se p-valor > 0.05
lmtest::bptest(mod2)

## Autocorrelação dos resíduos
# Resíduos correlacionados se p-valor < .05
car::durbinWatsonTest(mod2)

## Análise de pontos influentes
# Gráfico da Distancia de Cook
ols_plot_cooksd_chart(mod2)

# Lista dos outliers
outliers <- ols_plot_cooksd_chart(mod2)$data %>%
  filter(color == "outlier") %>%
  arrange(desc(cd))
outliers

# avaliando a influência dos outliers no modelo
mod3 <- lm(Bsal ~ Sal77 + Sex + Senior + Age +
             Educ, data = salarios[-7,])
summary(mod3)
# Como a retirada do outlier influencia muda muito as
# estimativas do modelo, não vamos removê-lo

# Multicolinearidade
# VIF > 5 e tolerance < 0.2 indica multicolinearidade
ols_vif_tol(mod2)


