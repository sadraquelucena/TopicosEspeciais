# ----------------------------------------
# Exercício
# ----------------------------------------

# Suponha que uma clínica deseja desenvolver um modelo
# preditivo para estimar a pressão arterial dos
# pacientes com base em métricas de saúde e estilo de
# vida. O objetivo é criar um portal de autoatendimento
# interativo para os pacientes para aumentar a
# conscientização sobre o problema.

# Vamos usar o conjunto de dados "health.csv" que
# contém dados de pressão arterial de seus pacientes

# Variáveis dos conjunto de dados:
# - "systolic": pressão arterial sistólica do paciente
#               (mmHg). Esta é a variável resposta.
# - "weight": peso em kg.
# - "height": altura em cm.
# - "bmi": IMC.
# - "waist": circunferência da cintura (cm).
# - "age": idade auto-relatada do paciente.
# - "diabetes": indicador binário se o paciente tem
#               diabetes (1) ou não (0).
# - "smoker": indicador binário se o paciente fuma
#             regularmente (1) ou não (0).
# - "fastfood": contagem auto-relatada de quantas
#               refeições de fast food o paciente
#               teve na última semana.

# Se necessário, instale os pacotes necessários
# install.packages(c("olsrr","lmtest","car"))

### Pacotes
library(tidyverse)
library(olsrr)

# -----
# Importando os dados
# -----
health <- read_csv("dados/health.csv",
                   col_types = "iddddiffi")

# Renomeando as variáveis
saude <- health %>%
  mutate(press.sistolica = systolic,
         peso = weight,
         altura = height,
         imc = bmi,
         cintura = waist,
         idade = age,
         diabetes = diabetes,
         fuma = smoker,
         fastfood = fastfood,
         .keep = "none")

glimpse(saude) # importamos 1445 linhas e 9 colunas


# -----
# Explorando os dados
# -----

# Descritiva da variável resposta
saude %>%
  summarise(
    `mínimo` = min(press.sistolica),
    `média` = mean(press.sistolica),
    mediana = median(press.sistolica),
    `máximo` = max(press.sistolica),
    `desvio padrão` = sd(press.sistolica)
  ) %>%
  t()

# Visualizando a variável resposta
histograma <- saude %>%
  ggplot(mapping=aes(x=press.sistolica)) +
  geom_histogram(breaks = hist(saude$press.sistolica)$breaks,
                 color = "#02457a", fill = "#97cadb") +
  #geom_density(color = "#02457a", fill = "#97cadb", alpha = 0.4) +
  labs(x = "Pressão sistólica (mmHg)",
       y = "Frequência",
       title = "Histograma") +
  geom_boxplot(aes(y=-10), fill = "#018abe", color = "#02457a",
               width = 10, # largura da caixa
               outlier.color = "#018abe", # escolhe a cor do outlier
               alpha = 0.5 # adiciona transparência
  ) +
  theme(text = element_text(size=14)) +
  theme_minimal()

qqplot <- saude %>%
  ggplot(mapping = aes(sample=press.sistolica)) +
  stat_qq(color = "#018abe") +
  stat_qq_line(color = "#02457a") +
  labs(x = "Quantis teóricos", y = "Quantis amostrais",
       title = "Q-Q plot") +
  theme_minimal()
gridExtra::grid.arrange(histograma, qqplot, ncol = 2)
ols_test_normality(saude$press.sistolica)

# Transformação de Box-Cox
BC <- MASS::boxcox(press.sistolica ~ diabetes + fastfood
                   + peso + altura + imc + cintura + idade
                   + fuma, data = saude)
lambda <- BC$x[BC$y == max(BC$y)] # valor de lambda
lambda

# Como o intervalo contém -1, vamos tentar a transformação inversa
# antes de usar (y^lambda -1)/lambda
saude <- saude %>%
  mutate(press.sistolicaINV = 1/press.sistolica)

# Visualizando
histogramaINV <- saude %>%
  ggplot(mapping=aes(x=press.sistolicaINV)) +
  geom_histogram(breaks = hist(saude$press.sistolicaINV)$breaks,
                 color = "#02457a", fill = "#97cadb") +
  labs(x = "Inversa da pressão sistólica (mmHg)",
       y = "Frequência",
       title = "Histograma") +
  geom_boxplot(aes(y=-10), fill = "#018abe", color = "#02457a",
               width = 10, # largura da caixa
               outlier.color = "#018abe", # escolhe a cor do outlier
               alpha = 0.5 # adiciona transparência
  ) +
  theme(text = element_text(size=14)) +
  theme_minimal()

qqplotINV <- saude %>%
  ggplot(mapping = aes(sample=press.sistolicaINV)) +
  stat_qq(color = "#018abe") +
  stat_qq_line(color = "#02457a") +
  labs(x = "Quantis teóricos", y = "Quantis amostrais",
       title = "Q-Q plot") +
  theme_minimal()
gridExtra::grid.arrange(histogramaINV, qqplotINV, ncol = 2)
ols_test_normality(saude$press.sistolicaINV)

# Como melhorou a simetria, vamos usar essa variável transformada

# Visualizando as variáveis preditoras contínuas
saude %>%
  select(-press.sistolica) %>% # seleciona todas menos press.sistolica
  keep(is.numeric) %>% # mantem apenas as variáveis numéricas
  gather() %>% # reúne todos os dados em uma coluna com os nomes das variáveis (key) e outra com os valores correspondentes (value)
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") + # cria painéis separados para cada variável
  theme_minimal() +
  labs(x = "Variável", y = "Frequência") +
  theme(text = element_text(size=14))

# Visualizando a dispersão entre as variáveis contínuas
# e a resposta
saude %>%
  ggplot(mapping = aes(x = peso, y = press.sistolica)) +
  geom_point()

saude %>%
  ggplot(mapping = aes(x = altura, y = press.sistolica)) +
  geom_point()

saude %>%
  ggplot(mapping = aes(x = imc, y = press.sistolica)) +
  geom_point()

saude %>%
  ggplot(mapping = aes(x = cintura, y = press.sistolica)) +
  geom_point()

saude %>%
  ggplot(mapping = aes(x = idade, y = press.sistolica)) +
  geom_point()

saude %>%
  ggplot(mapping = aes(x = fastfood, y = press.sistolica)) +
  geom_point()

# Correlação entre as variáveis contínuas
correlacoes <- cor(saude[c("press.sistolica","peso","altura","imc","cintura","idade","fastfood")])

# Gŕafico
corrplot::corrplot.mixed(correlacoes)



# -----
# Ajustando o modelo
# -----

# Ajustando o modelo de regressão com todas as
# variáveis explicativas
mod1 <- lm(press.sistolicaINV ~ diabetes + fastfood
            + peso + altura + imc + cintura + idade
            + fuma, data = saude)
summary(mod1)

# -----
# Vamos fazer um diagnóstico do modelo
# -----

# Multicolinearidade
# Há multicolinearidade se VIF > 5 e tolerance < 0.2
ols_vif_tol(mod1)

# Vamos retirar altura, imc e cintura
mod2 <- lm(press.sistolicaINV ~ diabetes + fastfood
           + peso + idade + fuma, data = saude)
summary(mod2)

# Checando novamente a multicolinearidade
# Há multicolinearidade se VIF > 5 e tolerance < 0.2
ols_vif_tol(mod2)

# Testando interações no modelo
mod3 <- MASS::stepAIC(mod2,
                      scope = list(lower = ~1,upper = ~.^2),
                      k = log(546)) # usa o critério BIC
summary(mod3)

# Verificando se a média dos resíduos é zero
mean(mod2$residuals)

# Verificando se há homoscedasticidade (variância constante)
# (p-valor > 0.05 => homoscedasticidade)
ols_plot_resid_fit(mod3)
ols_test_breusch_pagan(mod3)

# como p-valor > 0.05, há homoscedasticidade

# Vamos verificar pontos de alavanca
ols_plot_resid_lev(mod3)

# Quando há pontos de alavanca, devemos retirar variáveis usando
# o estimador HC4
lmtest::coeftest(mod3, vcov=sandwich::vcovHC(mod3, type="HC4"))


# Normalidade dos resíduos
ols_plot_resid_qq(mod3)
ols_test_normality(mod3)
car::qqPlot(mod3, simulate = TRUE, n = 1000, main = "Envelope Simulado - Resíduos Padronizados", pch=20)

# Teste de especificidade do modelo
# H0: o modelo está corretamente especificado
lmtest::resettest(mod3)


# Estimando a pressão sistólica de um paciente
novo.paciente <- data.frame(peso = 75, idade = 35)
pred <- predict(mod3, newdata = novo.paciente)
1/pred # obtendo a transformação inversa
