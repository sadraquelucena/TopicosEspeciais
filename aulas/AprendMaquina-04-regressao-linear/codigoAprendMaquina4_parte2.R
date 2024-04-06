# -----------------------------------------------------
#  APLICAÇÃO DE UM MODELO DE REGRESSÃO LINEAR
# -----------------------------------------------------

# Pacotes
library(tidyverse)
library(olsrr)
library(gtsummary) # para criar tabelas

# ----------------------------------------
# Importando os dados
# ----------------------------------------
bd <- read_table("dados/dados.txt")

# Trduzindo as variáveis
dados <- bd %>%
  mutate(preco = price,
         tam_lote = 0.09*lotsize, # transformando pés em metros quadrados
         quartos = bedrooms,
         banheiros = bathrms,
         pavimentos = stories,
         garagem = fct_recode(driveway,
                              "Sim" = "yes",
                              "Não" = "no"),
         quarto_recreacao = fct_recode(recroom,
                                       "Sim" = "yes",
                                       "Não" = "no"),
         porao = fct_recode(fullbase,
                            "Sim" = "yes",
                            "Não" = "no"),
         aquec_gas_agua = fct_recode(gashw,
                                     "Sim" = "yes",
                                     "Não" = "no"),
         ar_central = fct_recode(airco,
                                 "Sim" = "yes",
                                 "Não" = "no"),
         n_vagas_garagem = garagepl,
         bairro_preferido = fct_recode(prefarea,
                                       "Sim" = "yes",
                                       "Não" = "no"),
         .keep = "none")

# Ficamos então com as seguintes variáveis
# - preco: preço de venda da casa
# - tam_lote: tamanho do lote do imóvel
# - quartos: número de quartos
# - banheiros: número de banheiros
# - pavimentos: número de pavimentos, excluindo o porão
# - garagem: há entrada para carros
# - quarto_recreacao: há um quarto de recreação
# - porao: há um porão totalmente construído
# - aquec_gas_agua: uso de gás para aquecimento de água
# - há ar-condicionado central
# - n_vagas_garagem: número de vagas na garagem
# - bairro_preferido: é localizada no bairro preferido da cidade

glimpse(dados)


# ----------------------------------------
# Explore os dados
# ----------------------------------------

#*****
# Descritiva da variável resposta
dados %>%
  summarise(
    `mínimo` = min(preco),
    `média` = mean(preco),
    mediana = median(preco),
    `máximo` = max(preco),
    `desvio padrão` = sd(preco)
  ) %>%
  t() # transposta

# Gráfico
hist.preco <- dados %>%
  ggplot(mapping = aes(x = preco)) +
  geom_histogram(aes(y = ..density..), color = "#02457a",
                 fill = "#018abe", breaks = hist(dados$preco)$breaks) +
  geom_density(color = "#018abe", fill = "#97cadb", alpha = 0.4) +
  ylab("Densidade") +
  xlab("Preço do imóvel") +
  geom_boxplot(aes(y=-.000001), fill = "#018abe", color = "#02457a",
               width = 10^(-6), # largura da caixa
               outlier.color = "#018abe", # escolhe a cor do outlier
               alpha = 0.5 # adiciona transparência
  ) +
  theme_minimal()

qq.preco <- qqploty <- dados %>%
  ggplot(mapping = aes(sample=preco)) +
  stat_qq(color = "#018abe") +
  stat_qq_line(color = "#02457a") +
  labs(x = "Quantis teóricos", y = "Quantis amostrais",
       title = "Q-Q plot") +
  theme_minimal()

gridExtra::grid.arrange(hist.preco, qq.preco, ncol = 2)

# Testes de normalidade
ols_test_normality(dados$preco)

# A variável resposta não é normal
# Vamos transformá-la mais à frente e ver se o modelo fica melhor

#*****
## Variáveis explicativas
dados %>%
  summarise(
    `mínimo` = min(tam_lote),
    `média` = mean(tam_lote),
    mediana = median(tam_lote),
    `máximo` = max(tam_lote),
    `desvio padrão` = sd(tam_lote)
  ) %>%
  round(2) %>% # arredondar para 2 casas decimais
  t()

dados %>%
  select(-c(preco, tam_lote)) %>% # seleciona todas menos preco e tam_lote
  keep(is.numeric) %>% # mantem apenas as variáveis numéricas
  gather() %>% # reúne todos os dados em uma coluna com os nomes das variáveis (key) e outra com os valores correspondentes (value)
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +
  facet_wrap(~ key, scales = "free") + # cria painéis separados para cada variável
  theme_minimal() +
  labs(x = "Variável", y = "Frequência") +
  theme(text = element_text(size=14))

dados %>%
  select(-c(preco, tam_lote)) %>%
  tbl_summary()

# Como algumas catergorias das variáveis têm baixa
# frequência, vamos juntar algumas delas e criar
# variáveis dummies

# quartos: vamos juntar até 2 e também 5 ou mais
dados <- dados %>%
  mutate(quartos12 = ifelse(quartos <= 2, 1, 0),
         quartos3 = ifelse(quartos == 3, 1, 0),
         quartos4 = ifelse(quartos == 4, 1, 0),
         quartos56 = ifelse(quartos >= 5, 1, 0)) %>%
  mutate(quartos12 = factor(quartos12, levels = c(0,1), labels = c("Não","Sim")),
         quartos3 = factor(quartos3, levels = c(0,1), labels = c("Não","Sim")),
         quartos4 = factor(quartos4, levels = c(0,1), labels = c("Não","Sim")),
         quartos56 = factor(quartos56, levels = c(0,1), labels = c("Não","Sim")))

# banheiros: juntar 3 e 4 banheiros
dados <- dados %>%
  mutate(banheiros1 = ifelse(banheiros == 1, 1, 0),
         banheiros2 = ifelse(banheiros == 2, 1, 0),
         banheiros34 = ifelse(banheiros >= 3, 1, 0)) %>%
  mutate(banheiros1 = factor(banheiros1, levels = c(0,1), labels = c("Não","Sim")),
         banheiros2 = factor(banheiros2, levels = c(0,1), labels = c("Não","Sim")),
         banheiros34 = factor(banheiros34, levels = c(0,1), labels = c("Não","Sim")))

# pavimentos
dados <- dados %>%
  mutate(pavimentos1 = ifelse(pavimentos == 1, 1, 0),
         pavimentos2 = ifelse(pavimentos == 2, 1, 0),
         pavimentos3 = ifelse(pavimentos == 3, 1, 0),
         pavimentos4 = ifelse(pavimentos == 4, 1, 0)) %>%
  mutate(pavimentos1 = factor(pavimentos1, levels = c(0,1), labels = c("Não","Sim")),
         pavimentos2 = factor(pavimentos2, levels = c(0,1), labels = c("Não","Sim")),
         pavimentos3 = factor(pavimentos3, levels = c(0,1), labels = c("Não","Sim")),
         pavimentos4 = factor(pavimentos4, levels = c(0,1), labels = c("Não","Sim")))

# vagas_garagem: juntar 2 e 3 vagas
dados <- dados %>%
  mutate(n_vagas_garagem0 = ifelse(n_vagas_garagem == 0, 1, 0),
         n_vagas_garagem1 = ifelse(n_vagas_garagem == 1, 1, 0),
         n_vagas_garagem23 = ifelse(n_vagas_garagem >= 2, 1, 0)) %>%
  mutate(n_vagas_garagem0 = factor(n_vagas_garagem0, levels = c(0,1), labels = c("Não","Sim")),
         n_vagas_garagem1 = factor(n_vagas_garagem1, levels = c(0,1), labels = c("Não","Sim")),
         n_vagas_garagem23 = factor(n_vagas_garagem23, levels = c(0,1), labels = c("Não","Sim")))

# Gráfico de dispersão entre variável resposta
# e a variável explicativa tam_lote
dados %>%
  ggplot(mapping = aes(x = tam_lote, y = preco)) +
  geom_point(color = "#018abe", fill = "#97cadb", shape = 21) +
  labs(x = expression('Tamanho do lote (m'^2*')'),
       y = "Preço ($)") +
  theme_minimal()

# ----------------------------------------
# Ajuste do modelo
# ----------------------------------------

# No modelo de regressão linear, os erros devem ter
# distribuição normal, isso implica que a variável
# resposta também deve ter

# Como a variável resposta não tem distribuição normal,
# vamos usar a transformação de Box-Cox

# A transformação de Box-Cox é utilizada para estabilizar
# a variância e tornar os dados mais aproximados da normalidade
# No método buscamos um valor lambda de modo que usamos a
# tranformação:
#   -> y_transf = (y^(lambda)-1)/(lambda), se lambda != 0
#   -> y_transf = log(y), se lambda = 0

BC <- MASS::boxcox(preco ~ tam_lote + quartos3 +
                     quartos4 + quartos56 + banheiros2 +
                     banheiros34 + pavimentos2 + pavimentos3 +
                     pavimentos4 + garagem + quarto_recreacao +
                     porao + aquec_gas_agua + ar_central +
                     n_vagas_garagem1 + n_vagas_garagem23 + bairro_preferido,
                   data = dados)
lambda <- BC$x[BC$y == max(BC$y)] # valor de lambda
lambda

# Como o intervalo de confiança de 95% para lambda (ver gráfico)
# contém o zero, vamos usar a transformação log


dados <- dados %>%
  mutate(lpreco = log(preco))

# Histograma
hist.lpreco <- dados %>%
  ggplot(mapping = aes(x = lpreco)) +
  geom_histogram(aes(y = ..density..), color = "#0a6921",
                 fill = "#429b46", breaks = hist(dados$lpreco)$breaks) +
  geom_density(color = "#0a6921", fill = "#429b46", alpha = 0.4) +
  ylab("Densidade") +
  xlab("Logaritmo natural do preço do imóvel") +
  geom_boxplot(aes(y=-.1), fill = "#429b46", color = "#0a6921",
               width = .05, # largura da caixa
               outlier.color = "#429b46", # escolhe a cor do outlier
               alpha = 0.5 # adiciona transparência
  ) +
  theme_minimal()

qq.lpreco <- dados %>%
  ggplot(mapping = aes(sample=lpreco)) +
  stat_qq(color = "#429b46") +
  stat_qq_line(color = "#0a6921") +
  labs(x = "Quantis teóricos", y = "Quantis amostrais",
       title = "Q-Q plot") +
  theme_minimal()

gridExtra::grid.arrange(hist.lpreco, qq.lpreco, ncol = 2)

# Testes de normalidade
ols_test_normality(dados$lpreco)

# Vamos avaliar a relação entre a resposta transformada e a
# variável explicativa contínua
dados %>%
  ggplot(mapping = aes(x = tam_lote, y = lpreco)) +
  geom_point() +
  theme_minimal()
dados %>% select(lpreco, tam_lote) %>% cor()

# Vamos ver se melhora relação aplicando log em tam_lote
dados <- dados %>%
  mutate(ltam_lote = log(tam_lote))

dados %>%
  ggplot(mapping = aes(x = ltam_lote, y = lpreco)) +
  geom_point() +
  theme_minimal()
dados %>% select(lpreco, ltam_lote) %>% cor()

# Como melhorou a relação linear, vamos usar ltam_lote

#*****
# Modelo
mod1 <- lm(lpreco ~ ltam_lote + quartos3 +
             quartos4 + quartos56 + banheiros2 +
             banheiros34 + pavimentos2 + pavimentos3 +
             pavimentos4 + garagem + quarto_recreacao +
             porao + aquec_gas_agua + ar_central +
             n_vagas_garagem1 + n_vagas_garagem23 +
             bairro_preferido,
           data = dados)

# Verificando se há multicolinearidade
# Há multicolinearidade se VIF > 5 e tolerance < 0.2
# Quando isso ocorrer, deixamos apenas uma das variáveis
# no modelo
ols_vif_tol(mod1)

# Como não há multicolinearidade, vamos prosseguir
summary(mod1)

# Testando combinações de variáveis e interações no modelo
mod2 <- MASS::stepAIC(mod1,
                      scope = list(lower = ~1,upper = ~.^2),
                      k = log(546)) # usa o critério BIC e 546 é o número de obs.
summary(mod2)

# Verificando se a média dos resíduos é zero
mean(mod2$residuals)

# Verificando se há homoscedasticidade (variância constante)
# (p-valor > 0.05 => homoscedasticidade)
ols_plot_resid_fit(mod2)
ols_test_breusch_pagan(mod2)

# como p-valor > 0.05, há homoscedasticidade

# Vamos verificar pontos de alavanca
ols_plot_resid_lev(mod2)

# Quando há pontos de alavanca, devemos retirar variáveis usando
# o estimador HC4
lmtest::coeftest(mod2, vcov=sandwich::vcovHC(mod2, type="HC4"))

# removendo interação entre garagem e bairro_preferido
mod3 <- lm(lpreco ~ ltam_lote + banheiros2 +
             banheiros34 + pavimentos2 + pavimentos3 +
             pavimentos4 + garagem + porao + aquec_gas_agua +
             ar_central + n_vagas_garagem1 + n_vagas_garagem23 +
             bairro_preferido + aquec_gas_agua*n_vagas_garagem23 +
             banheiros2*porao,
           data = dados)
lmtest::coeftest(mod3, vcov=sandwich::vcovHC(mod3, type="HC4"))

# removendo interação entre aquec_gas_agua n_vagas_garagem2
mod3 <- lm(lpreco ~ ltam_lote + banheiros2 +
             banheiros34 + pavimentos2 + pavimentos3 +
             pavimentos4 + garagem + porao + aquec_gas_agua +
             ar_central + n_vagas_garagem1 + n_vagas_garagem23 +
             bairro_preferido +
             banheiros2*porao,
           data = dados)
lmtest::coeftest(mod3, vcov=sandwich::vcovHC(mod3, type="HC4"))

# Teste de heteroscedasticidade
ols_plot_resid_fit(mod3)
ols_test_breusch_pagan(mod3)

# Normalidade dos resíduos
ols_plot_resid_qq(mod3)
ols_test_normality(mod3)
car::qqPlot(mod3, simulate = TRUE, n = 1000, main = "Envelope Simulado - Resíduos Padronizados", pch=20)

# Pontos de alavanca
ols_plot_resid_lev(mod3)
ols_plot_cooksd_chart(mod3)


# Teste de especificidade do modelo
# H0: o modelo está corretamente especificado
lmtest::resettest(mod3)

# Estimando o preco de uma nova casa
nova.casa <- data.frame(ltam_lote = log(200), banheiros2 = "Sim",
                        banheiros34 = "Não", pavimentos2 = "Sim",
                        pavimentos3 = "Não", pavimentos4 = "Não",
                        garagem = "Sim", porao = "Sim",
                        aquec_gas_agua = "Sim", ar_central = "Não",
                        n_vagas_garagem1 = "Não",
                        n_vagas_garagem23 = "Sim",
                        bairro_preferido = "Sim")
pred <- predict(mod3, newdata = nova.casa)
exp(pred) # inversa da transformação log
