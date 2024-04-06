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
# install.packages(c("olsrr","lmtest"))

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

# 1. Obtenha estatísticas descritivas da variável resposta
#    (mínimo, média, mediana, máximo e desvio padrão), além
#    de um histograma, qq-plot e teste de normalidade. Se a
#    distribuição não for normal, tente a transformação de
#    Box-Cox. Avalie a simetria após a transformação inversa e,
#    se melhorar, utilize a variável transformada como resposta
#    no modelo adiante.




# 2. Visualize a dispersão entre as variáveis contínuas
#    e a resposta. Avalie a possibilidade de transformar
#    alguma variável explicativa para aprimorar a relação
#    linear no gráfico de dispersão. Prossiga caso não seja
#    possível.




# 3. Obtenha a correlação entre as variáveis contínuas.




# 4. Ajuste o modelo de regressão com todas as variáveis
#    explicativas.




# 5. Avalie se há muticolinearidade. Se identificada,
#    mantenha apenas uma das variáveis e remova as demais
#    associadas a essa multicolinearidade.




# 6. Teste interações no modelo usando MASS::stepAIC()




# 7. Verificando se há homoscedasticidade (variância constante)
#    e pontos de alavanca




# 8. Proceda a retirada de variáveis não significativas do modelo.
#    (lembre que se há muito pontos de alavanca, o teste deve ser
#    realizado usando o estimador HC4)




# 9. Verifique se os resíduos são normais




# 10. Teste se o modelo está corretamente especificado usando
#     o teste reset




# 11. Estime a pressão sistólica de um paciente que não come
#     fastfood, tem 75 kg, 177 cm de altura, IMC 23.94,
#     cintura 88 cm, 35 anos e não fuma



