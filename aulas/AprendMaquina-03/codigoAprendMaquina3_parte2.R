# Vamos usar dados sobre notas de estudantes de uma
# escola pública disponível em
# https://www.kaggle.com/datasets/desalegngeb/students-exam-scores

# Pacotes
library(tidyverse)
library(DescTools) # imputação de valores

# Importando os dados
bd <- read_csv("/home/sadraque/Documentos/UFS/Disciplinas/2023.2/ESTAT0016 - TOPICOS ESPECIAIS EM ESTATISTICA/aulas/AprendMaquina-03/students_exam_scores.csv")

# Traduzindo as variáveis para o português
dados <- bd %>%
  mutate(genero = recode(Gender,
                         "female" = "Feminino",
                         "male" = "Masculino"),
         etnia = recode(EthnicGroup,
                        "group A" = "Grupo A",
                        "group B" = "Grupo B",
                        "group C" = "Grupo C",
                        "group D" = "Grupo D",
                        "group E" = "Grupo E"),
         formacao_pais = recode(ParentEduc,
                                "associate's degree" = "Curso técnico",
                                "bachelor's degree" = "Bacharelado",
                                "high school" = "Ensino Médio",
                                "master's degree" = "Mestrado",
                                "some college" = "Superior incompleto",
                                "some high school" = "Ensino Médio incompleto"),
         tipo_almoco  = recode(LunchType,
                               "standard" = "Padrão",
                               "free/reduced" = "Gratuito/Reduzido"),
         curso_preparatorio = recode(TestPrep,
                                     "complete" = "sim",
                                     "none" = "não"),
         estado_civil_pais = recode(ParentMaritalStatus,
                                                   "married" = "Casado(a)",
                                                   "single" = "Solteiro(a)",
                                                   "widowed" = "Viúvo(a)",
                                                   "divorced" = "Divorciado(a)"),
         pratica_esporte = recode(PracticeSport,
                                             "never" = "Nunca",
                                             "sometimes" = "Às vezes",
                                             "regularly" = "Regularmente"),
         filho_primogenito = recode(IsFirstChild,
                                    "yes" = "Sim",
                                    "no" = "Não"),
         numero_irmaos = NrSiblings,
         meio_transporte = recode(TransportMeans,
                                           "school_bus" = "Ônibus escolar",
                                           "private" = "Veículo particular"),
         horas_estudo_semanal = recode(WklyStudyHours,
                                           "less than 5hrs" = "Menos de 5 horas",
                                           "between 5 and 10hrs" = "Entre 5 e 10 horas",
                                           "more than 10hrs" = "Mais de 10 horas"),
         nota_matematica = MathScore,
         nota_leitura = ReadingScore,
         nota_escrita = WritingScore,
         .keep = "none") # descarta as variaveis antigas

# Dimensão dos dados
dim(dados) # 30641 linhas e 14 colunas

# Observando os tipos de variáveis
str(dados)

#*****
# Lidando com NAs

# Contando quantos NAs há em cada variável
dados %>%       # seleciona os dados
  is.na() %>%   # verifica se há NAs
  colSums() %>% # soma quantos NAs há em cada coluna
  sort(decreasing = TRUE) %>% # ordena em ordem decrescente
  as.data.frame() # apresenta como um data frame


## Substituindo NAs pela moda em variáveis categóricas

# meio_transporte
dados %>%                     # seleciona os dados
  select(meio_transporte) %>% # seleciona a variavel meio_transporte
  table(useNA = "always") %>% # calcula as frequências, inclusive dos NAs
  sort(decreasing = TRUE)     # ordena em ordem decrescente

# substituindo NAs pela moda "Ônibus escolar"
dados <- dados %>%   # seleciona os dados
  # troca os NAs pela moda
  mutate(meio_transporte2 = ifelse(is.na(meio_transporte),
                                   "Ônibus escolar", meio_transporte))
# verificando
dados %>%
  select(meio_transporte2) %>% # seleciona a variavel meio_transporte2
  table(useNA = "always")  # calcula as frequências, inclusive dos NAs

# Faça para as demais variáveis categóricas
formacao_pais
etnia
curso_preparatorio


## Substituindo NAs em variáveis numéricas

# Em variáveis discretas costumamos usar a moda
# Em variáveis contínuas usamos:
# - a média se os dados não simétricos e não há valores discrepantes
# - a mediana se os dados não são simétricos e se há valores discrepantes

# numero_irmaos
hist(dados$numero_irmaos)

# Obtendo a moda
# meio_transporte
dados %>%                     # seleciona os dados
  select(numero_irmaos) %>% # seleciona a variavel meio_transporte
  table(useNA = "always") %>% # calcula as frequências, inclusive dos NAs
  sort(decreasing = TRUE)

# Substituindo NAs pela moda
dados <- dados %>%
  mutate(numero_irmaos2 = ifelse(is.na(numero_irmaos),
                                 1, numero_irmaos))

dados %>%                     # seleciona os dados
  select(numero_irmaos2) %>% # seleciona a variavel meio_transporte
  table(useNA = "always") %>% # calcula as frequências, inclusive dos NAs
  sort(decreasing = TRUE)



#*****
# Agora crie novas variáveis usando os diferentes
# tipos de normalização: escalonamento decimal, z-score,
# min-max e transformação logarítmica para cada situação
# adequada

# Faça apenas para nota_matematica
minmax <- function(x){
  return( (x - max(x))/(min(x)-max(x)) )
}

dados <- dados %>%
  mutate(nota_matematica_MM = minmax(nota_matematica) )

#*****
# Discretize as notas em maior ou menor que 70

dados <- dados %>%
  mutate(nota_matematica70 = ifelse(nota_matematica >= 70,
                                   1, 0))
dados %>%
  select(nota_matematica70) %>%
  table()

# Discretize as demais



#*****
# Transformando variáveis em dummies

# genero
dados <- dados %>%
  fastDummies::dummy_cols(select_columns = "genero")

dados %>%
  select(genero_Feminino) %>%
  table()


#*****
# Separe os dados em 70% treino e 30% teste



#*****
# Outros algoritmos de imputação de dados que podem ser usados no R:
# MICE, kNN e missForest
