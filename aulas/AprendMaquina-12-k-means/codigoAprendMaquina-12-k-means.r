#################################################################
##                    Clustering com k-means                    #
#################################################################

rm(list=ls()) # remove o que tiver na memória

# Instale os pacotes que vamos precisar
# install.packages(c("stats","factoextra","gridExtra","cluster"))

library(tidyverse)

#----------------------------------------------------------------
# 1. Carregue os dados
#----------------------------------------------------------------
# Abra os dados sobre universidades
college <- read_csv("dados/college.csv", col_types = "nccfffffnnnnnnnnn")

# Preview dos dados
glimpse(college)

#----------------------------------------------------------------
# 2. Explorando e preparando os dados
#----------------------------------------------------------------
# Vamos usar apenas as universidades no estado de Maryland
# e converter o nome de cada faculdade para uma linha
maryland_college <- college %>%
  filter(state == "MD") %>%
  column_to_rownames(var = "name")

# Resumo dos dados
maryland_college %>%
  select(admission_rate, sat_avg) %>%
  summary()

# Sempre precisamos normalizar os dados para usar as técnicas de agrupamento
# Vamos normalizar os dados usando a normalização z-escore
maryland_college_scaled <- maryland_college %>%
  select(admission_rate, sat_avg) %>%
  scale()

# Como ficou
maryland_college_scaled %>%
  summary()


#----------------------------------------------------------------
# 3. Treinando o modelo
#----------------------------------------------------------------
library(stats)

# Primeiro vamos treinar o k-means com k=3 (argumento 'centers = 3').
# O argumento 'nstart' especifica o número de configurações iniciais
# para gerar os clusters. Vamos usar 25.
set.seed(1234)
k_3 <- kmeans(maryland_college_scaled, centers = 3, nstart = 25)


#----------------------------------------------------------------
# Avaliando a performance do modelo
#----------------------------------------------------------------
# Vamos olhar quantas observações há em cada cluster
k_3$size

# e os centroides de cada cluster
k_3$centers

# # Vamos visualizar os clusters gerados para insgiths adicionais
library(factoextra)
fviz_cluster(k_3,
             data = maryland_college_scaled,
             repel = TRUE,
             ggtheme = theme_minimal()) + theme(text = element_text(size = 14))

# As faculdades no cluster 1 (Johns Hopkins e University of Maryland–College Park)
# têm pontuações no SAT acima da média (> 0) e taxas de admissão abaixo da média (< 0)
# em comparação com as outras faculdades no estado.
# Essas são escolas altamente seletivas com uma população estudantil de alto desempenho.
# A pontuação média no SAT para as faculdades no cluster 2 está abaixo da média estadual,
# assim como a taxa de admissão para essas faculdades.
# As faculdades no cluster 3 geralmente têm taxas de admissão e pontuações no SAT
# iguais ou acima da média estadual.


# To further evaluate our results, we need to look at how attributes vary by cluster.
maryland_college %>%
  mutate(cluster = k_3$cluster) %>% # adiciona a coluna indicando o cluester
  select(cluster,     # seleção de variáveis 
         undergrads,
         tuition,
         faculty_salary_avg,
         loan_default_rate,
         median_debt) %>%
  group_by(cluster) %>% # agrupando os dados por cluster
  summarise_all("mean") # calculando a média

# Em comparação com outras faculdades no estado, as faculdades no cluster 1 (em média)
# tendem a ter uma população de estudantes de graduação maior (16.286),
# mensalidades mais altas ($28.244) e professores melhor remunerados ($11.258).
# Os resultados também nos indicam que os estudantes que se formam nessas escolas 
# tendem a inadimplir em seus empréstimos estudantis a uma taxa mais baixa (1,75%).
# Isso está correlacionado com o fato de que esses estudantes também tendem a ter
# um menor encargo de empréstimos ao se formarem ($17.875).

#----------------------------------------------------------------
# 5. Melhorar a performance do modelo
#----------------------------------------------------------------
# Vamos escolher o melhor valor para k.

# *****
# Método do cotovelo
# Basta colocar o argumento 'method = "wss"' na função fviz_nbclust
# A função testa automaticamente dez de 1 a 10 clusters
fviz_nbclust(maryland_college_scaled, kmeans, method = "wss")

# Os valores de k = 4 e k = 7 parecem os melhores

# Vamos circular os k ótimos usando a função geom_point()
fviz_nbclust(maryland_college_scaled, kmeans, method = "wss") +
  geom_point(
    shape = 1,
    x = 4,
    y = 7.3,
    colour = "red",
    size = 8,
    stroke = 1.5
  ) + 
  geom_point(
    shape = 1,
    x = 7,
    y = 2.3,
    colour = "red",
    size = 8,
    stroke = 1.5
  )

# *****
# Método da silhueta
fviz_nbclust(maryland_college_scaled, kmeans, method = "silhouette")

# k = 4 ou k = 7 são os melhores, mas k = 4 é superior

# Vamos circular os k ótimos usando a função geom_point()
fviz_nbclust(maryland_college_scaled, kmeans, method = "silhouette") +
  geom_point(
    shape = 1,
    x = 4,
    y = 0.393,
    colour = "red",
    size = 8,
    stroke = 1.5
  ) +
  geom_point(
    shape = 1,
    x = 7,
    y = 0.375,
    colour = "red",
    size = 8,
    stroke = 1.5
  )

# *****
# Estatística gap
fviz_nbclust(maryland_college_scaled, kmeans, method = "gap_stat")

# k = 1 ou k = 7 parecem melhores

# Vamos circular os k ótimos usando a função geom_point()
fviz_nbclust(maryland_college_scaled, kmeans, method = "gap_stat") +
  geom_point(
    shape = 1,
    x = 1,
    y = 0.218,
    colour = "red",
    size = 8,
    stroke = 1.5
  ) +
  geom_point(
    shape = 1,
    x = 7,
    y = 0.2,
    colour = "red",
    size = 8,
    stroke = 1.5
  )


# Como nossos dados têm apenas 19 faculdades, usar k = 7 vai gerar apenas
# 2 ou 3 faculdades em cada cluster na média
# Então vamos ficar com k = 4
set.seed(1234)
k_4 <- kmeans(maryland_college_scaled, centers = 4, nstart = 25)

fviz_cluster(
  k_4,
  data = maryland_college_scaled,
  main = "Maryland Colleges Segmented by SAT Scores and Admission Rates",
  repel = TRUE,
  ggtheme = theme_minimal()
) +
  theme(text = element_text(size = 14))
