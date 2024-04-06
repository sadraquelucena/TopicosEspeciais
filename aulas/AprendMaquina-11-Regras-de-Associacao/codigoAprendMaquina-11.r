#################################################################
##                     Regras de Associação                     #
#################################################################

rm(list=ls()) # remove o que tiver na memória


# Vamos usar o pacore 'arules'. Instale-o se precisar.
# install.packages("arules")
library(arules)

#----------------------------------------------------------------
# 1. Carregue os dados
#----------------------------------------------------------------
# Carregue os dados de varejo em uma matriz esparsa.
# (uma matriz esparsa armazena apenas os elementos diferentes de zero e suas
#  posições na matriz).
# (Observe o uso de 'read.transactions' para este conjunto de dados).
supermart <- read.transactions("dados/retail.txt", sep = "")

# Cada linha no conjunto de dados representa uma única transação no caixa da loja.
# As linhas consistem em uma lista de números inteiros correspondentes aos itens
# comprados nessa transação.
# Os dados foram coletados durante 5 meses em uma loja.

#----------------------------------------------------------------
# 2. Explorando e preparando os dados
#----------------------------------------------------------------
# Vamos dar uma olhada nas estatísticas dos dados
summary(supermart)

# As três primeiras linhas da saída dizem que há 88162 transações (linhas)
# e 16470 itens únicos (colunas).
# A densidade (razão de itens no conjunto de dados que não estão faltando)
# é 0.0006257289.

# As próximas três linhas da saída mostram os itens mais comprados.
# O item 39 foi comprado 50675 vezes entre as 88162 transações.

# As próximas linhas informam o número de itens comprados e sua frequência.
# Ex.: Há 3016 transações de tamanho 1 (apenas 1 item comprado).

# As demais linhas da saída mostram o intervalo de valores para o tamanho das
# transações e uma amostra de três itens no conjunto de dados.

# *****
# Vamos olhar as cinco primeiras transações.
# Note que para uma matriz esparsa, o pacote 'arules' fornece a função 'inspect()'.
inspect(supermart[1:5])

# A função itemFrequency() permite examinar a frequência relativa de um item.
itemFrequency(supermart[ ,"39"])

# Note que o item 39 ocorreu em quase 60% das transações.

# *****
# Agora, vamos converter os dados em um tibble para podermos usá-lo.
library(tidyverse)
supermart_frequency <-
  tibble(
    Items = names(itemFrequency(supermart)),
    Frequency = itemFrequency(supermart)
  )

# Como ficou:
head(supermart_frequency)

# Os dez itens mais comprados nessa loja:
supermart_frequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)


#----------------------------------------------------------------
# 3. Treinando o modelo
#----------------------------------------------------------------
# Suponha que decidimos incluir itens que foram comprados pelo menos 5 vezes
# por dia em média.
# Como temos 150 dias de transações (5 meses com 30 dias em média), vamos usar
# o suporte mínimo igual a 0.0085 -> ((5*150)/88162).
# Vamos ajustar o limite de confiança em 0.5 e o tamanho mínimo da regra em 2.
supermartrules <-
  apriori(supermart,
          parameter = list(
            support = 0.0085,
            confidence = 0.5,
            minlen = 2
          ))

supermartrules

#----------------------------------------------------------------
# Avaliando a performance do modelo
#----------------------------------------------------------------
# Resumo das regras de associação da mercearia.
summary(supermartrules)

# As duas primeiras seções da saída indicam que 145 regras foram criadas.
# 76 regras têm tamanho 2, 54 têm tamanho 3 e 15 têm tamanho 4.
# A seção seguinte mostra um resumo do suporte, confiança, lift e contagem de
# regras geradas.

# Vamos olhar as 10 primeiras regras.
inspect(supermartrules[1:10])

# A primeira regra diz que 98% das vezes (confiança) os clientes que
# compram o item 371 também compram o item 38. Esse padrão foi encontrado em
# 0,86% (suporte) ou 767 (contagem) das transações no conjunto de dados.
# A regra também diz que clientes que compram o item 371 têm 5,54 vezes mais
# chance de comprar o item 38.

#----------------------------------------------------------------
# 5. Melhorar a performance do modelo
#----------------------------------------------------------------
# Na prática, geralmente teremos centenas/milhares de regras geradas a partir
# dos dados. Como não podemos (ou não devemos) analisar manualmente centenas de
# regras, precisamos encontrar maneiras de identificar as regras que podem ser úteis.

# Para nos ajudar a identificar as regras mais fortes no conjunto de dados,
# podemos ordená-las e filtrá-las.
# Vamos começar ordenando as regras por lift e examinando as 10 melhores.
supermartrules %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect() 

# Suponha que queremos descobrir se o item '41' também comprado frequentemente
# com outros itens.
# Nós usamos a função 'subset()' para encontrar subconjuntos de regras
# contendo o item '41'.
supermartrules %>%
  subset(items %in% "41") %>%
  inspect()

# E, se quisermos ver o top 10 das regras em termos de lift que incluem '41',
# podemos combinar as funções 'sort()' e 'subset()'.
supermartrules %>%
  subset(items %in% "41") %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()


# Note que a função 'subset()' pode ser usada com várias palavras-chave e operadores:
# - A palavra-chave 'items' corresponde a um item que aparece em qualquer lugar na regra.
# - Limite o subconjunto com 'lhs' e 'rhs' em vez disso.
# - O operador %in% significa que pelo menos um dos itens deve ser encontrado na lista que você definiu.
# - Para correspondência parcial (%pin%) e correspondência completa (%ain%).
# - Também podemos filtrar por suporte, confiança ou lift.
# - Também podemos combinar operadores lógicos padrão do R, como and (&), or (|) e not (!).
