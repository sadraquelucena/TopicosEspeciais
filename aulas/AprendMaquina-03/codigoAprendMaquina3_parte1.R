# Pacotes
#install.packages("spotifyr")
library(tidyverse)
library(spotifyr)

# credenciais de acesso ao Spotify
Sys.setenv(SPOTIFY_CLIENT_ID = "0890762a9f504fce822a215129d19687")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "0fdd8307a5da4275b8bc2f128b7fb218")
token <- get_spotify_access_token()

# Obter as faixas da playlist "Top 50 - Brazil"
topbr <- get_playlist_tracks(playlist_id = "37i9dQZEVXbMXbN3EUUhlg")

# Nomes dos artistas
artista <- vector()
for (i in 1:nrow(topbr))
  artista[i] <- paste(topbr$track.artists[[i]]$name, collapse = "|")

# caracteristicas das músicas
caracteristicas <- get_track_audio_features(ids = topbr$track.id) %>%
  select(ao_vivo = liveness,
         dancabilidade = danceability,
         energia = energy,
         fala = speechiness,
         acustica = acousticness,
         valencia = valence)

# Juntando tudo em uma única tabela
tudo <- tibble(topbr, artista, caracteristicas)

# Separando as informações necessárias
top50br <- tudo %>%
  select(id = track.id, nome = track.name, artista,
         duracao = track.duration_ms,
         popularidade = track.popularity,
         ao_vivo, dancabilidade, energia, fala,
         acustica, valencia)

# Salvando os dados para usar depois
writexl::write_xlsx(top50br,
                    path = "/home/sadraque/Documentos/UFS/Disciplinas/2023.2/ESTAT0016 - TOPICOS ESPECIAIS EM ESTATISTICA/aulas/AprendMaquina-03/top50br.xlsx")

# Sobre os dados

# id: Identificador da música no spotify
# nome: nome da música
# artista: nome dos artistas separados por "|"
# duracao: duração da música em milissegundos
# popularidade: valor entre 0 e 100,
#               com 100 sendo o mais popular
# ao_vivo: probabilidade de que a música tenha
#          sido executada ao vivo
# dancabilidade: valor de 0,0 é menos dançável
#                e 1,0 é mais dançante.
# energia: medida de 0,0 a 1,0 e representa uma
#          medida perceptual de intensidade e atividade.
# fala: quanto mais exclusivamente falada for a gravação,
#       mais próximo de 1,0
# acustica: medida de confiança de 0,0 a 1,0 para
#           saber se a faixa é acústica. 1.0 representa alta confiança de que a faixa é acústica
# valencia: quanto mais próximo de 1, mais positividade musical transmitida



#*********
# Análise
top50br <- readxl::read_xlsx("/home/sadraque/Documentos/UFS/Disciplinas/2023.2/ESTAT0016 - TOPICOS ESPECIAIS EM ESTATISTICA/aulas/AprendMaquina-03/top50br.xlsx")
View(top50br) # visualizar a tabela com os dados

# Artista
top50br %>%   # seleciona o objeto top50br
  select(artista) %>%  # seeciona a coluna artista
  separate_longer_delim(artista, delim = "|") %>% # separa os elementos da coluna e coloca-os um abaixo do outro na mesma linha
  count(artista, sort = TRUE) %>%
  slice_head(n = 18) %>%
  ggplot(aes(x = n, y = fct_reorder(artista, n))) +
  geom_bar(stat = "identity", fill = "blue", alpha = .7) +
  labs(title = "Frequência dos 15 Principais Artistas no Top 50 Brasil",
       x = "Frequência",
       y = "Artista") +
  theme_minimal()

# Distribuição da duração da música

# função que converte milissegundos em minutos e segundos
converte_ms_min <- function(x){
  # calcula os minutos
  minutos <- floor(x / 60000)

  # calcula os segundos
  segundos <- round((x %% 60000) / 1000)

  # junta tudo
  min_s <- paste(minutos, segundos, sep = ".")

  return( as.numeric(min_s) ) # retornando a saída
}

top50br <- top50br %>%   # convertendo milissegundos em minutos
  mutate( duracao_min = converte_ms_min(duracao) )

ggplot(top50br, aes(x = duracao_min)) +
  geom_histogram(aes(y = ..density..), color = "#02457a", fill = "#018abe") +
  geom_density(color = "#018abe", fill = "#97cadb", alpha = 0.4) +
  ylab("Densidade") +
  xlab("Duração em minutos") +
  geom_boxplot(aes(y=-.1), fill = "#018abe", color = "#02457a",
               width = .1, # largura da caixa
               outlier.color = "#018abe", # escolhe a cor do outlier
               alpha = 0.5 # adiciona transparência
  ) +
  theme_minimal()

# identificando os outliers
top50br %>%
  filter(duracao_min > 10) %>%
  select(duracao_min, nome, artista)

top50br %>%
  filter(duracao_min < 2) %>%
  select(duracao_min, nome, artista)

top50br %>%
  summarise(min(duracao_min), mean(duracao_min),
            median(duracao_min), max(duracao_min),
            sd(duracao_min))

summary(top50br$duracao_min)
sd(top50br$duracao_min)

# Presença de musicas ao vivo
ggplot(top50br, aes(x = ao_vivo)) +
  geom_histogram(aes(y = ..density..), binwidth = .1, color = "#0A6921", fill = "#429B46") +
  geom_density(color = "#429B46", fill = "#94C58C", alpha = 0.4) +
  ylab("Densidade") +
  xlab("Chance de ser música ao vivo") +
  geom_boxplot(aes(y=-.1), fill = "#429B46", color = "#0A6921",
               width = .1, # width of boxes
               outlier.color = "#429B46", # remove color of outliers
               alpha = 0.5 # add transparency
  ) +
  xlim(0,1) +
  theme_minimal()

# dançabilidade
# energia
# fala
# acustica
# valencia


# Valencia vs energia

ggplot(top50br, aes(x = valencia, y = energia)) +
  geom_point(color = "mediumblue", alpha = .5, size = 4) +
  geom_vline(xintercept = 0.5, color = "darkgrey") +
  geom_hline(yintercept = 0.5, color = "darkgrey") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Agitado/Raivoso", fontface =
             "bold") +
  annotate('text', 1.75 / 2, 0.95, label = "Feliz/Alegre", fontface = "bold") +
  annotate('text', 1.75 / 2, 0.05, label = "Relaxado/Sereno", fontface =
             "bold") +
  annotate('text', 0.25 / 2, 0.05, label = "Triste/Deprimente", fontface =
             "bold") +
  labs(title = "Classificação emocional da música") +
  xlab("Valência") +
  ylab("Energia") +
  theme_minimal()

top50br %>%
  filter(valencia > .5 & energia > .5) %>%
  count()

36/50

# Dançabilidade vs popularidade
# Dançabilidade vs energia
