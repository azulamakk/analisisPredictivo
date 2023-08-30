library(tidyverse)
library(ggplot2)
library(corrplot)

datos <- read.csv("~/Universidad/Datasets/Predictiva/dataset.csv")

datosFiltrados <- datos %>% filter(track_genre == 'classical'|track_genre == 'metal' | track_genre == 'jazz' | track_genre == 'punk-rock'
                                   | track_genre == 'techno' | track_genre == 'reggae' | track_genre == 'sleep'
                                   | track_genre == 'trance' | track_genre == 'study' | track_genre == 'rap') 

# Vista preliminar
summary(datos)

# Chequeo de NAs
colnames(datos)[apply(datos, 2, anyNA)] # No hay columnas con missings

# Chequeo de outliers
colnames(datos)
boxplot(datos$popularity)
boxplot(datos$loudness)
boxplot(datos$tempo)

variables <- c("acousticness", "instrumentalness", "liveness", "valence",
               "speechiness", "danceability", "energy" )
# Correlaciones
datosNumericos <- datosFiltrados %>% select(popularity, duration_ms, danceability,
                                   energy, loudness, speechiness,
                                   acousticness, instrumentalness, liveness, 
                                   valence, tempo)

M1 <- cor(datosNumericos)
corrplot.mixed(M1, order = 'AOE')

# Analisis de correlacion
p <- ggplot(datosFiltrados, aes(x = loudness, y = energy, color = explicit)) +
  geom_point(alpha = 0.3) +
  labs(x = "Loudness", y = "Energy") +
  scale_color_manual(values = c("green3", "black"), labels = c("No Explicit", "Explicit")) +
  geom_smooth() +
  theme_minimal()

print(p)

p <- ggplot(datosFiltrados, aes(x = tempo, y = loudness, color = explicit)) +
  geom_point(alpha = 0.3) +
  labs(x = "Tempo", y = "Loudness") +
  scale_color_manual(values = c("green2", "black"), labels = c("No Explicit", "Explicit")) +
  geom_smooth() +theme_minimal()

print(p)


