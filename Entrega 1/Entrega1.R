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

datos_resumen <- datosFiltrados %>%
  group_by(track_genre) %>%
  summarise(mean_energy = mean(energy),
            prop_explicit = mean(explicit))

# Crea un gráfico de barras
grafico_barras <- ggplot(datos_resumen, aes(x = track_genre, y = mean_energy, fill = prop_explicit)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Género de la pista", y = "Media de Energy", title = "Bar Chart de Media de Energy y Proporción de Explícitas por Género") +
  scale_fill_gradient(low = "red", high = "blue") +  # Gradiente de colores según la proporción de explicit
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas en el eje x para mejor legibilidad

# Muestra el gráfico
print(grafico_barras)

variables_numericas <- datosFiltrados[, c("energy", "loudness", "tempo", "speechiness")]

# Crea la matriz de gráficos de dispersión
pairs(variables_numericas)