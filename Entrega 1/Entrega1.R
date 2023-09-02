library(tidyverse)
library(ggplot2)
library(corrplot)

datos <- read.csv("~/Universidad/Datasets/Predictiva/dataset.csv")

datosFiltrados <- datos %>% filter(track_genre == 'classical'|track_genre == 'metal' | track_genre == 'jazz' | track_genre == 'punk-rock'
                                   | track_genre == 'techno' | track_genre == 'reggae' | track_genre == 'sleep'
                                   | track_genre == 'trance' | track_genre == 'study' | track_genre == 'hip-hop') 
# Vista preliminar
summary(datosFiltrados)

# Chequeo de NAs
colnames(datosFiltrados)[apply(datosFiltrados, 2, anyNA)] # No hay columnas con missings

# Análisis de outliers
hacer_boxplot <- function(variable, titulo, colores){
  ggplot(variable, aes(x = reorder(Variable, Valor, na.rm=T),y = Valor)) +
    geom_boxplot(fill = colores,size = 0.7) +
    labs(title = titulo, x = "Variables", y = "Valor") +
    theme(text = element_text(family = "mono"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey'))+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Variables numéricas con rango de [0,1]
variables_numericas <- datosFiltrados %>% select(all_of(c("acousticness", "instrumentalness", "liveness", "valence","speechiness", "danceability", "energy" ))) #Sólo uso las numéricas
variables_boxplot <- tidyr::gather(variables_numericas, Variable, Valor) #Las meto en un df con 2 columnas
colores <- colorRampPalette(c("green", "black"))(7) #Creo la paleta de colores 
hacer_boxplot(variables_boxplot, "Boxplots de las variables numéricas con rango [0,1]", colores)

# Variables numéricas rango fuera del [0,1]
variables_numericas_2 <- datosFiltrados %>% select(popularity, loudness, tempo)
variables_boxplot_2 <- tidyr::gather(variables_numericas_2, Variable, Valor)
colores_2 <- colorRampPalette(c("green", "black"))(3) 
hacer_boxplot(variables_boxplot_2, "Boxplots de las variables numéricas con rango fuera de [0,1]", colores_2)

# Duration_ms en un boxplot aparte porque tenia otro rango 
ggplot(datosFiltrados, aes(y = duration_ms)) +
  geom_boxplot(fill = "#0B5015",size = 0.7) +
  labs(title = "Boxplot de duration_ms", x = "Variable", y = "Valor") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))+
  scale_y_continuous(labels = comma_format())

# Matriz de correlaciones
datosNumericos <- datosFiltrados %>% select(popularity, duration_ms, danceability,
                                            energy, loudness, speechiness,
                                            acousticness, instrumentalness, liveness, 
                                            valence, tempo)
M1 <- cor(datosNumericos)
ggplot(data = as.data.frame(as.table(M1)),
       aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), vjust = 1) +
  scale_fill_gradient2(low = "#0B5015", mid = "white", high = "green3",
                       midpoint = 0) +
  theme_minimal() +
  coord_fixed() +
  labs(title = "Matriz de Correlación") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), 
        axis.text.y = element_text(angle = 45, hjust= 1, face = "bold"))

# Analizando a Explicit en relacion a Energy y Loudness
ggplot(datosFiltrados, aes(x = loudness, y = energy, color = explicit)) +
  geom_point(alpha = 0.3) +
  labs(x = "Loudness", y = "Energy", title = "Relación entre Energy, Loudness y Explicit") +
  scale_color_manual(values = c("green3", "black"), labels = c("No Explicit", "Explicit")) +
  geom_smooth() +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.x = element_text(hjust = 1, face = "bold"), 
        axis.text.y = element_text(hjust= 1, face = "bold"))

# Analizando a Explicit en relacion a Tempo y Loudness
ggplot(datosFiltrados, aes(x = tempo, y = loudness, color = explicit)) +
  geom_point(alpha = 0.3) +
  labs(x = "Tempo", y = "Loudness", title = "Relación entre Tempo, Loudness y Explicit") +
  scale_color_manual(values = c("green3", "black"), labels = c("No Explicit", "Explicit")) +
  geom_smooth() +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.x = element_text(hjust = 1, face = "bold"), 
        axis.text.y = element_text(hjust= 1, face = "bold"))

#Grafico de barras de cantidad de canciones no explicitas y promedio de energy por género
generos_deseados <- c("metal", "techno", "sleep", "trance", "study", "reggae", "punk-rock", "jazz", "classical", "hip-hop")
df_filtrado <- subset(datosFiltrados, track_genre %in% generos_deseados)
df_filtrado$explicit_numeric <- as.numeric(df_filtrado$explicit == "False") # Crear una copia de la columna "explicit" como valores numéricos (0 para True, 1 para False) (hay generos sin explicitas)
canciones_explicitas_por_genero <- aggregate(explicit_numeric ~ track_genre, data = df_filtrado, FUN = sum) # Contar la cantidad de canciones "explicit" por género
promedio_energy_por_genero <- aggregate(energy ~ track_genre, data = df_filtrado, FUN = mean) # Calcular el promedio de "energy" por género
datos_combinados <- merge(canciones_explicitas_por_genero, promedio_energy_por_genero, by = "track_genre", all.x = TRUE) # Combinar los datos
datos_combinados$track_genre <- fct_reorder(datos_combinados$track_genre, datos_combinados$explicit_numeric, .fun = sum) # Ordenar los niveles del factor "track_genre" por "explicit_numeric"

ggplot(datos_combinados, aes(x = track_genre, na.rm=TRUE, y = explicit_numeric, fill = energy)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "light green", high = "dark green") +
  labs(x = "Género de Música", y = "Cantidad de Canciones No Explicitas", fill = "Promedio de Energy") +
  ggtitle("Cantidad de Canciones No Explicitas por Género de Música") +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"), 
        axis.text.y = element_text(hjust= 1, face = "bold"))

#Pairs para ver la correlación entre variables
pairs(datosNumericos, pch = 16)



