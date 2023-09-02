library(tidyverse)
library(ggplot2)
library(corrplot)
library(scales)
library(glue)
library(GGally)
library(ggridges)
library(plotly)
library(factoextra)
library(amap)
library(sf)
library(igraph)
library(heatmaply)

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
#pairs(datosNumericos, pch = 16)




# Clustering

# Pre-Procesamiento
# funciones (udf) para normalizar
minmax = function(x) (x - min(x)) / (max(x) - min(x))
rob_scale = function(x) (x - median(x)) / IQR(x)
id_cols = c("X","track_id","artists","album_name","track_name", 'mode', "key","time_signature",
            'explicit', 'track_genre', 'duration_ms')
colnames(datosFiltrados)

# data numerica normalizada
dat_c = datosFiltrados %>% 
  select_if(is.numeric) %>% 
  select("popularity","duration_ms","danceability","energy","loudness","speechiness","acousticness",   
         "instrumentalness","liveness","valence","tempo") %>%
  # scale(center=T, scale=T) %>% # estandarizacion media-desvio
  mutate_all(rob_scale) %>% # normalizacion
  as.data.frame() # las funciones de cluster se llevan mejor con data.frame (admite row.names)

# Analisis exploratorio
# formato long con datos normalizados
colnames(datosFiltrados)
gdat = datosFiltrados %>%
  mutate_if(is.numeric, rob_scale) %>% 
  pivot_longer(
    cols = -all_of(id_cols), # Selecciona todas las columnas numéricas excepto las de id_cols
    names_to = "variable",
    values_to = "value"
  )

# plot de coordenadas paralelas
plt = ggplot(gdat, aes(x = variable, y = value, group = track_genre)) +
  geom_line(alpha = 0.3) +
  geom_line(data = filter(gdat, track_genre == "hip-hop"), color = "green3", alpha = 0.7) +
  theme_minimal()

# plot interactivo
ggplotly(plt, width=860, height=500)

# matriz de distancias
dist_obj = dist(dat_c, method="manhattan")
dist_matrix = as.matrix(dist_obj)
# nombres de filas y columnas
dimnames(dist_matrix) = list(genero1=datosFiltrados$track_genre, genero2=datosFiltrados$track_genre)
# de matriz a data.frame long con un atajo
dist_df = as.data.frame(as.table(dist_matrix)) %>%
  rename(dist = Freq) 

# distancia mediana de cada genero vs el resto
gdat = dist_df %>% 
  group_by(genero1) %>%
  summarise(median_dist = median(dist))
# plot de las medianas ordenadas
plt = 
  ggplot(gdat, aes(x=reorder(genero1, median_dist), y=median_dist)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x=element_blank())

ggplotly(plt, width=860, height=450) 

# Clustering Jerarquico
# Calcular unique(datosFiltrados$track_genre) y hc$order una vez
hc = amap::hcluster(dat_c, method="manhattan", link="average")
unique_track_genre <- unique(datosFiltrados$track_genre)
order_hc <- hc$order

# Crear un nuevo dataframe con las columnas genero1 y genero2 modificadas
# Asegurarse de que los niveles sean únicos
unique_track_genre_order <- unique(datosFiltrados$track_genre[hc$order])

# Crear los factores con niveles únicos
gdat = dist_df %>% 
  mutate(
    genero1 = factor(genero1, levels = unique_track_genre_order),
    genero2 = factor(genero2, levels = unique_track_genre_order)
  )

dist_df <- as.data.frame(dist_matrix)
colnames(dist_matrix) <- dist_df$genero2


# Punto de quiebre
fviz_nbclust(dat_c, FUNcluster=hcut, method="wss", k.max=10
             ,diss=dist(dat_c, method="manhattan"), hc_method="average") 

# Silhouette
fviz_nbclust(dat_c, FUNcluster=hcut, method="silhouette", k.max=10
             ,diss=dist(dat_c, method="manhattan"), hc_method="average") 

# La cantidad optima de clusters es 8
fviz_dend(hc, horiz=T, k=8, repel=T) 

fviz_dend(hc, type="phylogenic", k=8, repel=T) 

# Densidades por cluster
# Data con variables originales
dat_hc = datosFiltrados %>%
  mutate(cluster = factor(hc$cluster))

# indicamos "outliers"
outlier_countries = dat_hc %>% 
  group_by(cluster) %>% 
  filter(n() == 1) %>% 
  pull(track_genre)

# variables normalizadas
dat_c_hc = dat_c %>%
  bind_cols(dat %>% select(all_of(id_cols))) %>% 
  mutate(cluster = factor(hc$cluster))


