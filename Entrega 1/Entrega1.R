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

#Limpieza de datos
datos <- read.csv("~/Universidad/Datasets/Predictiva/dataset.csv")
datosFiltrados <- datos %>% filter(track_genre == 'classical'|track_genre == 'metal' | track_genre == 'jazz' | track_genre == 'punk-rock'
                                   | track_genre == 'techno' | track_genre == 'reggae' | track_genre == 'sleep'
                                   | track_genre == 'trance' | track_genre == 'study' | track_genre == 'hip-hop') 
# Vista preliminar
summary(datosFiltrados)
datosNumericos <- datosFiltrados %>% select(popularity, duration_ms, danceability,
                                            energy, loudness, speechiness,
                                            acousticness, instrumentalness, liveness,
                                            valence, tempo)

# Chequeo de NAs
colnames(datosFiltrados)[apply(datosFiltrados, 2, anyNA)] # No hay columnas con missings

#Graficos de densidad de las variables numericas
plots <- lapply(names(datosNumericos), function(colname) { # Crea gráficos de densidad 
  ggplot(data = datosNumericos, aes_string(x = colname)) +
    geom_density(fill = "green3", alpha = 0.5) +
    labs(title = paste("Gráfico de Densidad de", colname)) +
    theme(text = element_text(family = "mono"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey'))
})
grid.arrange(grobs = plots, ncol = 2) # Organiza los gráficos en una cuadrícula utilizando gridExtra

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

#Pie chart para ver la cantidad de explicit-vs-no explicit 
datosFiltrados$explicit_numeric <- as.numeric(datosFiltrados$explicit == "True") # Crear una copia de la columna "explicit" como valores numericos (1 para True, 0 para False) (hay generos sin explicitas)
resumen_explicit <- data.frame(
  Categoria = c("Explicit", "No explicit"),
  Frecuencia = c(sum(datosFiltrados$explicit_numeric == 1), sum(datosFiltrados$explicit_numeric == 0))
)
total <- sum(resumen_explicit$Frecuencia) # Calcular los porcentajes
resumen_explicit$Porcentaje <- (resumen_explicit$Frecuencia / total) * 100

ggplot(resumen_explicit, aes(x = "", y = Frecuencia, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste(round(Porcentaje, 2), "%")), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("darkgreen", "lightgreen")) + 
  labs(title = "Proporción de canciones explícitas y no explícitas")+
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))

#Comparacion de medias de explicit-noexplicit en las variables numericas continuas
canciones_explicitas <- datosFiltrados[datosFiltrados$explicit == "True", ] # Filtrar las canciones explicitas
canciones_no_explicitas <- datosFiltrados[datosFiltrados$explicit == "False", ] #Filtrar canciones no explicitas
#energy
t.test(canciones_explicitas$energy, canciones_no_explicitas$energy)
#tempo
t.test(canciones_explicitas$tempo, canciones_no_explicitas$tempo)
#loudness
t.test(canciones_explicitas$loudness, canciones_no_explicitas$loudness)
#popularity
t.test(canciones_explicitas$popularity, canciones_no_explicitas$popularity)
#speechiness
t.test(canciones_explicitas$speechiness, canciones_no_explicitas$speechiness)
#duration
t.test(canciones_explicitas$duration_ms, canciones_no_explicitas$duration_ms)
#liveness
t.test(canciones_explicitas$liveness, canciones_no_explicitas$liveness)
#valence
t.test(canciones_explicitas$valence, canciones_no_explicitas$valence)
#acousticness
t.test(canciones_explicitas$acousticness, canciones_no_explicitas$acousticness)
#danceability
t.test(canciones_explicitas$danceability, canciones_no_explicitas$danceability)
#instrumentalness
t.test(canciones_explicitas$instrumentalness, canciones_no_explicitas$instrumentalness)

# Analizando a Explicit en relacion a Energy y Loudness
ggplot(datosFiltrados, aes(x = loudness, y = energy, color = explicit)) +
  geom_point(alpha = 0.3) +
  labs(x = "Loudness", y = "Energy", title = "Relación entre Energy, Loudness y Explicit") +
  scale_color_manual(values = c("green3", "black"), labels = c("No Explicit", "Explicit")) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
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
  geom_smooth(aes(group = explicit), method = "lm", se = FALSE) +
  theme(text = element_text(family = "mono"),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        panel.background = element_rect(fill = '#F0F0F0', color = 'grey'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        axis.text.x = element_text(hjust = 1, face = "bold"), 
        axis.text.y = element_text(hjust= 1, face = "bold"))

#Calcular valores de V de Cramer
#explicit-genero
tabla_contingencia <- table(datosFiltrados$track_genre, datosFiltrados$explicit) # Crear una tabla de contingencia entre las variables track_genre y explicit
resultado_cramer <- assocstats(tabla_contingencia) # Calcular el coeficiente V de Cramer
sqrt(resultado_cramer$chisq / (sum(tabla_contingencia) * (min(nrow(tabla_contingencia), ncol(tabla_contingencia)) - 1)))

#explicit-key
tabla_contingenciaII <- table(datosFiltrados$key, datosFiltrados$explicit) # Crear una tabla de contingencia entre las variables key y explicit
resultado_cramerII <- assocstats(tabla_contingenciaII)
sqrt(resultado_cramerII$chisq / (sum(tabla_contingenciaII) * (min(nrow(tabla_contingenciaII), ncol(tabla_contingenciaII)) - 1)))

#explicit-mode
tabla_contingenciaIII <- table(datosFiltrados$mode, datosFiltrados$explicit)# Crear una tabla de contingencia entre las variables key y explicit
resultado_cramerIII <- assocstats(tabla_contingenciaIII)
sqrt(resultado_cramerIII$chisq / (sum(tabla_contingenciaIII) * (min(nrow(tabla_contingenciaIII), ncol(tabla_contingenciaIII)) - 1)))

#explicit-time signature
tabla_contingenciaIV <- table(datosFiltrados$time_signature, datosFiltrados$explicit) # Crear una tabla de contingencia entre las variables key y explicit
resultado_cramerIV <- assocstats(tabla_contingenciaIV)
sqrt(resultado_cramerIV$chisq / (sum(tabla_contingenciaIV) * (min(nrow(tabla_contingenciaIV), ncol(tabla_contingenciaIV)) - 1)))


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

# Clustering
# Pre-Procesamiento
# funciones (udf) para normalizar
minmax = function(x) (x - min(x)) / (max(x) - min(x))
rob_scale = function(x) (x - median(x)) / IQR(x)
id_cols = c("X","track_id","artists","album_name","track_name", 'mode', "key","time_signature",
            'explicit', 'track_genre')

# data numerica normalizada
dat_c = datosFiltrados %>% 
  select_if(is.numeric) %>% 
  select("popularity","duration_ms","danceability","energy","loudness","speechiness","acousticness",   
         "instrumentalness","liveness","valence","tempo", 'duration_ms') %>%
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

# Punto de quiebre
fviz_nbclust(dat_c, FUNcluster=hcut, method="wss", k.max=10
             ,diss=dist(dat_c, method="manhattan"), hc_method="average") 

# Silhouette
fviz_nbclust(dat_c, FUNcluster=hcut, method="silhouette", k.max=10
             ,diss=dist(dat_c, method="manhattan"), hc_method="average") 

# La cantidad optima de clusters es 8
# Analisis de resultados
hc = hcut(dat_c, k=8, hc_method="average", hc_metric="manhattan", stand=F)
plt = fviz_silhouette(hc, label=T, print.summary=F) +
  theme(axis.text.x=element_text(angle=-90, size=4))
print( plt )

# Distribuciones de Clusters
# data con variables originales
dat_hc = datosFiltrados %>%
  mutate(cluster = factor(hc$cluster))

# variables normalizadas
dat_c_hc = dat_c %>%
  bind_cols(datosFiltrados %>% select(all_of(id_cols))) %>% 
  mutate(cluster = factor(hc$cluster))

# long data.frame con variables normalizadas
gdat = dat_c_hc %>% 
  pivot_longer(
    -all_of(c(id_cols, "cluster")), names_to="variable", values_to="value")

# densidades por variable
plt_density <- ggplot(gdat, aes(x = value, y = variable, color = cluster, point_color = cluster, fill = cluster)) +
  geom_density_ridges(
    alpha = 0.5, scale = 1,
    jittered_points = TRUE,
    position = position_jitter(height = 0),
    point_shape = "|", point_size = 2,
    bandwidth = 0.1  # Specify the bandwidth value here
  ) +
  theme_minimal()

print(plt_density)