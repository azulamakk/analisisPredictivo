library(tidyverse)
library(ggplot2)

datos <- read.csv("~/Universidad/Datasets/Predictiva/dataset.csv")

# Vista preliminar
datos %>% head(5) %>% View()
summary(datos)
glimpse(datos)

# Extraccion de duplicados
spotify <- datos %>% group_by(track_id) %>% summarise(cant = n()) %>% filter(cant > 1)

# Chequeo de NAs
colnames(spotify)[apply(spotify, 2, anyNA)] # No hay columnas con missings


