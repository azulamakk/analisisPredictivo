library(tidyverse)
library(ggplot2)

datos <- read.csv("~/Universidad/Analisis Predictivo/Entrega 1/avocado 2.csv")

# Dentro del dataset, nos enfocaremos unicamente en las ventas del año 2017
datosFiltrados <- datos %>% filter(year == 2017)

# Una vez obtenidos los datos filtrados, realizamos un sample de 300 registros
set.seed(13) 
avocadoSample <- datosFiltrados[sample(nrow(datosFiltrados), size = 300, replace = FALSE), ]

avocadoSample %>% head(5) %>% View()
summary(avocadoSample)


# Estudio de outliers
colnames(datosFiltrados)

ggplot(datosFiltrados, aes(y = AveragePrice, x = "")) +
  geom_boxplot() +
  labs(title = "Box Plot de Precio Promedio")

ggplot(datosFiltrados, aes(y = Total.Volume, x = "")) +
  geom_boxplot() +
  labs(title = "Box Plot de Volumen Total")

# En funcion al tipo de palta
dataPLU <- datosFiltrados %>%
  gather(key = "PLU", value = "Cantidad", X4046, X4225, X4770)

ggplot(dataPLU, aes(x = PLU, y = Cantidad, fill = PLU)) +
  geom_boxplot() +
  labs(title = "Número Total de Paltas con Distintos PLU Vendidas") +
  xlab("PLU") + ylab("Cantidad") +
  scale_fill_manual(values = c("X4046" = "red", "X4225" = "green", "X4770" = "blue")) +
  theme_minimal() +
  theme(legend.position = "none")

# En funcion a tamaño de bolsa
datosBolsas <- datosFiltrados %>%
  gather(key = "BagType", value = "Cantidad", Total.Bags, Small.Bags, Large.Bags, XLarge.Bags)

ggplot(datosBolsas, aes(x = BagType, y = Cantidad, fill = BagType)) +
  geom_boxplot() +
  labs(title = "Cantidad Total de Bolsas de Distintos Tipos") +
  xlab("Tipo de Bolsa") + ylab("Cantidad") +
  scale_fill_manual(values = c("Total.Bags" = "red", "Small.Bags" = "green", "Large.Bags" = "blue", "XLarge.Bags" = "purple")) +
  theme_minimal() +
  theme(legend.position = "none")