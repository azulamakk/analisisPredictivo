# Para data.csv calcular todos los epsilon.sq posibles 

library(tidyverse)

data <- read.csv("~/Universidad/Analisis Predictivo/Clases/Clase 3/data.csv")


table(data$v1)

names_num <- data %>% 
  select_if(function(x) is.numeric(x) & length(unique(x)) >2) %>% 
  names()

names_cat <- data %>% 
  select_if(function(x) !is.numeric(x)) %>%
  names()

data %>% select(names_num)
data %>% select(names_cat)

df_epsilon <- expand_grid(v1 = names_num, v2 = names_cat) # Combina cada numerica con cada categorica

epsilonsq <- function(name_cat, name_num, data){
  form = paste0(name_num, '~', names_cat) # Concatenamos strings
  res_kruskal = rstatix::kruskal_test(data, form)
  epsilon_sq = res_kruskal$statistic / 
    ((res_kruskal$n**2 - 1) / (res_kruskal$n + 1))
  return(epsilon_sq)
}

epsilons <- c()
for (i in 1:nrow(df_epsilon)){
  df_tmp <- df_epsilon[i, ]
  epsilons[i] <- epsilonsq(df_tmp[2], df_tmp[1], data)
}

df_epsilon = df_epsilon %>% mutate(eps_sq = epsilons)

df_epsilon %>% arrange(-eps_sq)