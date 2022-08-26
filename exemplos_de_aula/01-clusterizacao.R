# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(factoextra)

# Matriz de distâncias ----------------------------------------------------

iris_tibble <- tibble(iris) |>
  select(-Species)

objeto_distancias <- dist(iris_tibble)
distancias_manhattan <- dist(iris_tibble, method = "manhattan")
distancias_max <- dist(iris_tibble, method = "maximum")

matriz_de_distancias <- as.matrix(objeto_distancias)
matriz_dist_manhattan <- as.matrix(distancias_manhattan)
matriz_dist_max <- as.matrix(distancias_max)

matriz_de_distancias2 <- as.matrix(dist(iris))
# cuidado, pode impactar sua conta!


# observação que o hclust espera receber objeto que sai do dist

plot(hclust(objeto_distancias))
# funciona, pq o hclust espera receber um objeto de distancias

plot(hclust(matriz_de_distancias))
# não funciona

# criando hclusts

dendro_complete_euclid <- hclust(objeto_distancias)
dendro_complete_manht <- hclust(distancias_manhattan)
dendro_complete_max <- hclust(distancias_max)

dendro_median_euclid <- hclust(objeto_distancias, method = "median")
dendro_median_manht <- hclust(distancias_manhattan, method = "median")
dendro_median_max <- hclust(distancias_max, method = "median")

plot(dendro_median_euclid)
plot(dendro_complete_euclid)

alturas_ao_contrario_c_e <- rev(dendro_complete_euclid$height)
alturas_ao_contrario_m_e <- rev(dendro_median_euclid$height)

barplot(alturas_ao_contrario_c_e[1:10])
barplot(alturas_ao_contrario_m_e[1:10])

alturas_ao_contrario_c_m <- rev(dendro_complete_manht$height)
alturas_ao_contrario_m_m <- rev(dendro_median_manht$height)

barplot(alturas_ao_contrario_c_m[1:10])
barplot(alturas_ao_contrario_m_m[1:10])

# Outro jeito de visualizar distancias ------------------------------------

distancia_alternativa <- get_dist(iris_tibble)

fviz_dist(distancia_alternativa)

fviz_dist(dist(USArrests))

dados::pinguins |>
  select(comprimento_bico, profundidade_bico, comprimento_nadadeira, massa_corporal) |>
  drop_na() |>
  dist() |>
  fviz_dist()

# Dendrograma com ggplot2 -------------------------------------------------

library(ggdendro)

ggdendrogram(hclust(objeto_distancias))

plot(hclust(objeto_distancias))
rect.hclust(hclust(objeto_distancias), k = 3)

# hclust do USArrests -----------------------------------------------------

us_dist_euclid <- dist(USArrests)
us_dist_manht <- dist(USArrests, method = "manhattan")

us_dendro_e <- hclust(us_dist_euclid)
us_dendro_m <- hclust(us_dist_manht)
us_dendro_m_m <- hclust(us_dist_manht, method = "median")

alturas_ao_contrario_e <- rev(us_dendro_e$height)
alturas_ao_contrario_m <- rev(us_dendro_m$height)
alturas_ao_contrario_m_m <- rev(us_dendro_m_m$height)

barplot(alturas_ao_contrario_e[1:10])
barplot(alturas_ao_contrario_m[1:10])
barplot(alturas_ao_contrario_m_m[1:10])
