
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(factoextra)
library(ISLR)

# PCA na Iris -------------------------------------------------------------

iris_sem_species <- iris[,-5]

PCA_iris <- princomp(iris_sem_species)

plot(PCA_iris$scores[,1:2])

fviz(PCA_iris, element = "ind")

A_do_iris <- t(PCA_iris$loadings)

fviz(PCA_iris, element = "var")

A_do_iris[,3]
A_do_iris[,4]

# PCA no Hitters ----------------------------------------------------------

View(Hitters)
# vem do pacote ISLR

dados_batedores <- Hitters |>
  drop_na() |>
  select(-League, -Division, -NewLeague) |>
  mutate(
    across(
      .fns = function(x){(x-mean(x))/sd(x)}
    )
  )

princomp(dados_batedores) |>
  fviz("var")

princomp(dados_batedores) |>
  fviz("ind")

princomp(dados_batedores) |>
  plot()

PCA_batedores <- princomp(dados_batedores)

cumsum(PCA_batedores$sdev^2)/sum(PCA_batedores$sdev^2)

fviz_cluster(kmeans(dados_batedores, centers = 3), data = dados_batedores)
fviz_cluster(hcut(dados_batedores, k = 3), data = dados_batedores)

# PCA na matriz de distancias ---------------------------------------------
# escalonamento multidimensional clássico/simples

matriz_de_distancias_batedores <- dados_batedores |>
  dist(method = "euclidian") |>
  as.matrix()

pca_matriz_de_distancias <- princomp(matriz_de_distancias_batedores)

fviz(pca_matriz_de_distancias, "ind")

dados_corredores <- factoextra::decathlon2 |>
  drop_na() |>
  select_if(is.numeric) |>
  mutate(
    across(
      .fns = function(x){(x-mean(x))/sd(x)}
    )
  )

dados_corredores |>
  princomp() |>
  plot()

dados_corredores |>
  princomp() |>
  fviz("ind")

dados_corredores |>
  princomp() |>
  fviz("var")

dados_corredores |>
  princomp() |>
  biplot()

dados_corredores |>
  dist() |>
  as.matrix() |>
  princomp() |>
  fviz("ind")
