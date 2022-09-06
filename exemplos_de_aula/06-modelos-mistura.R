
# Pacotes -----------------------------------------------------------------

library(tidyverse)

library(mclust)
library(factoextra)
# melhor para mistura de normais e integra com o factoextra

library(mixtools)
library(mixR)
library(mixture)
# dão mais possibilidades além da normal


# Primeiros modelos -------------------------------------------------------

hist(faithful[,1])

modelo_mistura_faithful <- Mclust(faithful[,1])

summary(modelo_mistura_faithful)

# fviz_nbclust(as.data.frame(faithful[,1]), hcut, method = "gap_stat")
# aqui paramos no 2!

modelo_mistura_faithful$parameters

modelo_mistura_faithful$z |> View()

modelo_mistura_faithful_ve <- Mclust(faithful[,1], modelNames = "E")

summary(modelo_mistura_faithful_ve)
summary(modelo_mistura_faithful)

plot(modelo_mistura_faithful, "BIC")

# Mistura 2d --------------------------------------------------------------

modelo_iris_2d <- Mclust(iris[,1:2])

summary(modelo_iris_2d)

plot(modelo_iris_2d)

modelo_iris_2d$modelName

modelo_iris_2d <- Mclust(iris[,1:2])

summary(modelo_iris_2d)

plot(modelo_iris_2d, "BIC")
plot(modelo_iris_2d, "classification")

# Mistura 4d --------------------------------------------------------------

modelo_iris_4d <- Mclust(iris[,1:4])

summary(modelo_iris_4d)

plot(modelo_iris_4d)

modelo_iris_4d$modelName

modelo_iris_4d <- Mclust(iris[,1:4])

summary(modelo_iris_4d)

plot(modelo_iris_4d, "BIC")
plot(modelo_iris_4d, "classification")

# mclust + factoextra -----------------------------------------------------

Mclust(iris[,1:4], 3) |>
  fviz_cluster()

# Misturas gaussianas acertam ---------------------------------------------

set.seed(05092022)

Mclust(rnorm(10000)) |>
  plot("BIC")

fviz_nbclust(as.matrix(rnorm(10000)), hcut, method = "gap_stat")

mistura_gama <- mixR::mixfit(faithful[,1], ncomp = 3, family = "gamma")
