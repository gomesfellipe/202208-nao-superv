# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(factoextra)

# kmeans ------------------------------------------------------------------

dados_k_medias <- iris |>
  tibble() |>
  select(-Species)
  #select(Sepal.Length, Sepal.Width)

modelo_k_medias <- dados_k_medias |>
  kmeans(centers = 3)

dados_k_medias |>
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  geom_point(aes(x = 5.006, y = 3.428), color = "red", size = 10)+
  geom_point(aes(x = 5.901, y = 2.748), color = "red", size = 10)+
  geom_point(aes(x = 6.85, y = 3.074), color = "red", size = 10)
# visualizando os pontos ^

modelo_k_medias$withinss
# SSW no slide. 1 pra cada grupo (ou seja, k)

modelo_k_medias$tot.withinss
# soma dos SSW anteriores: sum(modelo_k_medias$withinss)

modelo_k_medias$betweenss

modelo_k_medias$totss

# Escolher o melhor numero de clusters ------------------------------------

todos_os_twss <- c(
  (dados_k_medias |> kmeans(centers = 1, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 2, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 3, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 4, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 5, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 6, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 7, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 8, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 9, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 10, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 11, iter.max = 400))$tot.withinss,
  (dados_k_medias |> kmeans(centers = 12, iter.max = 400))$tot.withinss
)

qplot(1:12, todos_os_twss, geom = "line")

# Método menos manual -----------------------------------------------------

# crash course de factorextra

# fviz é uma função pra visualizar objetos de análises não supervisionadas específicas

# fviz_?? são funções especializadas:

fviz_nbclust(dados_k_medias, kmeans, method = "wss")

fviz_nbclust(dados_k_medias, hcut, method = "wss")

# aqui concluímos que é tudo parecido!

fviz_nbclust(USArrests, kmeans, method = "wss")

fviz_nbclust(USArrests, hcut, method = "wss")

# Pegar os grupos ---------------------------------------------------------

dados_com_clusters <- modelo_k_medias |>
  augment(dados_k_medias)


# Dados do circulo --------------------------------------------------------

dados_circulo <- tibble(
  X = runif(5000, -1, 1),
  Y = runif(5000, -1, 1)
) |>
  filter(X^2 + Y^2 <= 0.2 | (X^2 + Y^2 <= 0.8 & X^2 + Y^2 >= 0.6))

qplot(dados_circulo$X, dados_circulo$Y)

fviz_nbclust(dados_circulo, kmeans, method = "wss")
fviz_nbclust(dados_circulo, hcut, method = "wss", hc_method = "single")

modelo_cluster_h = hcut(dados_circulo, k = 2, hc_method = "single")

dados_com_clusters_h <- dados_circulo |>
  mutate(
    .cluster = factor(modelo_cluster_h$cluster)
  )

dados_com_clusters_h |>
  ggplot(aes(x = X, y = Y, color = .cluster)) +
  geom_point()

dados_com_clusters_k <- dados_circulo |>
  mutate(
    .cluster = factor(kmeans(dados_circulo, centers = 2)$cluster)
  )

dados_com_clusters_h |>
  ggplot(aes(x = X, y = Y, color = .cluster)) +
  geom_point()

dados_com_clusters_k |>
  ggplot(aes(x = X, y = Y, color = .cluster)) +
  geom_point()

# Padronização ------------------------------------------------------------

padroniza <- function(x){(x-mean(x))/sd(x)}

dados_k_medias_padr <- dados_k_medias |>
  mutate(
    Sepal.Length = padroniza(Sepal.Length),
    Sepal.Width = padroniza(Sepal.Width),
    Petal.Length = padroniza(Petal.Length),
    Petal.Width = padroniza(Petal.Width)
  )

dados_k_medias |>
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()


dados_k_medias_padr |>
  ggplot(aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

USArrests_padr <- USArrests |>
  mutate(
    Murder = padroniza(Murder),
    Assault = padroniza(Assault),
    UrbanPop = padroniza(UrbanPop),
    Rape = padroniza(Rape)
  )

USArrests |>
  ggplot(aes(x = Rape, y = Assault)) +
  geom_point()


USArrests_padr |>
  ggplot(aes(x = Rape, y = Assault)) +
  geom_point()

fviz_nbclust(dados_k_medias, kmeans, method = "wss")
fviz_nbclust(dados_k_medias_padr, kmeans, method = "wss")

fviz_nbclust(USArrests, kmeans, method = "wss")
fviz_nbclust(USArrests_padr, kmeans, method = "wss")

# Seleção de numero de clusters -------------------------------------------

fviz_nbclust(dados_k_medias, kmeans, method = "gap_stat")

fviz_nbclust(dados_k_medias_padr, kmeans, method = "gap_stat")

fviz_nbclust(dados_circulo, kmeans, method = "gap_stat")

fviz_nbclust(dados_circulo, hcut, method = "gap_stat", hc_method = "single")

modelo_cluster_h = hcut(dados_circulo, k = 2, hc_method = "single")

dados_com_clusters_h <- dados_circulo |>
  mutate(
    .cluster = factor(modelo_cluster_h$cluster)
  )

dados_com_clusters_h |>
  ggplot(aes(x = X, y = Y, color = .cluster)) +
  geom_point()


# Silhueta ----------------------------------------------------------------


fviz_nbclust(dados_circulo, hcut, method = "sil", hc_method = "single")
# encontra 2 perfeitamente

fviz_nbclust(dados_circulo, kmeans, method = "sil")
# aqui manda quebrar em 9 partes

fviz_nbclust(dados_k_medias, hcut, method = "sil", hc_method = "single")

fviz_nbclust(dados_k_medias, kmeans, method = "sil")

fviz_nbclust(dados_k_medias_padr, hcut, method = "sil", hc_method = "single")

fviz_nbclust(dados_k_medias_padr, kmeans, method = "sil")

fviz_nbclust(USArrests, hcut, method = "sil")

fviz_nbclust(USArrests, kmeans, method = "sil")

fviz_nbclust(USArrests_padr, hcut, method = "sil")

fviz_nbclust(USArrests_padr, kmeans, method = "sil")

# Avaliando a importância das variáveis ----------------------------------

install.packages("randomForest")

library(randomForest)

glm_importancia_das_variaveis <- dados_com_clusters |>
  mutate(.cluster = factor(hcut(dados_k_medias, k = 3)$cluster)) |>
  randomForest(.cluster ~ ., data = _)

varImpPlot(glm_importancia_das_variaveis)

# note abaixo a importância da padronização:

glm_importancia_das_variaveis <- USArrests |>
  mutate(.cluster = factor(hcut(USArrests, k = 3)$cluster)) |>
  randomForest(.cluster ~ ., data = _)

varImpPlot(glm_importancia_das_variaveis)

glm_importancia_das_variaveis <- USArrests_padr |>
  mutate(.cluster = factor(hcut(USArrests_padr, k = 3)$cluster)) |>
  randomForest(.cluster ~ ., data = _)

varImpPlot(glm_importancia_das_variaveis)

fviz_nbclust(USArrests, hcut, method = "gap_stat")

fviz_nbclust(USArrests, kmeans, method = "gap_stat")

fviz_nbclust(USArrests_padr, hcut, method = "gap_stat")

fviz_nbclust(USArrests_padr, kmeans, method = "gap_stat")

# Spoiler da próxima aula! ------------------------------------------------

prcomp()

fviz_cluster(kmeans(dados_k_medias, centers = 3), data = dados_k_medias)

fviz_cluster(hcut(dados_k_medias, k = 3), data = dados_k_medias)

fviz_cluster(kmeans(USArrests, centers = 3), data = USArrests)

fviz_cluster(hcut(USArrests, k = 3), data = USArrests)

fviz_cluster(kmeans(USArrests_padr, centers = 3), data = USArrests_padr)

fviz_cluster(hcut(USArrests_padr, k = 3), data = USArrests_padr)

fviz_cluster(kmeans(dados_circulo, centers = 3), data = dados_circulo)

fviz_cluster(hcut(dados_circulo, k = 2, hc_method = "single"), data = dados_circulo)
