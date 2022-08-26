
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(tidymodels)
library(cluster)
library(factoextra)
# pacotes que vamos usar mais ao longo do curso

# Modelo ------------------------------------------------------------------

dados_modelo <- iris |>
  select(-Species)

modelo_k_medias <- kmeans(dados_modelo, centers = 3)

base_final <- modelo_k_medias |>
  augment(iris)

base_final |>
  ggplot(aes(x = Petal.Length, y = Petal.Width, color = .cluster)) +
  geom_point()

