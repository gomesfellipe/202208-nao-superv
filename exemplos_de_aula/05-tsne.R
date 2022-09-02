
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(tsne)


# Dados -------------------------------------------------------------------


multishapes <- factoextra::multishapes |>
  tibble()

multishapes |>
  ggplot(aes(x = x, y = y, color = factor(shape))) +
  geom_point()

multishapes |>
  select(-3) |>
  kmeans(centers = 3) |>
  fviz_cluster(data = multishapes[,-3])

multishapes |>
  select(-3) |>
  hcut(k = 5, hc_method = "single") |>
  fviz_cluster(data = multishapes[,-3])

multishapes |>
  select(-3) |>
  hcut(k = 5) |>
  fviz_cluster(data = multishapes[,-3])

reducao_sne <- multishapes[,-3] |>
  tsne(k = 2, perplexity = 50)

reduzido = reducao_sne |>
  as_tibble() |>
  set_names(c("x", "y")) |>
  mutate(shape = multishapes$shape)

reduzido |>
  ggplot(aes(x = x, y = y, color = factor(shape))) +
  geom_point()

reduzido[,-3] |>
  #kmeans(centers = 5) |>
  hcut(k = 5, hc_method = "single") |>
  fviz_cluster(data = reduzido[,-3])

