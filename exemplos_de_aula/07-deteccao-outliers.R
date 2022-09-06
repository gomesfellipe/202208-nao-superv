
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(ISLR)
library(ANN2)

# Deteccao descritiva -----------------------------------------------------

mtcars |>
  mutate(
    outlier_wt = abs(wt-mean(wt))/sd(wt) > 3
  ) |>
  ggplot(aes(x = mpg,  y = wt, color = outlier_wt)) +
  geom_point(size = 3)

mtcars |>
  mutate(
    outlier_wt = abs(wt-median(wt))/median(abs(wt-median(wt))) > 3,
    outlier_mpg = abs(mpg-median(mpg))/median(abs(mpg-median(mpg))) > 3
  ) |>
  ggplot(aes(x = mpg,  y = wt, color = outlier_mpg)) +
  geom_point(size = 3)

plot(factoextra::multishapes)

# Detecção de outliers multivariada ---------------------------------------

Hitters_numero <- Hitters |>
  select_if(is.numeric) |>
  drop_na()

Hitters_padronizado <- Hitters_numero |>
  mutate(
    across(
      .fns = function(x){(x-mean(x))/sd(x)}
    )
  )

PCA <- princomp(Hitters_numero)

fviz(PCA, "ind")

Hitters_aproximado <- PCA$scores[, 1] %*% t(PCA$loadings[1,]) + PCA$center

autoencoder_hitters <- autoencoder(Hitters_numero, hidden.layers = 4,
                                   val.prop = 0.15)

Hitters_aproximado_ae <- predict(autoencoder_hitters, Hitters_numero)

erro_reconstrucao_ae <- sqrt(rowSums(Hitters_numero-Hitters_aproximado_ae)^2)
erro_reconstrucao_pca <- sqrt(rowSums((Hitters_numero-Hitters_aproximado)^2))

View(Hitters_numero)
View(Hitters_aproximado)

verifica_se_esta_fora_de_3_mad <- function(x){
  (x-median(x))/median(abs(x-median(x))) > 3
}

Hitters_numero |>
  tibble::rownames_to_column() |>
  as_tibble() |>
  mutate(
    erro_pca = erro_reconstrucao_pca,
    erro_ae = erro_reconstrucao_ae,
    outlier_pca = verifica_se_esta_fora_de_3_mad(erro_pca),
    outlier_ae = verifica_se_esta_fora_de_3_mad(erro_ae)
  ) |>
  mutate(
    comp1 = PCA$scores[,1],
    comp2 = PCA$scores[,2]
  ) |>
  #filter(outlier_pca)  |>
  ggplot(aes(x = comp1, comp2, color = outlier_ae)) +
  geom_point()

# autoencoder -------------------------------------------------------------

encode(autoencoder_hitters, Hitters_numero) |>
  plot()
# autoencoder é uma técnica de redução de dimensionalidade também!

autoencoder_hitters2 <- autoencoder(Hitters_numero, hidden.layers = 2,
                                   val.prop = 0.15)

autoencoder_hitters3 <- autoencoder(Hitters_numero, hidden.layers = 3,
                                   val.prop = 0.15)

autoencoder_hitters4 <- autoencoder(Hitters_numero, hidden.layers = 4,
                                   val.prop = 0.15)

autoencoder_hitters5 <- autoencoder(Hitters_numero, hidden.layers = 5,
                                    val.prop = 0.15)

# mclust e outilers -------------------------------------------------------

clusters_hitters_mlcust = Mclust(Hitters_numero)

mclust::dmvnorm(Hitters_numero[189,],
                mean = as.matrix(clusters_hitters_mlcust$parameters$mean[,1], nrow = 1),
                sigma = clusters_hitters_mlcust$parameters$variance$sigma[,,1])
