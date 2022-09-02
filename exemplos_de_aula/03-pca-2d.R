
# Pacotes -----------------------------------------------------------------

library(tidyverse)
library(ggfortify)


# PCA-2d ------------------------------------------------------------------

set.seed(01092022)

Xx <- rnorm(1000, sd = 1)

X <- tibble(
  Xx,
  Y = Xx + rnorm(length(Xx), sd = .5)
)

PCA <- princomp(X)
# outra opção prcomp

mu <- PCA$center
# mu dos slides

Y <- PCA$scores
# Y dos slides

A <- t(PCA$loadings)
# A dos slides


# Y e A aproximam X -------------------------------------------------------

X_aprox = set_names(as_tibble(Y %*% A + mu), c("X", "Y"))

X_aprox[1,]
X[1,]

X_aprox[2,]
X[2,]

X_aprox[3,]
X[3,]

# O que é a matriz A? -----------------------------------------------------

plot(X)
points(matrix(mu, nrow = 1), col = "blue", lwd = 10)
points(
  matrix(A[,1], nrow = 1),
  col = 'red', lwd = 10
)
points(
  matrix(A[,2], nrow = 1),
  col = 'green', lwd = 10
)
lines(
  rbind(
    mu,
    A[,1]
  ), col = 'red', lwd = 10
)
lines(
  rbind(
    mu,
    A[,2]
  ), col = 'green', lwd = 10
)

# Aproximação de dimensão menor -------------------------------------------

Y_1d = matrix(Y[,1], ncol = 1)

X_aprox_1d = as_tibble(Y_1d %*% A[1,] + mu) |>
  set_names(c("X", "Y"))

X[1,]
X_aprox_1d[1,]

plot(X)
points(matrix(mu, nrow = 1), col = "blue", lwd = 10)
points(
  matrix(A[,1], nrow = 1),
  col = 'red', lwd = 10
)
points(
  matrix(A[,2], nrow = 1),
  col = 'green', lwd = 10
)
lines(
  rbind(
    mu,
    A[,1]
  ), col = 'red', lwd = 10
)
lines(
  rbind(
    mu,
    A[,2]
  ), col = 'green', lwd = 10
)
points(X_aprox_1d, col = "red", lwd = 10)

# Erro --------------------------------------------------------------------

# isso está sendo minimizado:
sqrt(sum((X-X_aprox_1d)^2)/(2000))

var(X)
var(X_aprox_1d)

(0.8873892+1.146773)/(0.9546543+1.1988115)

(var(X_aprox_1d[,1]) + var(X_aprox_1d[,2]))/(var(X[,1]) + var(X[,2]))

plot(PCA)

PCA$sdev

