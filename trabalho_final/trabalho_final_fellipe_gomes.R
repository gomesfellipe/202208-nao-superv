# Importar dependencias
library(keras)
library(tidyverse)
library(patchwork)

# reprodutibilidade
set.seed(1)

# funcao equivalente a np.clip do python
clip <- function(x, lower, upper) {
  pmax(pmin(x, upper), lower)
}

# tema para graficos
theme_set(theme_classic()+theme(legend.position = 'bottom'))

# Objetivos ---------------------------------------------------------------

# Neste script aplicaremos o método de auto-encoder desenvolvido com keras
# como método de redução de dimensionalidade, e detecção de outliers.
# Para clusterização utilizaremos kmeans

# Dados -------------------------------------------------------------------

# Carregar dados
mnist   <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test  <- mnist$test$x
y_test  <- mnist$test$y

# Normalizar dados no intervalo [0,1]
x_train <- x_train / 255
x_test <-  x_test / 255

# adicionar ruido as imagens
noise_factor = 0.2
x_train_noisy = x_train + noise_factor * array( rnorm(60000*28*28,mean=0,sd=1), c(60000, 28, 28))
x_test_noisy = x_test + noise_factor * array( rnorm(10000*28*28,mean=0,sd=1), c(10000, 28, 28))

# garantir intervalo [0,1]
x_train_noisy = clip(x_train_noisy, 0., 1.)
x_test_noisy = clip(x_test_noisy, 0., 1.)

# Ver 36 primeiros digitos da base
par(mfcol=c(6,6))
par(mar=c(0, 0, 3, 0), xaxs='i', yaxs='i')
for (idx in 1:36) {
  im <- x_train_noisy[idx,,]
  im <- t(apply(im, 2, rev))
  image(1:28, 1:28, im, col=gray((0:255)/255),
        xaxt='n', main=paste(y_train[idx]))
}

# reshape para formato tabular
x_train <- array_reshape(x_train, c(nrow(x_train), 28*28))
x_train_noisy <- array_reshape(x_train_noisy, c(nrow(x_train_noisy), 28*28))
x_test <- array_reshape(x_test, c(nrow(x_test), 28*28))
x_test_noisy <- array_reshape(x_test_noisy, c(nrow(x_test_noisy), 28*28))

# Construir um modelo "Deep autoencoder" ----------------------------------

# Definir modelo que sera utilizado
model <- keras_model_sequential()

model %>%
  layer_dense(units = 128, activation = "relu", input_shape =  c(784)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 2, activation = "linear", name = "bottleneck") %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 28*28, activation = "sigmoid")

# Instanciar modelo
model %>%
  compile(loss = "binary_crossentropy",
          optimizer = optimizer_adam())

summary(model)

# Treinar modelo
# (ja foi treinado em GPU e salvo localmente, nao preisa rodar)
# tictoc::tic()
# history <- model %>%
#   fit(x = x_train_noisy, y = x_train_noisy,
#       epochs = 100,
#       batch_size = 256,
#       shuffle=T,
#       validation_split =.2,
#       view_metrics = TRUE,
#       callbacks=list(callback_early_stopping(
#         monitor = "val_loss",
#         min_delta = 0.01,
#         patience = 50,
#         restore_best_weights = TRUE
#       )),
#       verbose=2)
# tictoc::toc()
# 144.195 sec na GPU do kaggle

# Carregar modelo treinado e seus resultados
model <- load_model_tf("model_ae", compile=F )
history <- readRDS("history_ae.rds")

# Historico de treinamento do modelo
as_tibble(history$metrics) %>%
  mutate(epochs = 1:nrow(.)) %>%
  gather(key, val, -epochs) %>%
  mutate(metric = case_when(
    str_detect(key, "accuracy") ~ "accuracy",
    str_detect(key, "loss") ~ "log_loss" )) %>%
  ggplot(aes(x = epochs, y = val, col=key)) +
  geom_point()+
  geom_smooth(se = F)+
  theme_bw()+
  facet_wrap(~metric, scales = "free_y")
# Parece que 50 epochs ja seria o suficiente.
# Talvez usar early stopping entre 30~50 pode ser uma opcao

# Reducao de dimensionalidade ---------------------------------------------

# Obter camada do "gargalo" do modelo
intermediate_layer_model <- keras_model(inputs = model$input,
                                        outputs = get_layer(model, "bottleneck")$output)

# Obter estimativas
intermediate_output <- predict(intermediate_layer_model, x_train_noisy)

# visualizar
ggplot(data.frame(PC1 = intermediate_output[,1],
                  PC2 = intermediate_output[,2]),
       aes(x = PC1, y = PC2, col = as.factor(y_train))) +
  geom_point(alpha=.5)+
  labs(color = "Target")
# Parece que a camada da rede responsavel por comprimir a informacao dos dados
# em duas dimensoes conseguiu diferenciar bem a maioria dos digitos
# com excecao para alguns como "7" e "2" e tambem "4" e "9", que sao bem parecidos
# talvez outra configuracao de rede, como convolucional com dropouts
#poderia mitigar esse problema.

# Detecao de outliers -----------------------------------------------------

# reconstrucao dos digitos estimados
x_test_pred <- predict(model, x_test_noisy)

# converter para array original para plot
x_test_pred <- array_reshape(x_test_pred, c(nrow(x_test_pred), c(28,28)))
x_test_noisy <- array_reshape(x_test_noisy, c(nrow(x_test_noisy), c(28,28)))

# Ver 36 primeiros digitos da base (COM RUIDO)
par(mfcol=c(6,6))
par(mar=c(0, 0, 3, 0), xaxs='i', yaxs='i')
for (idx in 1:36) {
  im <- x_test_noisy[idx,,]
  im <- t(apply(im, 2, rev))
  image(1:28, 1:28, im, col=gray((0:255)/255),
        xaxt='n', main=paste(y_test[idx]))
}

# Ver 36 primeiros digitos da base (SEM RUIDO)
par(mfcol=c(6,6))
par(mar=c(0, 0, 3, 0), xaxs='i', yaxs='i')
for (idx in 1:36) {
  im <- x_test_pred[idx,,]
  im <- t(apply(im, 2, rev))
  image(1:28, 1:28, im, col=gray((0:255)/255),
        xaxt='n', main=paste(y_test[idx]))
}

# Ver 36 primeiros digitos da base (Erro quadratico medio)
par(mfcol=c(6,6))
par(mar=c(0, 0, 3, 0), xaxs='i', yaxs='i')
for (idx in 1:36) {
  im <- sqrt((x_test_noisy[idx,,] - x_test_pred[idx,,])^2)
  im <- t(apply(im, 2, rev))
  image(1:28, 1:28, im, col=gray((0:255)/255),
        xaxt='n', main=paste(y_test[idx]))
}

# Ver 36 primeiros digitos da base (Outlier quando >3*MAD)
erro_reconstrucao_ae <- sqrt((x_test_noisy - x_test_pred)^2)

verifica_se_esta_fora_de_3_mad <- function(x){
  (x-median(x))/median(abs(x-median(x))) > 3
}

par(mfcol=c(6,6))
par(mar=c(0, 0, 3, 0), xaxs='i', yaxs='i')
for (idx in 1:36) {
  im <- sqrt((x_test_noisy[idx,,] - x_test_pred[idx,,])^2)
  im <- t(apply(im, 2, rev))
  image(1:28, 1:28, im, col=verifica_se_esta_fora_de_3_mad(erro_reconstrucao_ae)[idx,,],
        xaxt='n', main=paste(y_test[idx]))
}

# Clustering --------------------------------------------------------------

set.seed(1)

# reshape para formato tabular
x_test <- array_reshape(x_test, c(nrow(x_test), 28*28))
x_test_pred <- array_reshape(x_test_pred, c(nrow(x_test_pred), 28*28))
x_test_noisy <- array_reshape(x_test_noisy, c(nrow(x_test_noisy), 28*28))

# Reducao de dimensionalidade nos dados de teste
intermediate_output_test <- predict(intermediate_layer_model, x_test)

# converter para o formato tabular
dados_k_medias <- tibble(PC1 = intermediate_output_test[,1],
                         PC2 = intermediate_output_test[,2])

# ajustar kmeans com k=10
modelo_k_medias <- x_test_pred %>%
  kmeans(centers = 10)

# grafico da reducao de dimensionalidade com as labels originais
p1 <- ggplot(dados_k_medias,
       aes(x = PC1, y = PC2, col = as.factor(y_test))) +
  geom_point(alpha=.5)+
  labs(color = "Target")

# grafico de reducao de dimensionalidade com as categorizacoes do kmeans
p2 <- broom::augment(modelo_k_medias, dados_k_medias) %>%
  ggplot(aes(x = PC1, y = PC2, col = as.factor(.cluster))) +
  geom_point(alpha=.5)+
  labs(color = "Target")

p1 | p2

# Referencia --------------------------------------------------------------

# https://blog.keras.io/building-autoencoders-in-keras.html
