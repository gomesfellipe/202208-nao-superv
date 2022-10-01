
# Curso-R
# Trabalho final para o curso de Aprendizado Não-Supervisionado
# Fellipe Gomes

# Objetivos ---------------------------------------------------------------

# Neste script aplicaremos o metodo de auto-encoder desenvolvido com keras.

# Apesar do autoencoder não ser necessariamente um modelo não supervisionado
# (mais precisamente "autosupervisionado", pois estimamos os valores de entrada)
# ele serve para buscar representações uteis sem a necessidade de rotulos.

# Sera usado como metodo de redução de dimensionalidade e detecção de outliers.
# Para clusterização utilizaremos kmeans.

# ======================== ATENCAO ================================
# Se quiser ver os melhores resultados que consegui com este modelo
# altere a linha 117 onde units = 2 para units = 100.
# isso permitira que o "gargalo" da rede nao seja tao "apertado".
# Porem nao conseguiremos visualizar a representacao 2D de como
# os dados foram mapeados pela rede.
# =================================================================

# Importar dependencias ---------------------------------------------------

# Importar bibliotecas
library(keras)
library(tidyverse)
library(patchwork)

# reprodutibilidade
set.seed(1)

# funcao equivalente a np.clip do python
clip <- function(x, lower, upper) {
  pmax(pmin(x, upper), lower)
}

# plotar n digitos do mnist
plot_digits_mnist <- function(x, y, n){

  map_dfr(1:n,~{

    digit = paste0(str_pad(.x, 3, pad = "0"), ": ", y[.x])

    x[.x,,] %>%
      as_tibble() %>%
      rownames_to_column(var = 'y') %>%
      gather(x, val, V1:V28) %>%
      mutate(x = str_replace(x, 'V', '')) %>%
      mutate(x = as.numeric(x),
             y = as.numeric(y)) %>%
      mutate(y = 28-y) %>%
      mutate(digit = digit)
  }) %>%
    ggplot(aes(x, y))+
    geom_tile(aes(fill = val+1))+
    coord_fixed()+
    theme_void()+
    theme(legend.position="none")+
    facet_wrap(~digit) +
    scale_fill_gradient(low = "white", high = "black", na.value = NA)
}

# tema para graficos
theme_set(theme_classic()+theme(legend.position = 'bottom'))

# Dados -------------------------------------------------------------------

# Carregar dados
mnist   <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test  <- mnist$test$x
y_test  <- mnist$test$y

# Normalizar dados no intervalo [0,1]
x_train <- x_train / 255
x_test  <- x_test / 255

# adicionar ruido as imagens
noise_factor <- 0.2
x_train_noisy <- x_train + noise_factor * array( rnorm(60000*28*28,mean=0,sd=1), c(60000, 28, 28))
x_test_noisy <- x_test + noise_factor * array( rnorm(10000*28*28,mean=0,sd=1), c(10000, 28, 28))

# garantir intervalo [0,1]
x_train_noisy <- clip(x_train_noisy, 0., 1.)
x_test_noisy <- clip(x_test_noisy, 0., 1.)

# Ver 36 primeiros digitos da base
# visualizar dados
{
  p1 <- plot_digits_mnist(x_test, y_test, 25) + labs(title="Original")
  p2 <- plot_digits_mnist(x_test_noisy, y_test, 25) + labs(title = "Ruidoso")
  p1|p2
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
  # encoder
  layer_dense(units = 1000, activation = "relu", input_shape =  c(784)) %>%
  layer_dense(units = 500, activation = "relu") %>%
  layer_dense(units = 300, activation = "relu") %>%
  layer_dense(units = 100, activation = "relu") %>%
  # bottleneck
  layer_dense(units = 2, name = "bottleneck") %>% # units = 100 produz bons resultados
  layer_batch_normalization() %>%
  layer_activation_leaky_relu() %>%
  # decoder
  layer_dense(units = 100, activation = "relu") %>%
  layer_dense(units = 300, activation = "relu") %>%
  layer_dense(units = 500, activation = "relu") %>%
  layer_dense(units = 1000, activation = "relu") %>%
  # output
  layer_dense(units = 28*28, activation = "sigmoid")

# Instanciar modelo
model %>%
  compile(loss = "mse",
          optimizer = optimizer_adam(learning_rate = 0.0005))

summary(model)

# Treinar modelo
tictoc::tic()
history <- model %>%
  fit(x = x_train_noisy,
      y = x_train_noisy,
      epochs = 100,
      batch_size = 256,
      shuffle=T,
      validation_split =.2,
      view_metrics = TRUE,
      callbacks=list(callback_early_stopping(
        monitor = "val_loss",
        patience = 10,
        restore_best_weights = TRUE
      )),
      verbose=2)
tictoc::toc()
# 144.716 sec elapsed quando units=100 no bottleneck
# 244.296 sec elapsed quando units=2 no bottleneck (se diminuir o lr melhora)

# reconstrucao dos digitos estimados
x_test_pred <- predict(model, x_test_noisy)

# Plot para comparar resultados com gabarito
{
  p1 <- plot_digits_mnist(
    array_reshape(x_test_noisy, c(nrow(x_test_noisy), c(28,28))),
    y_test, 25)+
    labs(title="Dados ruidosos")

  p2 <- plot_digits_mnist(
    array_reshape(x_test, c(nrow(x_test_noisy), c(28,28))),
    y_test, 25)+
    labs(title="Gabarito")

  p3 <- plot_digits_mnist(
    array_reshape(x_test_pred, c(nrow(x_test_pred), c(28,28))),
    y_test, 25)+
    labs(title="Imagens decodificadas")

  p1 | p2 | p3
}

# Quando units=100 os resultados ficam muito bons
# Quando units=2 os digitos 4 se confundem muito com o 9 e o 3 com 5

# Reducao de dimensionalidade ---------------------------------------------

# A vantagem de usar units=2 no bottleneck é que podemos visualizar
# como foi a codificacao dos digitos, assim como um tsne de 2 componentes

# Obter camada do "gargalo" do modelo
bottleneck_layer_model <- keras_model(
  inputs = model$input,
  outputs = get_layer(model, "bottleneck")$output)

# Obter estimativas
intermediate_output <- predict(bottleneck_layer_model, x_test_noisy)

# visualizar
amostra <- sample(x = 1:nrow(y_test), size = 1000)
tibble(PC1 = intermediate_output[,1],
       PC2 = intermediate_output[,2]) %>%
  slice(amostra) %>%
  ggplot(aes(x = PC1, y = PC2, col = as.factor(y_test[amostra]))) %+%
  geom_point(alpha=.5)%+%
  labs(color = "Target") %>%
  plotly::ggplotly()
# Parece que a camada da rede responsavel por comprimir a informacao dos dados
# em duas dimensoes conseguiu diferenciar bem a maioria dos digitos.
# Alguns ainda estão sobrepostos e isto refletirá nos digitos estimados.
# Depois de diversos testes notei que nosso "bottleneck" esta muito "apertado".
# Aumentando o "bottleneck" para 100 neuronios produz excelentes resultados.
# Ainda conseguimos reduzir a dimensao dos dados (de 784 para 10 colunas)
# mas esta visualizacao parecida com a PCA perde o sentido pois nao sabemos
# quando % da variabilidade dos dados é explicada por essas dimensões.

# Detecao de outliers -----------------------------------------------------

# Voltar para formato original
x_test_noisy <- array_reshape(x_test, c(nrow(x_test_noisy), c(28,28)))
x_test_pred <- array_reshape(x_test_pred, c(nrow(x_test_pred), c(28,28)))

# Calcular erro de reconstrucao da imagem
erro_reconstrucao_ae <- sqrt((x_test_noisy - x_test_pred)^2)

# Conferir se MAD > 3
verifica_se_esta_fora_de_095 <- function(x){
  # (x-median(x))/median(abs(x-median(x))) > 3 # MAD
  (x-mean(x))/sd(x) > 1.96
}

# visualizar erros
{
  p1 <- plot_digits_mnist(
    array_reshape(x_test_pred, c(nrow(x_test_pred), c(28,28))),
    y_test, 25)+
    labs(title="Imagens decodificadas")

  p2 <- plot_digits_mnist(
    erro_reconstrucao_ae,
    y_test, 25)+
    labs(title="Erro de reconstrução")

  p3 <- plot_digits_mnist(
    verifica_se_esta_fora_de_095(erro_reconstrucao_ae),
    y_test, 25)+
    labs(title="Esta acima de 3 MAD")

  p1 | p2 | p3
}

# Quando units=100 no bottleneck, vera que os digitos estao bem mais legiveis
# e os erros sao muito menores e estao mais associados a posicao do digito.

# Quando units=2 no bottlenck (util para representacao em 2 dimensoes)
# vera que os digitos 4, 9, 5 e 6 ficam muito ruidosos e ate errados mesmo

# Clustering --------------------------------------------------------------

set.seed(1)

# reshape para formato tabular
x_test <- array_reshape(x_test, c(nrow(x_test), 28*28))
x_test_pred <- array_reshape(x_test_pred, c(nrow(x_test_pred), 28*28))
x_test_noisy <- array_reshape(x_test_noisy, c(nrow(x_test_noisy), 28*28))

# Reducao de dimensionalidade nos dados de teste
bottleneck_output_test <- predict(bottleneck_layer_model, x_test)

# converter para o formato tabular
dados_k_medias <- tibble(PC1 = bottleneck_output_test[,1],
                         PC2 = bottleneck_output_test[,2])

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
