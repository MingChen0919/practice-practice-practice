library(keras)
library(dplyr)
library(ggplot2)
library(tidyr)
library(tibble)

# download the IMDB dataset
num_words = 10000
imdb = dataset_imdb(num_words = num_words)

train_data = imdb$train$x
train_labels = imdb$train$y
test_data = imdb$test$x
test_labels = imdb$test$y

# multi-hot-encoding the lists
multi_hot_sequences = function(sequences, dimension) {
  multi_hot = matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences)) {
    multi_hot[i, sequences[[i]]] = 1
  }
  multi_hot
}

train_data = multi_hot_sequences(train_data, num_words)
test_data = multi_hot_sequences(test_data, num_words)

dim(train_data)
dim(test_data)

# look at one of the resulting multi-hot vectors
plot(x = 1:10000, yaxt='n', type='n', frame.plot = FALSE)
abline(v=which(train_data[1, ] == 1))


# CREATE A BASELINE MODEL
baseline_model = keras_model_sequential() %>%
  layer_dense(units = 16, activation = 'relu', input_shape = 10000) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dense(units = 1, activation = "sigmoid")

baseline_model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

baseline_model %>% summary()

baseline_history = baseline_model %>% fit(
  train_data, train_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(test_data, test_labels),
  verbose = 2
)


# CREATE A SMALLER MODEL
smaller_model = keras_model_sequential() %>%
  layer_dense(units = 4, activation = 'relu', input_shape = 10000) %>%
  layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

smaller_model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

smaller_model %>% summary()

smaller_history = smaller_model %>% fit(
  train_data, train_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(test_data, test_labels),
  verbose = 2
)


# CREATE A BIGGER MODEL
bigger_model = keras_model_sequential() %>%
  layer_dense(units = 512, activation = 'relu', input_shape = 10000) %>%
  layer_dense(units = 512, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

bigger_model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

bigger_model %>% summary()

bigger_history = bigger_model %>% fit(
  train_data, train_labels,
  epochs = 20,
  batch_size = 512,
  validation_data = list(test_data, test_labels),
  verbose = 2
)


# PLOT THE TRINING AND VALIDATION LOSS
compare_cx = data.frame(
  baseline_train = baseline_history$metrics$loss,
  baseline_val = baseline_history$metrics$val_loss,
  smaller_train =smaller_history$metrics$loss,
  smaller_val = smaller_history$metrics$val_loss,
  bigger_train = bigger_history$metrics$loss,
  bigger_val = bigger_history$metrics$val_loss,
  epochs = 1:20
) %>% 
  gather(key='type', value = 'loss', -epochs)

compare_cx$model = str_replace(compare_cx$type, '_.+', '')
compare_cx$type = str_replace(compare_cx$type, '.+_', '')

ggplot(compare_cx, aes(x=epochs, y=loss, color=model, linetype=type)) +
  geom_line()


## STRATEGIES
# option 1: add weight regularization
# option 2: add dropout