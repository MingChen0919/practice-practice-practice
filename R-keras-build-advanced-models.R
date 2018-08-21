library(keras)

data <- matrix(rnorm(1000 * 32), nrow = 1000, ncol = 32)
labels <- matrix(rnorm(1000 * 10), nrow = 1000, ncol = 10)

# FUNCTIONAL API
# use the keras function api to build complex model topologies such as
# multi-input models,
# multi-output models,
# models with shared layers (the same layer called several times)
# models with non-sequential data flows (e.g., residual connections)

inputs = layer_input(shape = (32))

predictions = inputs %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

# instantiate the model diven inputs and outputs
model = keras_model(inputs = inputs, outputs = predictions)

# the compile step specifies the training configuration
model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.001),
  loss = 'categorical_crossentropy',
  metrics = list('accuracy')
)

# trains for 5 epochs
model %>% fit(
  data,
  labels,
  batch_size = 32,
  epochs = 5
)


# CUSTOM LAYERS
