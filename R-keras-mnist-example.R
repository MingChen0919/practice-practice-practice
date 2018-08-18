devtools::install_github("rstudio/keras")

library(keras)
install_keras()

# MNIST Example
## preparing data
library(keras)
mnist = dataset_mnist()
str(mnist)
x_train = mnist$train$x
y_train = mnist$train$y
x_test = mnist$test$x
y_test = mnist$test$y

str(x_train)
str(y_train)

# reshape
x_train = array_reshape(x_train, c(nrow(x_train),  prod(dim(x_train)[2:3])))
x_test = array_reshape(x_test, c(nrow(x_test), prod(dim(x_test)[2:3])))


# rescale
x_train = x_train / 255
x_test = x_test / 255

# y has more than two categories. 
# let's one-hot encode y into binary class matrices
y_train = to_categorical(y_train, 10)
y_test = to_categorical(y_test, 10)


# DEFINING THE MODEL

# we begin by creating a sequential model and then adding layers using the 
# pipe (%>%) operator
model = keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

# use summary() function to print the details of the model
summary(model)

# next, compile the model with appropriate loss function, optimizer, and metrics
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# TRAINING AND EVALUATION
# use the fit() function to train the model for 30 epochs using batches of 128 images
history = model %>% fit(
  x_train, y_train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)

# the history object returned by fit() includes loss and accuracy metrics which we can plot
plot(history)

# evaluate the model's performance on the test data
model %>% evaluate(x_test, y_test)

# generate predictions on new data
model %>% predict_classes(x_test)
