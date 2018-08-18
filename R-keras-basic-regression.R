library(keras)

# import data
boston_housing = dataset_boston_housing()

c(train_data, train_labels) %<-% boston_housing$train
c(test_data, test_labels) %<-% boston_housing$test

# explore data
dim(train_data)
dim(train_labels)

# Let's add column names for better data inspection
library(tibble)

column_names <- c('CRIM', 'ZN', 'INDUS', 'CHAS', 'NOX', 'RM', 'AGE', 
                  'DIS', 'RAD', 'TAX', 'PTRATIO', 'B', 'LSTAT')
train_df = as_tibble(train_data)
colnames(train_df) = column_names
train_df

# normalize features
train_data = scale(train_data)

# use means and standard deviations from training set to normalize
# test set
col_means_train = attr(train_data, "scaled:center")
col_stddevs_train = attr(train_data, "scaled:scale")
test_data = scale(test_data, center = col_means_train, scale = col_stddevs_train)

train_data[1, ]

# CREATE THE MODEL
build_model = function() {
  model = keras_model_sequential() %>%
    layer_dense(units = 64, activation = 'relu',
                input_shape = dim(train_data)[2]) %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dense(units = 1)
  
  model %>% compile(
    loss = 'mse',
    optimizer = optimizer_rmsprop(),
    metrics = list('mean_absolute_error')
  )
  
  model
}

model = build_model()
model %>% summary()


# TRAIN THE MODEL
# display training progress by printing a single dot for each completed epoch
print_dot_callback = callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat('\n')
    cat('>.<')
  }
)

epochs = 500

# fit the model and store training stats
history = model %>% fit(
  train_data, train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)


library(ggplot2)
plot(history, metrics = "mean_absolute_error", smooth = FALSE) +
  coord_cartesian(ylim = c(0, 5))


# this graph shows little improvement in the model after about 200 epochs
# let's update the fit method to automatically stop training when the validation
# score doesn't improve
# we'll use a callback that tests a training condition for every epoch.
# if a set amount of epochs elapses without showing improvement, it automatically
# stops the training

early_stop = callback_early_stopping(monitor = "val_loss", patience = 20)

model = build_model()
history = model %>% fit(
  train_data, train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(history, metrics = 'mean_absolute_error', smooth = FALSE) +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 5))

# evaluate model's performance with test set
model %>% evaluate(test_data, test_labels)

# predict
pred = model %>% predict(test_data)

plot(pred, test_labels)
