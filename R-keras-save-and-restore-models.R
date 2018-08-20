# setup
library(keras)

mnist = dataset_mnist()
train_images = mnist$train$x
train_labels = mnist$train$y
test_images = mnist$test$x
test_labels = mnist$test$y

dim(train_images)

train_labels <- train_labels[1:1000]
test_labels <- test_labels[1:1000]

train_images = train_images[1:1000, , ] %>%
  array_reshape(c(1000, 28*28))
train_images = train_images / 255

test_images = test_images[1:1000, , ] %>%
  array_reshape(c(1000, 28*28))
test_images = test_images / 255


# BUILD MODEL
create_model = function() {
  model = keras_model_sequential() %>%
    layer_dense(units = 512, activation = 'relu', input_shape = 28*28) %>%
    layer_dropout(0.2) %>%
    layer_dense(units = 10, activation = 'softmax')
  model %>% compile(
    optimizer = 'adam',
    loss = 'sparse_categorical_crossentropy',
    metrics = list('accuracy')
  )
  model
}

model = create_model()
model %>% summary()


# save the entire model
model = create_model()
model %>% fit(
  train_images, train_labels,
  epochs = 5
)
model %>% save_model_hdf5('my_model.h5')
# to only save the weights
model %>% save_model_weights_hdf5('my_model_weights.h5')


# now recreate the model from that file
new_model = load_model_hdf5('my_model.h5')
new_model %>% summary()



# save checkpoints during training
checkpoint_dir = "checkpoints"
dir.create(checkpoint_dir, showWarnings = FALSE)
filepath = file.path(checkpoint_dir, "weights.{epoch:02d}-{val_loss:.2f}.hd5")
cp_callback = callback_model_checkpoint(
  filepath = filepath,
  save_best_only = TRUE,
  verbose = 1
)
model = create_model()
model %>% fit(
  train_images, train_labels,
  epochs = 10,
  validation_data = list(test_images, test_labels),
  callbacks = list(cp_callback) # pass callback to training
)

# inspect the files that were created
list.files(checkpoint_dir)

# now rebuild afresh, untrained model and evaluate it on the test set
# this untrained model should have very poor performance
fresh_model = create_model()
score = fresh_model %>% evaluate(test_images, test_labels)
cat('Test lost:', score$loss, '\n')
cat('Test accuracy:', score$acc, '\n')


# then load the weights from the latest checkpoint (epoch 10), and re-evaluate:
fresh_model %>% load_model_weights_hdf5(
  file.path(checkpoint_dir, "weights.08-0.39.hd5")
)
score = fresh_model %>% evaluate(test_images, test_labels)
cat('Test lost:', score$loss, '\n')
cat('Test accuracy:', score$acc, '\n')


# to reduce the number of files, you can also save model weights only once every nth epoch.
checkpoint_dir = "checkpoints"
unlink(checkpoint_dir, recursive = TRUE)
dir.create(checkpoint_dir)
filepath <- file.path(checkpoint_dir, "weights.{epoch:02d}-{val_loss:.2f}.hdf5")

# Create checkpoint callback
cp_callback <- callback_model_checkpoint(
  filepath = filepath,
  save_weights_only = TRUE,
  period = 5,
  verbose = 1
)

model <- create_model()

model %>% fit(
  train_images,
  train_labels,
  epochs = 10, 
  validation_data = list(test_images, test_labels),
  callbacks = list(cp_callback)  # pass callback to training
)
list.files(checkpoint_dir)

# alternatively, you can also decide to save only the best model
checkpoint_dir <- "checkpoints"
unlink(checkpoint_dir, recursive = TRUE)
dir.create(checkpoint_dir)
filepath <- file.path(checkpoint_dir, "weights.{epoch:02d}-{val_loss:.2f}.hdf5")

# Create checkpoint callback
cp_callback <- callback_model_checkpoint(
  filepath = filepath,
  save_weights_only = TRUE,
  save_best_only = TRUE,
  verbose = 1
)

model <- create_model()

model %>% fit(
  train_images,
  train_labels,
  epochs = 10, 
  validation_data = list(test_images, test_labels),
  callbacks = list(cp_callback)  # pass callback to training
)

list.files(checkpoint_dir)

# In this case, weights were saved on all epochs but the 6th and 7th, where validation loss did not improve.

