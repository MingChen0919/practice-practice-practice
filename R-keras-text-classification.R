# classify movie reviews as positive or negative using the text of the review
# use the IMDB dataset that contains the text of 50,000 movie reviews from
# the internet Movie Database
library(keras)
library(dplyr)
library(ggplot2)
library(purrr)

# download the IMDB dataset
# the argument num_words = 10000 keeps the top 10,000 most frequently
# occuring words in the training data
imdb = dataset_imdb(num_words = 10000)
train_data = imdb$train$x
train_labels = imdb$train$y
test_data = imdb$test$x
test_labels = imdb$test$y

# the dataset comes with an index mapping words to integers
# which needs to be downloaded separately
word_index = dataset_imdb_word_index()

# Explore the data
dim(train_data)
str(train_data)
str(train_labels)

# convert the integers back to words
word_index_df = data.frame(
  word = names(word_index),
  idx = unlist(word_index, use.names = FALSE),
  stringsAsFactors = FALSE
)

decode_review = function(word_idx) {
  paste(map(word_idx, function(number) word_index_df %>% 
              filter(idx == number) %>%
              select(word) %>%
              pull()),
        collapse = ' ')
}
decode_review(train_data[[1]])



# PREPARE THE DATA
# the reviews have different length, they must be converted to tensors before
# fed into the neural network.
# two options
# 1. one-hot-encode the arrays to convert them into vectors of 0s and 1s, which
# will be memory intensive
# 2. pad the arrays so they all have the same length

train_data = pad_sequences(
  train_data,
  value = 0,
  padding = "post",
  maxlen = 256
)

test_data = pad_sequences(
  test_data,
  value = 0,
  padding = "post",
  maxlen = 256
)



# BUILD THE MODEL

vocab_size = 10000
model = keras_model_sequential()
model %>%
  layer_embedding(input_dim = vocab_size, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% summary()

# LOSS FUNCTION AND OPTIMIZER
model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

# create a validation set
x_val <- train_data[1:10000, ]
partial_x_train <- train_data[10001:nrow(train_data), ]

y_val <- train_labels[1:10000]
partial_y_train <- train_labels[10001:length(train_labels)]

dim(x_val)
dim(y_val)
dim(partial_x_train)
dim(partial_y_train)


# train the model
history = model %>% fit(
  partial_x_train, partial_y_train,
  epochs = 40,
  batch_size = 512,
  validation_data = list(x_val, y_val),
  verbose = 1
)


# evaluate the model
results = model %>% evaluate(test_data, test_labels)
results

# plot training history
plot(history)
