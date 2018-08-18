library(keras)

# import the Fashion MNIST dataset
fashion_mnist = dataset_fashion_mnist()

train_images = fashion_mnist$train$x
train_labels = fashion_mnist$train$y
test_images = fashion_mnist$test$x
test_labels = fashion_mnist$test$y

str(train_images)
str(train_labels)

# since the class names are not included with the dataset,
# we'll store them in a vector to use later when plotting the images
class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

# explore the data
dim(train_images)
dim(train_labels)

# preprocess the data
library(tidyr)
library(ggplot2)

image_1 = as.data.frame(train_images[1,,])
colnames(image_1) = seq_len(ncol(image_1))
image_1$y = seq_len(nrow(image_1))
# gather 1:28 columns
image_1 = gather(image_1, key = "x", value = "value", -y)
image_1$x = as.integer(image_1$x)

# plot
ggplot(image_1, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  scale_fill_gradient(low = "black", high = "orange", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  theme(aspect.ratio = 1) +
  labs(x="", y="")

# scale images
dim(train_images)
train_images = train_images/255  
test_images = train_images/255

# display the first 25 images from the training set and display the class name
par(mfcol=c(5,5))
par(mar=c(0, 0, 1.5, 0), xaxs='i', yaxs='i')
for (i in 1:25) {
  img = train_images[i, , ]
  img = t(apply(img, 2, rev))
  image(1:28, 1:28, img, col = gray((0:255)/255), 
        xaxt = 'n', yaxt = 'n',
        # label code is from 0 to 9
        main = paste(class_names[train_labels[i] + 1]))
}


# BUILD THE MODEL
# setup the layers
model = keras_model_sequential()
model %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 10, activation = 'softmax')

# compile the model
model %>% compile(
  optimizer = 'adam',
  loss = 'sparse_categorical_crossentropy',
  metrics = c('accuracy')
)

# train the model
history = model %>% fit(
  train_images, train_labels,
  epochs = 5
)

# plot history
plot(history)

# evaluate model's performance with test data
model %>% evaluate(test_images, test_labels)

# make predictions
predictions = model %>% predict(test_images)
predictions[1, ]

# a prediction is an array of 10 probabilities.
# which corresponds to each of the 10 different categories
which.max(predictions[1, ])

# alternatively, we can also directly get the class prediction
class_pred = model %>% predict_classes(test_images)
class_pred[1:20]
test_labels[1:20]

library(caret)
confusionMatrix(as.factor(class_pred), as.factor(test_labels))


# let plot several images with their predictions.
# correct prediction labels are green and incorrect prediction labels are red
par(mfcol=c(5,5))
par(mar=c(0,0,1.5,0), xaxs='i', yaxs='i')
for (i in 1:25) {
  img = test_images[i,,]
  img = t(apply(img, 2, rev))
  # substract 1 as labels go from 0 to 9
  predicted_label = which.max(predictions[i, ]) - 1
  true_label = test_labels[i]
  color = ifelse(predicted_label == true_label, 'green', 'red')
  image(1:28, 1:28, img, col = gray((0:255)/255), 
        xaxt = 'n', yaxt = 'n',
        main = paste0(class_names[predicted_label + 1],
                      " (", class_names[true_label + 1], ")"),
        col.main = color)
}

# finally, use the trained model to make a prediction about a single image
img = test_images[1,,, drop = FALSE]
dim(img)

# now predict the image
class_names[model %>% predict_classes(img) + 1]
