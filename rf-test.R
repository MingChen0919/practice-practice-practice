library(rsample)
library(randomForest)
library(ranger)
library(caret)
library(h2o)
library(AmesHousing)


set.seed(123)
ames_split = initial_split(make_ames(), prop = .7)
ames_train = training(ames_split)
ames_test = testing(ames_split)

dim(ames_train)
dim(ames_test)


# for reproducibility
set.seed(123)

m1 <- randomForest(
  formula = Sale_Price ~ .,
  data = ames_train
)
m1

plot(m1)

which.min(m1$mse)
sqrt(m1$mse[which.min(m1$mse)])


# randomeForest also allows us to use a validation set to measure prediction 
set.seed(123)

valid_split = initial_split(ames_train, .8)

# training_data
ames_train_v2 = analysis(valid_split)

# validation data
ames_valid = assessment(valid_split)

dim(ames_train_v2)
dim(ames_valid)

x_test = ames_valid[, !(names(ames_valid) %in% "Sale_Price")]
dim(x_test)
y_test = ames_valid$Sale_Price


