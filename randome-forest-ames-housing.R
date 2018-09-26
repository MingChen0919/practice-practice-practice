set.seed(123)

# get ames housing data
ames_housing = AmesHousing::make_ames()
dim(ames_housing)
str(ames_housing)

# split data
library(rsample)
ames_split = initial_split(ames_housing, prop = 0.7)
str(ames_split)
dim(ames_housing)
dim(ames_split)

ames_train = training(ames_split)
ames_test = testing(ames_split)


# RandomForest package
# for reproducibility
set.seed(123)
library(randomForest)
m1 = randomForest(formula = Sale_Price ~ ., data = ames_train)
m1

plot(m1)

summary(m1)
which.min(m1$mse)
# RMSE of the optimal random forest
sqrt(m1$mse[which.min(m1$mse)])
