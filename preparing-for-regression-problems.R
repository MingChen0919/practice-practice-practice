library(rsample)
library(caret)
library(h2o)
library(dplyr)

h2o.no_progress() # turn off progress bars

# get some data 
library(AmesHousing)
# data()
ames = make_ames()

# splitting data
# typical training-testing splits include 60:40 and 70:30
train_index = createDataPartition(ames$Sale_Price, p = 0.7, list = FALSE)
train_index

# different ways to split the data into training and testing sets
# base R
index = sample(1:nrow(ames), round(nrow(ames) * 0.7))
train_1 = ames[index, ]
test_1 = ames[-index, ]

# caret package
set.seed(123)
index2 = createDataPartition(y = ames$Sale_Price, p = 0.7, list = FALSE)
train_2 = ames[index2, ]
test_2 = ames[-index2, ]

# rsample package
split_1 = initial_split(ames, prop = 0.7)
train_3 = training(split_1)
test_3 = testing(split_1)

# h2o package
h2o.init()
ames_h2o = as.h2o(ames)
split_2 = h2o.splitFrame(ames_h2o, ratios = 0.7, seed = 123)
train_4 = split_2[[1]]
test_4 = split_2[[2]]



# stratified sampling
dim(attrition)

table(attrition$Attrition) %>% prop.table()

# stratified sampling with rsample package
set.seed(123)
split_strat = initial_split(attrition, prop = .7, strata = "Attrition")
train_strat = training(split_strat)
test_strat = testing(split_strat)


table(train_strat$Attrition) %>% prop.table()
table(test_strat$Attrition) %>% prop.table()


# transform categorical variables into numeric representations
# many ways:
# one-hot encoding, ordinal, binary, sum, Helmert
df = data.frame(id=1:8, x = rep(letters[1:3], length=8))
df


full_rank = dummyVars(~ .,  data = df, fullRank = TRUE)

ggplot(data = ames, mapping = aes(x = Sale_Price)) +
  geom_density() +
  geom_density(data = test_1, col = 'red')

# response transformation
# opt 1: log transformation
# opt 2: Box Cox transformation
log(train_1$Sale_Price)
lambda = forecast::BoxCox.lambda(train_1$Sale_Price)


# Predictor transformation
features = setdiff(names(train_1), "Sale_Price")

pre_process = preProcess(
  x = train_1[, features],
  method = c('center', 'scale')
)

# apply to both training and test
train_x = predict(pre_process, train_1[, features])
test_x = predict(pre_process, test_1[, features])

