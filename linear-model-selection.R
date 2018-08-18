# packages
library(tidyverse) # data manipulation and visualization
# install.packages('leaps')
library(leaps) # model selection functions

# load data and remove rows with missing data
(
  hitters <- na.omit(ISLR::Hitters) %>%
    as_tibble
)


# Best Subset Selection
# to perform best subset selection, we fit a separate least squares regression for
# each possible combination of the p predictors.
# the three-stage process of performing best subset selection includes:
# step 1: fit Null model M0, which contains no predictors
# step 2: fit all other possible models
# select a single best model from all models using cross-validated prediction
# error, Cp, AIC, BIC, or adjusted R^2
best_subset <- regsubsets(Salary ~ ., hitters, nvmax = 19)
summary(best_subset)


# stepwise selection
# for computational reasons, best subset selection cannot be applied when the number
# of p predictor variables is large.
# also, enormous model search space can lead to overfitting and high variance
# of the coefficient estimates.

# Forward stepwise
# three-stage process
# step 1: let M0 denote the null model, which contains no predictors.
# this model simply predicts the sample mean for each observation
# step 2: select the best model from each of the models that contain k = 0, 1, ..., p-1 models
# best model is defined as having smallest RSS or highest R^2
# select a single best model from among all best models using cross validated prediction error
forward <- regsubsets(Salary ~ ., hitters, nvmax = 19, method = 'forward')

# Backward stepwise
backward <- regsubsets(Salary ~ ., hitters, nvmax = 19, method = 'backward')

# comparing models
# indirectly estimating test error with Cp, AIC, BIC, and Adjusted R^2
# create training - testing data
set.seed(1)
sample <-  sample(c(TRUE, FALSE), nrow(hitters), replace = T, prob = c(0.6, 0.4))
train <- hitters[sample, ]
test <- hitters[!sample, ]

# perform best subset selection
best_subset <- regsubsets(Salary ~ ., train, nvmax = 19)
results <- summary(best_subset)

# extract and plot results
tibble(predictors = 1:19,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~ statistic, scales = "free")

which.max(results$adjr2)
which.min(results$bic)
which.min(results$cp)

# we can compare the variables and coefficients that these models include
# using the coef function.
coef(best_subset, which.max(results$adjr2))
coef(best_subset, which.min(results$bic))
coef(best_subset, which.min(results$cp))

# we could perform the same process using forward and backward stepwise
# selection and obtain even more options for optiomal modles
forward <- regsubsets(Salary ~ ., train, nvmax = 19, method = 'forward')
backward <- regsubsets(Salary ~ ., train, nvmax = 19, method = 'backward')

which.max(summary(forward)$adjr2)
which.max(summary(backward)$adjr2)

# this highlights two important findings:
# 1. different subsetting procedures (best subset vs. forward vs. backward)
# will likely identify different "best" models
# 2. different indirect error test estimate statistics (Cp, AIC, BIC, 
# and Adjusted R^2) will likely identify different "best" models.



# Directly estimating test error
test_m <- model.matrix(Salary ~ ., data = test)
