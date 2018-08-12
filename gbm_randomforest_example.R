library(h2o)

# create an H2O cloud
h2o.init(
  nthreads = -1,            ## -1: use all availabel threads
  max_mem_size = "2G"       ## specify the memory size for the H2O cloud
)
h2o.removeAll() # clean slate - just in case the cluster was already running

# import data
data <- h2o.importFile("./data/h2o_data/data_training/loan.csv")
dim(data) # 163,987 rows x 15 columns

# since we want to train a binary classification model,
# we must ensure that the response is coded as a factor
# if the response is 0/1, H2O will assume it's numeric,
# which means that H2O will train a regression model instead
data$bad_loan <- as.factor(data$bad_loan)
h2o.levels(data$bad_loan)

# partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = data,
                         ratios = c(0.7, 0.15), # partition data into 70%, 15%, 15% chunks
                         seed = 1) # setting a seed will guarantee reproducibility
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

# take a look at the size of each partition
# notice that h2o.splitFrame uses approximate splitting not exact splitting (for efficiency)
nrow(train) # 114908
nrow(valid) # 24498
nrow(test) #  24581

# identify response and predictor variables
y <- "bad_loan"
x <- setdiff(names(data), c(y, "int_rate")) # remove the interest rate column because it's correlate with otucome
print(x)

# [1] "loan_amnt"             "term"                  "emp_length"           
# [4] "home_ownership"        "annual_inc"            "purpose"              
# [7] "addr_state"            "dti"                   "delinq_2yrs"          
# [10] "revol_util"            "total_acc"             "longest_credit_length"
# [13] "verification_status"  

# now that we have prepared the data, we can train some models
# we will start by training a single model from each of the H2O supervised algorithms
# 1. Generalized Linear Modle (GLM)
# 2. Random Forest (RF)
# 3. Gradient Boosting Machine (GBM)
# 4. Deep Learning (DL)
# 5. Naive Bayes (NB)

# 1. let's start with a basic binomial GLM
# by default, h2o.glm uses a regularized, elastic net model
glm_fit1 <- h2o.glm(x = x,
                    y = y,
                    training_frame = train,
                    model_id = "glm_fit1",
                    family = "binomial") # similar to R's glm, h2o.glm has the family argument

# next we will do some automatic tuning by passing in a validation frame and setting
# `lambda_search = TRUE`. Since we are training a GLM with regularization, we should
# try to find the right amount of regularization (to avoid overfitting). The model
# parameter, `lambda`, controls the amount of regularization in a GLM model and we can
# find the optimal value for `lambda` automatically by setting `lambda_search = TRUE`
# and passing in a validation frame (which is used to evaluate model performance using a 
# particular value of lambda)
glm_fit2 <- h2o.glm(x = x,
                    y = y,
                    training_frame = train,
                    model_id = "glm_fit2",
                    validation_frame = valid,
                    family = "binomial",
                    lambda_search = TRUE)
# Let's compare the performance of the two GLMs
glm_perf1 <- h2o.performance(model = glm_fit1,
                             newdata = test)
glm_perf2 <- h2o.performance(model = glm_fit2,
                             newdata = test)

# print model performance
glm_perf1
glm_perf2

# retrieve test set AUC
h2o.auc(glm_perf1)
h2o.auc(glm_perf2)

# compare test AUC to the training AUC and valiation AUC
h2o.auc(glm_fit2, train = TRUE, valid = TRUE)
glm_fit2@model$validation_metrics






# 2. Random Forest
# H2O's Random Forest implements a distributed version of the standard RF algorithm
# and variable importance measures.
# First we will train a basic RF model with default parameters
# The RF model with infer the response distribution from the response encoding.
# A seed is required for reproducibility
rf_fit1 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "rf_fit1",
                            seed = 1)

# next we will increase the number of trees used in the forest by setting `ntrees = 100`
# the default number of trees in an H2O RF is 50, so this RF will be twice as big
# as the default. Usually increasing the number of trees in a RF will increase
# performance as well. Unlike GBMs, RF are fairly resistant (although not free from)
# overfitting.
# see the GBM example below for additional guidance on preventing overfitting using
# H2O's early stopping functionality
rf_fit2 <- h2o.randomForest(x = x,
                            y = y,
                            training_frame = train,
                            model_id = "rf_fit2",
                            # validation_frame = valid, # only used if stopping_rounds > 0,
                            ntrees = 100,
                            seed = 1)
