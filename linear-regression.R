library(tidyverse)
library(modelr) # provide easy pipeline modeling functions
library(broom)

advertising = read_csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
advertising = advertising %>%
  select(-X1)

# preparing out data
set.seed(123)

library(rsample)

my_split = initial_split(data = advertising, prop = 0.7)
train = training(my_split)
test = testing(my_split)


# model building
model1 = lm(sales ~ TV, data = train)
# plot(model1)

summary(model1)

tidy(model1)

model_summary = tidy(model1)
model_summary$estimate - 1.96 * model_summary$std.error
model_summary$estimate + 1.96 * model_summary$std.error


TSS = sum((train$sales - mean(train$sales))^2)
RSS = sum(model1$residuals^2)
p = 1
n = length(train$sales)

F = ( (TSS - RSS)/1 )/(RSS/(n - p - 1))
F

# assessing our model visually
ggplot(train, aes(TV, sales)) +
  geom_point() + 
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color = "red")

# first plot is fitted value vs residuals
model1_results = augment(model1, train)

ggplot(model1_results, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals vs Fitted")


# multiple linear regression
model2 = lm(sales ~ TV + radio + newspaper, data = train)
tidy(model2)

confint(model2)

list(model1 = broom::glance(model1), model2 = broom::glance(model2))
