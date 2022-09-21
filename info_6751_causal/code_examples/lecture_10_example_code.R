
# Instructions:

# The data file "lec10.csv" contains Simulation 1 from Dorie et al.
# It contains these variables:
# y: outcome (numeric)
# z: treatment (binary)
# x_*: confounders

# Your task is to estimate the SATT: Sample Average Treatment Effect on the Treated.
# This is the causal effect averaged over the units in the sample with z == 1.

# To do this, you should:
# - Apply the g-formula with two or more prediction functions of your choosing to estimate SATT
# - You might compare predictive performance of your candidates:
#    * learn among (set == "train")
#    * predict among (set == "test")
#    * calculate mean squared error in the test set
#    * if you know cross-validation, you are also welcome to do that

# At the end of class, your group will tell me your single best estimate of SATT.
# Because it is simulated, I have the answer! We will see who wins.

# Below is some code to get you started. This contains:
# - prepare environment
# - example with OLS
# - examples with other algorithms
# - a combination of all results
# Just use this code as it is helpful.
# You might choose an algorithm I have omitted entirely.
# You might tweak tuning parameters.
# You might change the model formula.
# Anything is allowed!

#######################
# PREPARE ENVIRONMENT #
#######################

# Load tidyverse for data manipulation
library(tidyverse)
d <- read_csv("lec10.csv")

# Define a model formula
my_formula <- as.formula(paste("y ~ z + ",paste0("x_",1:58, collapse = " + ")))

# For assessing predictive performance, separate train and test sets
train <- d %>% filter(set == "train")
test <- d %>% filter(set == "test")

# Define the data frames for making predictions:
# - the treated group
d_treated <- d %>%
  filter(z == 1)
# - the treated group with z set to 0
d_treated_under_control <- d_treated %>%
  mutate(z = 0)

# Some algorithms require a model matrix. Create that.
X <- model.matrix(my_formula, data = d)
X_treated <- model.matrix(my_formula, data = d_treated)
X_treated_under_control <- model.matrix(my_formula, data = d_treated_under_control)

####################
# EXAMPLE WITH OLS #
####################

# Estimate by LM
fit_lm <- lm(my_formula, data = d)

# Assess the out-of-sample performance
# Fit on the train set
fit_lm_train <- lm(my_formula, data = train)
# Predict and calculate mean squared error on the test set
mse_lm <- test %>%
  mutate(yhat = predict(fit_lm_train, newdata = test)) %>%
  summarize(mse = mean((y - yhat) ^ 2))

# Estimate the sample average treatment effect on the treated
satt_lm <- d_treated %>%
  # Predict the outcome under control
  mutate(yhat0 = predict(fit_lm, newdata = d_treated_under_control)) %>%
  # Difference the observed outcome under treatment from the predicted yhat0 and average
  summarize(satt = mean(y - yhat0)) %>%
  # Note what method made this estimate
  mutate(method = "lm")

################################
# EXAMPLES OF OTHER ALGORITHMS #
################################
# Estimate by elastic net regression (penalized OLS).
# This approach is just like OLS, except that coefficients are penalized.
# When alpha = 0, we have ridge regression and all coefficient get pulled toward 0 but not exactly to 0
# When alpha = 1, we have lasso regression and some coefficients are zeroed out entirely
# When 0 < alpha < 1, we have elastic net, which is a mix of the two.
# I'm using cv.glmnet to select the tuning parameter lambda.
# See documentation: https://glmnet.stanford.edu/articles/glmnet.html
library(glmnet)
fit_glmnet <- cv.glmnet(x = X, y = d$y, alpha = 0)
satt_glmnet <- d_treated %>%
  mutate(yhat0 = predict(fit_glmnet, 
                         # Use the penalty term that minimnized cross-validated error
                         s = fit_glmnet$lambda.min,
                         # Pass in the new X matrix for prediction
                         newx = X_treated_under_control)[,1]) %>%
  summarize(satt = mean(y - yhat0)) %>%
  mutate(method = "glmnet")

# Estimate by ranger (frequentist random forest)
# See documentation: https://www.jstatsoft.org/article/view/v077i01
library(ranger)
fit_ranger <- ranger(my_formula, data = d)
predicted_ranger_under_control <- predict(fit_ranger, data = d_treated_under_control)
yhat_ranger_under_control <- predicted_ranger$predictions
satt_ranger <- d_treated %>%
  mutate(yhat0 = yhat_ranger_under_control) %>%
  summarize(satt = mean(y - yhat0)) %>%
  mutate(method = "ranger")

# Estimate by bart (Bayesian random forest)
# There are several implementations.
# See documentation for this one: https://rdrr.io/cran/BART/man/gbart.html
library(BART)
fit_BART <- mc.gbart(x.train = X, y.train = d$y,
                     x.test = X_treated_under_control)
# Predictions are the average over trees
predicted_BART_under_control <- colMeans(fit_BART$yhat.test)
# Estimate the SATT
satt_BART <- d_treated %>%
  mutate(yhat0 = predicted_BART_under_control) %>%
  summarize(satt = mean(y - yhat0)) %>%
  mutate(method = "BART")

# Estimate by Super Learner
# See documentation: https://cran.r-project.org/web/packages/SuperLearner/vignettes/Guide-to-SuperLearner.html
# Also see useful intro paper: https://link.springer.com/article/10.1007/s10654-018-0390-z
# Super Learner takes a series of candidate learners.
# Through cross-validation, it estimates a weighted average of those learners
# to predict the outcome. For example, the result might be 29% OLS and 71% ranger.
library(SuperLearner)
fit_SuperLearner <- SuperLearner(Y = d$y, X = data.frame(X),
                                 newX = data.frame(X_treated_under_control),
                                 SL.library = c("SL.lm","SL.glmnet","SL.ranger"))
# See the weighted average SuperLearner produced
fit_SuperLearner$coef
satt_SuperLearner <- d_treated %>%
  mutate(yhat0 = fit_SuperLearner$SL.predict[,1]) %>%
  summarize(satt = mean(y - yhat0)) %>%
  mutate(method = "SuperLearner")

#######################
# COMBINE ALL RESULTS #
#######################
satt_lm %>%
  bind_rows(satt_glmnet) %>%
  bind_rows(satt_ranger) %>%
  bind_rows(satt_BART) %>%
  bind_rows(satt_SuperLearner) %>%
  ggplot(aes(x = method, y = satt)) +
  geom_point()

