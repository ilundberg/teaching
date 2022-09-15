
library(tidyverse)
n <- 10000
set.seed(14850)

# This code illustrates the parametric g-formula applied with OLS.

# Simulate some data
sim_data <- data.frame(L = rbinom(n,1,.5)) %>%
  # Generate the treatment
  mutate(A = rbinom(n,1,plogis(L))) %>%
  # Generate the outcome
  mutate(Y = rnorm(n, L + A))

#############################
# EXAMPLE 1: ADDITIVE MODEL #
#############################

# Assume an OLS model
fit <- lm(Y ~ L + A, data = sim_data)

g_estimate <- sim_data %>%
  # Predict for each unit under treatment and control.
  mutate(yhat1 = predict(fit, newdata = sim_data %>% mutate(A = 1)),
         yhat0 = predict(fit, newdata = sim_data %>% mutate(A = 0)),
         # Difference to make a conditional average effect estimate
         effect = yhat1 - yhat0) %>%
  # Average over the sample to yield a sample average effect estimate
  summarize(average_effect = mean(effect),
            .groups = "drop")

# In this case, the g-estimate and the OLS coefficient are mathematically equal.
g_estimate$average_effect == coef(fit)["A"]

# In general, the g-formula is great for outcome models more complex than this.

################################
# EXAMPLE 2: INTERACTIVE MODEL #
################################

# Assume an OLS model
fit <- lm(Y ~ L*A, data = sim_data)

g_estimate <- sim_data %>%
  # Predict for each unit under treatment and control.
  mutate(yhat1 = predict(fit, newdata = sim_data %>% mutate(A = 1)),
         yhat0 = predict(fit, newdata = sim_data %>% mutate(A = 0)),
         # Difference to make a conditional average effect estimate
         effect = yhat1 - yhat0) %>%
  # Average over the sample to yield a sample average effect estimate
  summarize(average_effect = mean(effect),
            .groups = "drop")

# In this case, the average effect is not the same as any model coefficient.