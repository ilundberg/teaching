
# This file repeats the estimator many times
# to visualize the variance over repeated samples.
# This file is written for me and is not one that workshop
# participants would produce.

library(tidyverse)
theme_set(theme_bw())
library(foreach)

simulate <- function(n = 100) {
  data.frame(X1 = rnorm(n)) %>%
    mutate(A1 = rbinom(n, 1, plogis(.25*X1)),
           U2 = rnorm(n),
           X2 = rnorm(n, mean = -.25 + .5*A1 + U2),
           A2 = rbinom(n, 1, plogis(.25*X2)),
           Y = rnorm(n, X1 + A1 + X2 + A2 + U2))
}

simulate_truth <- function(n = 100, a1, a2) {
  data.frame(X1 = rnorm(n)) %>%
    mutate(A1 = a1,
           U2 = rnorm(n),
           X2 = rnorm(n, mean = -.25 + .5*A1 + U2),
           A2 = a2,
           Y = rnorm(n, X1 + A1 + X2 + A2 + U2))
}

truth <- foreach(a1 = 0:1, .combine = "rbind") %do% {
  foreach(a2 = 0:1, .combine = "rbind") %do% {
    simulate_truth(n = 1e6, a1 = a1, a2 = a2)
  }
} %>%
  group_by(A1,A2) %>%
  summarize(truth = mean(Y), .groups = "drop")

# Estimator
estimator <- function(data) {
  
  # Fit propensity score models
  fit_A1 <- glm(A1 ~ X1,
                family = binomial,
                data = data)
  fit_A2 <- glm(A2 ~ A1 + X2,
                family = binomial,
                data = data)
  
  # Create probability of the observed treatments
  with_pi <- data %>%
    mutate(P_A1_equals_1 = predict(fit_A1, type = "response"),
           P_A2_equals_1 = predict(fit_A2, type = "response"),
           pi1 = case_when(A1 == 1 ~ P_A1_equals_1,
                           A1 == 0 ~ 1 - P_A1_equals_1),
           pi2 = case_when(A2 == 1 ~ P_A2_equals_1,
                           A2 == 0 ~ 1 - P_A2_equals_1))
  
  # Create inverse probability weighted estimates
  ipw <- with_pi %>%
    group_by(A1, A2) %>%
    summarize(estimate = weighted.mean(Y, w = 1 / (pi1 * pi2)),
              .groups = "drop") %>%
    mutate(method = "IPW")
  
  # Create marginal structural model estimates
  # Fit the MSM
  fit_msm <- lm(Y ~ A1 + A2,
                data = with_pi,
                w = (1 / (pi1 * pi2)))
  # Predict the estimates
  msm <- data.frame(A1 = c(0,0,1,1),
                    A2 = c(0,1,0,1)) %>%
    mutate(estimate = predict(fit_msm, newdata = .)) %>%
    mutate(method = "MSM")
  return(ipw %>%
           bind_rows(msm))
}

# Population estimates
population <- simulate(n = 1e5)
population_estimate <- estimator(population)

population_estimate %>%
  bind_rows(truth %>% rename(estimate = truth) %>% mutate(method = "Truth")) %>%
  mutate(A1 = paste("A1 =",A1),
         A2 = paste("A2 =",A2)) %>%
  ggplot(aes(x = method, y = estimate)) +
  geom_point() +
  facet_grid(A1 ~ A2)

# Carry out many sample-based estimates
simulations <- foreach(r = 1:1e3, .combine = "rbind") %do% {
  simulated_data <- simulate(n = 1e2)
  simulated_estimates <- estimator(simulated_data)
  return(simulated_estimates)
}

# Summarize performance across simulations
simulations %>%
  sample_n(2e3) %>%
  mutate(A1 = paste("A1 =",A1),
         A2 = paste("A2 =",A2)) %>%
  ggplot(aes(x = method, y = estimate)) +
  geom_point(position = position_jitter(width = .2, height = NULL),
             size = .5, color = "gray", alpha = .5) +
  facet_grid(A1 ~ A2) +
  xlab("Method") +
  ylab("Estimates Over Repeated Simulations")

ggsave("figures/msm_samples.pdf",
       height = 4, width = 4)

# Visualize sampling variance of estimators
simulations %>%
  mutate(A1 = paste("A1 =",A1),
         A2 = paste("A2 =",A2)) %>%
  group_by(method,A1,A2) %>%
  summarize(variance = var(estimate)) %>%
  ggplot(aes(x = method, y = variance)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_line() +
  facet_grid(A1 ~ A2) +
  ylab("Sampling Variance of Estimator") +
  xlab("Estimator")

ggsave("figures/msm_variance.pdf",
       height = 4, width = 4)

## SUPPLEMENTAL THINGS

# Visualize bias pf estimators
simulations %>%
  left_join(truth, by = c("A1","A2")) %>%
  mutate(A1 = paste("A1 =",A1),
         A2 = paste("A2 =",A2)) %>%
  group_by(method,A1,A2) %>%
  summarize(bias = mean(estimate - truth),
            bias_se = sd(estimate - truth) / sqrt(n()),
            ci.min = bias - qnorm(.975) * bias_se,
            ci.max = bias + qnorm(.975) * bias_se,
            .groups = "drop") %>%
  ggplot(aes(x = method, y = bias,
             ymin = ci.min, ymax = ci.max)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(width = .2) +
  facet_wrap(~A1 + A2) +
  ylab("Bias of Estimator") +
  xlab("Estimator")

# Compare to naive approach (which is wrong)
fit <- lm(Y ~ X1 + X2 + A1 + A2,
          data = population)


