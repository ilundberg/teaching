
library(tidyverse)
theme_set(theme_bw())

forplot <- data.frame(u = seq(.01,.99,.01)) %>%
  mutate(Marginal = dbeta(u,5,5),
         Treated = Marginal * u,
         Untreated = Marginal * (1 - u)) %>%
  pivot_longer(cols = -u) %>%
  mutate(facet = case_when(name == "Marginal" ~ "Marginal Distribution",
                           T ~ "Distribution Within Treatment Values"),
         facet = fct_rev(facet))
forplot %>%
  ggplot(aes(x = u, y  = value, fill = name)) +
  geom_polygon(alpha = .4) +
  ylab("Density") +
  facet_wrap(~facet, ncol = 1, scales = "free_y") +
  xlab("Propensity Score") +
  xlim(c(0,1)) +
  theme(legend.position = "None") +
  geom_text(data = forplot %>%
              group_by(name) %>%
              filter(1:n() == 1) %>%
              mutate(u = case_when(name == "Treated" ~ .85,
                                   T ~ .15)),
            aes(label = name, color = name,
                y = case_when(name == "Marginal" ~ 2,
                              T ~ 1)),
            vjust = -.5, fontface = "bold", size = 5)

ggsave("figures/propensity_overlap.pdf",
       height = 5, width = 4)

# Repeat with vertical truncation lines
forplot %>%
  ggplot(aes(x = u, y  = value, fill = name)) +
  geom_polygon(alpha = .4) +
  geom_vline(xintercept = c(.25,.75),
             color = "gray", linetype = "dashed", size = 1.2) +
  ylab("Density") +
  facet_wrap(~facet, ncol = 1, scales = "free_y") +
  xlab("Propensity Score") +
  xlim(c(0,1)) +
  theme(legend.position = "None") +
  geom_text(data = forplot %>%
              group_by(name) %>%
              filter(1:n() == 1) %>%
              mutate(u = case_when(name == "Treated" ~ .85,
                                   T ~ .15)),
            aes(label = name, color = name,
                y = case_when(name == "Marginal" ~ 2,
                              T ~ 1)),
            vjust = -.5, fontface = "bold", size = 5)

ggsave("figures/propensity_overlap_1.pdf",
       height = 5, width = 4)

# Trim to middle portion
p <- forplot %>%
  mutate(u = case_when(u < .25 ~ .25,
                       u > .75 ~ .75,
                       T ~ u)) %>%
  ggplot(aes(x = u, y  = value, fill = name)) +
  geom_polygon(alpha = .4) +
  geom_vline(xintercept = c(.25,.75),
             color = "gray", linetype = "dashed", size = 1.2) +
  ylab("Density") +
  facet_wrap(~facet, ncol = 1, scales = "free_y") +
  xlab("Propensity Score") +
  xlim(c(0,1)) +
  theme(legend.position = "None") +
  geom_text(data = forplot %>%
              group_by(name) %>%
              filter(1:n() == 1) %>%
              mutate(u = case_when(name == "Treated" ~ .85,
                                   T ~ .15)),
            aes(label = name, color = name,
                y = case_when(name == "Marginal" ~ 2,
                              T ~ 1)),
            vjust = -.5, fontface = "bold", size = 5)
p
ggsave("figures/propensity_overlap_trim.pdf",
       height = 5, width = 4)

# Truncate weights
p +
  geom_point(data = forplot %>% 
               filter(u %in% c(.25,.75)) %>%
               mutate(value = 0),
             aes(color = name),
             size = 4)

ggsave("figures/propensity_overlap_truncate.pdf",
       height = 5, width = 4)


# Simulate the bias-variance tradeoff

# Define the true ATT
truth <- 1 + mean(rbeta(10000,5,5) ^ 2)

simulate <- function(n = 100, trunc = 0) {
  # Simualte the propensity score from a Beta distribution
  estimate <- data.frame(p = rbeta(n,5,5)) %>%
    # Assign treatment
    mutate(A = rbinom(n,1,p),
           # Define the generalized propensity score
           pi = ifelse(A == 1, p, 1 - p)) %>%
    # 
    mutate(Y = rnorm(n, mean = p + A + A * p ^ 2, sd = 3)) %>%
    # Truncate
    mutate(pi = case_when(pi < trunc ~ trunc,
                          pi > 1 - trunc ~ 1 - trunc,
                          T ~ pi)) %>%
    group_by(A) %>%
    summarize(estimate = weighted.mean(Y, w = 1 / pi)) %>%
    pivot_wider(names_from = "A", values_from = "estimate") %>%
    mutate(difference = `1` - `0`)
  return(data.frame(trunc = trunc, error = estimate$difference - truth))
}

library(doParallel)
library(doRNG)
sims <- foreach(rep = 1:1000, .combine = "rbind", .packages = "tidyverse") %dorng% {
  foreach(trunc_value = seq(0,.4,.05), .combine = "rbind") %do% {
    simulate(trunc = trunc_value)
  }
}

sims %>%
  group_by(trunc) %>%
  summarize(bias = mean(error),
            mse = mean(error ^ 2)) %>%
  pivot_longer(cols = c("bias","mse")) %>%
  ggplot(aes(x = trunc, y = value, color = name)) +
  geom_point() +
  geom_smooth(method = "loess", se = F) +
  facet_wrap(~name, ncol = 1, scales = "free_y")

n <- 1000


truth <- mean((rbeta(1e6,.1,.1) - .5) ^ 2)

results <- foreach(rep = 1:100, .combine = "rbind") %do% {
  foreach(trunc = seq(0,.2,.01), .combine = "rbind") %do% {
    sim <- data.frame(p = rbeta(n,.1,.1)) %>%
      mutate(A = rbinom(n,1,p)) %>%
      mutate(Y = - (p - .5) ^ 2 + A * (p - .5) ^ 2)
    sim %>%
      group_by(A) %>%
      mutate(pi = ifelse(A == 1, p, 1 - p),
             pi = case_when(pi < trunc ~ trunc,
                            pi <= (1 - trunc) ~ pi,
                            T ~ (1 - trunc))) %>%
      mutate(w = 1 / pi) %>%
      summarize(ybar = weighted.mean(Y, w = w)) %>%
      pivot_wider(names_from = "A", values_from = "ybar") %>%
      mutate(effect = `1` - `0`) %>%
      select(effect) %>%
      mutate(trunc = trunc)
  }
}

results %>%
  group_by(trunc) %>%
  summarize(effect = mean(effect)) %>%
  ggplot(aes(x = trunc, y = effect)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = truth)



