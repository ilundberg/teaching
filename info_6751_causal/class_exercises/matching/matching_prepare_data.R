
# This code generates a simulated population for a matching exercise.

library(tidyverse)
set.seed(14850)

#################################################################
# 1. Simulate a population from a known data generating process #
#################################################################

# Set the population size at 100,000
N_population <- 100000

# Confounder L1 and L2 are exponential (distribution with long right tail)
data_population <- data.frame(L1 = rnorm(N_population),
                              L2 = rnorm(N_population)) %>%
  # Generate potential outcomes as functions of L
  mutate(Y0 = rnorm(n(), mean = L1 + L2, sd = 1),
         Y1 = rnorm(n(), mean = Y0 + 1, sd = 1)) %>%
  # Generate treatment as a function of L
  mutate(propensity_score = plogis(-2 + L1 + L2)) %>%
  mutate(A = rbinom(n(), 1, propensity_score)) %>%
  # Generate factual outcome
  mutate(Y = case_when(A == 0 ~ Y0,
                       A == 1 ~ Y1))

write_csv(data_population, file = "data_population.csv")
