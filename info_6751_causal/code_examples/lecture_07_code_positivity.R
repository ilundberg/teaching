
library(tidyverse)
n <- 10000
set.seed(14850)

# Note: In class we actually discussed a published paper, but I can't
# share the data from that paper directly. This code applies the ideas
# from class to a fully simulated dataset.

# This code illustrates nonparametric average treatment effect estimation
# by conditioning on pre-treatment variables to block backdoor paths.
# It relies on the tidyverse, but you could do it other ways.
# To learn code in this style, see R for Data Science by Wickham & Grolemund (https://r4ds.had.co.nz/)

# See Part 3 for a more compact version of the code (tighter, but the R is more complicated)

############################
# PART 1: POSITIVITY HOLDS #
############################

# Simulate some data
sim_data <- data.frame(x1 = rbinom(n,1,.5),
                       x2 = rbinom(n,1,.5),
                       x3 = rbinom(n,1,.5)) %>%
  # Generate the treatment
  mutate(a = rbinom(n,1,plogis(x1 + x2 + x3))) %>%
  # Generate the outcome
  mutate(y = rnorm(n, x1 + x2 + x3 + a))

# Estimate the outcomes under treatment in each subgroup
ybar1 <- sim_data %>%
  filter(a == 1) %>%
  group_by(x1, x2, x3) %>%
  summarize(cases1 = n(),
            ybar1 = mean(y),
            .groups = "drop")

# Estimate the outcomes under control in each subgroup
ybar0 <- sim_data %>%
  filter(a == 0) %>%
  group_by(x1, x2, x3) %>%
  summarize(cases0 = n(),
            ybar0 = mean(y),
            .groups = "drop")

# Combine those to estimate conditional average treatment effects
cate <- ybar1 %>%
  full_join(ybar0, by = c("x1","x2","x3")) %>%
  mutate(cases = cases0 + cases1,
         effect = ybar1 - ybar0)

# Estimate the average effect
average_effect <- cate %>%
  ungroup() %>%
  summarize(average_effect = weighted.mean(effect, w = cases),
            .groups = "drop")

####################################
# PART 2: POSITIVITY DOES NOT HOLD #
####################################

# Simulate some data
sim_data <- data.frame(x1 = rbinom(n,1,.5),
                       x2 = rbinom(n,1,.5),
                       x3 = rbinom(n,1,.5)) %>%
  # Generate the treatment
  mutate(a = rbinom(n,1,plogis(x1 + x2 + x3)),
         # enforce 0 positivity when all x1, x2, x3 hold
         a = ifelse(x1 & x2 & x3, F, a)) %>%
  # Generate the outcome
  mutate(y = rnorm(n, x1 + x2 + x3 + a))

# See for what proportion of the population positivity holds
sim_data %>%
  group_by(x1,x2,x3) %>%
  mutate(positivity_holds = any(a == 1) & any(a == 0)) %>%
  ungroup() %>%
  summarize(positivity_holds = mean(positivity_holds),
            .groups = "drop")

# Estimate in the feasible subsample
sim_data_feasible <- sim_data %>%
  group_by(x1,x2,x3) %>%
  filter(any(a == 1) & any(a == 0))

# Estimate the mean outcome under treatment in each covariate stratum
ybar1 <- sim_data_feasible %>%
  filter(a == 1) %>%
  group_by(x1, x2, x3) %>%
  summarize(cases1 = n(),
            ybar1 = mean(y),
            .groups = "drop")

# Estimate the mean outcome under control in each covariate stratum
ybar0 <- sim_data_feasible %>%
  filter(a == 0) %>%
  group_by(x1, x2, x3) %>%
  summarize(cases0 = n(),
            ybar0 = mean(y),
            .groups = "drop")

# Estimate the conditional average treatment effect in each stratum
cate <- ybar1 %>%
  full_join(ybar0, by = c("x1","x2","x3")) %>%
  group_by(x1,x2,x3) %>%
  mutate(cases = cases0 + cases1,
         effect = ybar1 - ybar0)

# Estimate the average effect in the feasible subsample
feasible_average_effect <- cate %>%
  ungroup() %>%
  summarize(average_effect = weighted.mean(effect, w = cases))


#######################################
# PART 3: ALTERNATIVE CODING APPROACH #
#######################################

# The above was designed to be maximally readable.
# The below is an alternative way to code this which is more efficient (fewer lines)
# but requires greater knowledge of tidyverse coding.

# Simulate data
sim_data <- data.frame(x1 = rbinom(n,1,.5),
                       x2 = rbinom(n,1,.5),
                       x3 = rbinom(n,1,.5)) %>%
  # Generate the treatment
  mutate(a = rbinom(n,1,plogis(x1 + x2 + x3))) %>%
  # Generate the outcome
  mutate(y = rnorm(n, x1 + x2 + x3 + a))

# Define the confounders
confounders <- c("x1","x2","x3")

# Count cases in each stratum
strata_counts <- sim_data %>%
  # Group by the confounders
  group_by(across(all_of(confounders))) %>%
  # Count the number of cases
  summarize(cases = n(),
            .groups = "drop")

# Estimate effect in each stratum
strata_effects <- sim_data %>%
  # Group by the confounders and treatment
  group_by(a, across(all_of(confounders))) %>%
  # Estimate the mean outcome
  summarize(ybar = mean(y),
            .groups = "drop") %>%
  # Prepare to make the data wider by re-valuing the treatment
  mutate(a = paste0("ybar_",a)) %>%
  # We now have a column "a" for treatment and
  # a column "ybar" for the mean outcome.
  # Pivot wider to have one row per stratum
  # with column "ybar_1" and "ybar_0" for
  # the outcome under treatment and under control
  pivot_wider(names_from = "a", values_from = "ybar") %>%
  # Estimate the effect
  mutate(conditional_effect = ybar_1 - ybar_0)

# Aggregate over strata
strata_counts %>%
  # Merge the effects into the counts data frame
  full_join(strata_effects, by = confounders) %>%
  # Stop working within strata. Average the effect
  ungroup() %>%
  summarize(average_effect = weighted.mean(conditional_effect, 
                                           w = cases))