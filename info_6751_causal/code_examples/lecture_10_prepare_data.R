
# Prepare data from ACIC Competition 2016 Simulation 1

# For the original data source, see
# https://github.com/vdorie/aciccomp

# Many thanks to Dorie et al. for producing these data that are
# very useful for teaching!

set.seed(14850)

library(aciccomp2016)
sim <- dgp_2016(input_2016,1,1)

# z is the treatment
# y is the outcome
# x_* are confounders

# To help students practice a train-test split,
# I am providing a dataset that is already labeled randomly
# with observations in train and test sets.

d <- data.frame(z = sim$z, y = sim$y) %>%
  bind_cols(input_2016) %>%
  mutate(set = sample(rep(c('train','test'), each = nrow(d) / 2)))

write_csv(d, file = "lec10.csv")

# See the true SATT
mean(sim$y.1 - sim$y.0)