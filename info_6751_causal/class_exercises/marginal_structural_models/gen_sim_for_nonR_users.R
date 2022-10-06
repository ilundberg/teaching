
set.seed(14850)
library(tidyverse)
library(doParallel)
library(doRNG)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

generate_data <- function(n) {
  L0_set <- rep(1:3, c(1,1,8))
  L1_set <- rep(1:3, c(8,1,1))
  
  d <- data.frame(L = 0,
                  A = rep(L0_set, ceiling(n / (2*10)))) %>%
    bind_rows(data.frame(L = 1,
                         A = rep(L1_set, ceiling(n / (2*10))))) %>%
    filter(1:n() <= n) %>%
    mutate(Y = rnorm(n(), 
                     mean = case_when(L == 0 ~ - (A - 2) ^ 2,
                                      L == 1 ~ (A - 2) ^ 2),
                     sd = 1))
}

generate_data_for_stabilized <- function(n) {
  L0_set <- rep(1:3, c(4,1,5))
  L1_set <- rep(1:3, c(8,1,1))
  
  d <- data.frame(L = 0,
                  A = rep(L0_set, ceiling(n / (2*10)))) %>%
    bind_rows(data.frame(L = 1,
                         A = rep(L1_set, ceiling(n / (2*10))))) %>%
    filter(1:n() <= n) %>%
    mutate(Y = rnorm(n(), 
                     mean = case_when(L == 0 ~ - (A - 2) ^ 2,
                                      L == 1 ~ (A - 2) ^ 2),
                     sd = 1))
}

sim <- foreach(r = 1:1000, .combine = "rbind", .packages = "tidyverse") %dorng% {
  generate_data(n = 100) %>%
    mutate(r = r) %>%
    select(r,L,A,Y)
}
write_csv(sim, file = "sim.csv")

sim_stabilized <- foreach(r = 1:10000, .combine = "rbind", .packages = "tidyverse") %dorng% {
  generate_data(n = 100) %>%
    mutate(r = r) %>%
    select(r,L,A,Y)
}
write_csv(sim_stabilized, file = "sim_stabilized.csv")
