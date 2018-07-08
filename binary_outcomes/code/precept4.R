
setwd("/Users/iandl/Dropbox/SOC504/2018 materials/Precept/Precept4")
library(tidyverse)
library(reshape2)
library(mvtnorm)
library(foreign)

## EVICTION EXAMPLE

d <- read.dta("data/ffEviction.dta")

## Look at the data
head(d)
summary(d)

## Define the log likelihood function
cloglog.loglik <- function(par, X, y) {
  beta <- par
  log.lik <- sum(y * log(1 - exp(-exp(X %*% beta))) -
                   (1 - y) * exp(X %*% beta))
  return(log.lik)
}

## Find the MLE
X <- model.matrix(~married + race + income,
                  data = d)

opt <- optim(par = rep(0, ncol(X)),
             fn = cloglog.loglik,
             X = X,
             y = d$ev,
             control = list(fnscale = -1),
             hessian = T,
             method = "BFGS")

## Find the standard errors
-solve(opt$hessian)
sqrt(diag(-solve(opt$hessian)))

niceHessian <- -solve(opt$hessian)
rownames(niceHessian) <- colnames(niceHessian) <- colnames(X)
xtable(niceHessian, digits = 3)
round(sqrt(diag(niceHessian)),3)

## Make a table of results
print(xtable::xtable(data.frame(
  Variable = c("Intercept","Married","Black","Hispanic","Other","Income / poverty line"),
  Coefficient = opt$par,
  SE = sqrt(diag(-solve(opt$hessian)))
)), include.rownames = F)

## Calculate our quantity of interest
get.pred.prob <- function(setX, beta) {
  ## Calculate the linear predictor
  eta <- setX %*% beta
  ## Transform by the inverse link function
  prob <- 1 - exp(-exp(eta))
  return(prob)
}
## Let's make predictions for one white child born to married parents
## with family income at the poverty line
colnames(X)
setX <- c(1, 1, 0, 0, 0, 1)
pi_hat <- get.pred.prob(setX = setX, beta = opt$par)

## Simulate one draw from the sampling distribution of the coefficients
draw.sim.prob <- function(setX, beta_hat, vcov_beta_hat) {
  beta_tilde <- t(rmvnorm(n = 1,
                          mean = beta_hat,
                          sigma = vcov_beta_hat))
  prob_tilde <- get.pred.prob(setX = setX, beta = beta_tilde)
  return(prob_tilde)
}
draw.sim.prob(setX = setX, beta_hat = opt$par,
              vcov_beta_hat = -solve(opt$hessian))

## Simulate at least 10,000 draws from the sampling distribution of the coefficients
beta_hat <- opt$par
vcov_beta_hat <- -solve(opt$hessian)
set.seed(08544)
draws <- replicate(20000, draw.sim.prob(setX = setX, beta_hat = beta_hat,
                                        vcov_beta_hat = vcov_beta_hat))

## Make a plot of the distribution

dens <- density(draws)
data.frame(x = dens$x, y = dens$y) %>%
  mutate(ci = factor((x > quantile(draws, .025)) + 
                       (x > quantile(draws, .975)))) %>%
  ggplot(aes(x = x, ymin = 0, ymax = y, fill = ci)) +
  geom_ribbon(show.legend = F, alpha = .6) +
  scale_fill_manual(values = c("blue","seagreen4","blue")) +
  geom_vline(xintercept = pi_hat, linetype = "dashed") +
  xlab("Probability of eviction") +
  ylab("Density") +
  ggsave("figs/pred_prob.pdf",
         height = 3, width = 4)

## Look at our results in tabular form
pi_hat
quantile(draws, prob = c(.025, .975))

###############################
## NEW QOI: FIRST DIFFERENCE ##
###############################

## Calculate our quantity of interest
get.pred.prob <- function(setX, beta) {
  ## Calculate the linear predictor
  eta <- setX %*% beta
  ## Transform by the inverse link function
  prob <- 1 - exp(-exp(eta))
  return(prob)
}
## Let's make predictions for one white child born to married parents
## with family income at half the poverty line (deep poverty) vs. twice the poverty line
colnames(X)
setX <- rbind(deep_poverty = c(1, 1, 0, 0, 0, .5),
              twice_poverty = c(1, 1, 0, 0, 0, 2))
pi_hat <- get.pred.prob(setX = setX, beta = opt$par)

get.pred.diff <- function(setX, beta) {
  probs <- get.pred.prob(setX, beta)
  difference <- probs[2] - probs[1]
  return(difference)
}
difference_hat <- get.pred.diff(setX = setX, beta = opt$par)

## Simulate one draw from the sampling distribution of the coefficients
draw.sim.diff <- function(setX, beta_hat, vcov_beta_hat) {
  beta_tilde <- t(rmvnorm(n = 1,
                          mean = beta_hat,
                          sigma = vcov_beta_hat))
  difference_tilde <- get.pred.diff(setX = setX, beta = beta_tilde)
  return(difference_tilde)
}
draw.sim.diff(setX = setX, beta_hat = opt$par,
              vcov_beta_hat = -solve(opt$hessian))

## Simulate at least 10,000 draws from the sampling distribution of the coefficients
beta_hat <- opt$par
vcov_beta_hat <- -solve(opt$hessian)
set.seed(08544)
draws <- replicate(20000,
                   draw.sim.diff(setX = setX, beta_hat = beta_hat,
                                 vcov_beta_hat = vcov_beta_hat))

## Make a plot of the distribution
dens <- density(draws)
data.frame(x = dens$x, y = dens$y) %>%
  mutate(ci = factor((x > quantile(draws, .025)) + 
                       (x > quantile(draws, .975)))) %>%
  ggplot(aes(x = x, ymin = 0, ymax = y, fill = ci)) +
  geom_ribbon(show.legend = F, alpha = .6) +
  scale_fill_manual(values = c("blue","seagreen4","blue")) +
  geom_vline(xintercept = difference_hat, linetype = "dashed") +
  xlab("Difference in P(Eviction)\nTwice poverty line - 1/2 poverty line") +
  ylab("Density") +
  ggsave("figs/pred_diff.pdf",
         height = 3, width = 4)

## Look at our results in tabular form
difference_hat
quantile(draws, prob = c(.025, .975))

#####################################
## NEW QOI: CUMULATIVE PROBABILITY ##
#####################################

## Calculate a new QOI: Cumulative probability of eviction
## from birth through age 9
## Calculate our quantity of interest
get.pred.cum <- function(setX, beta, weights) {
  ## Calculate the linear predictor
  eta <- setX %*% beta
  ## Transform by the inverse link function
  prob_annual <- 1 - exp(-exp(eta))
  prob_cum <- 1 - (1 - prob_annual) ^ 9
  return(weighted.mean(prob_cum,
                       w = weights))
  return(prob_cum)
}
## Let's make predictions for every child and take a weighted average
cum_hat <- get.pred.cum(setX = X, beta = opt$par, weights = d$m1natwt)

## Simulate one draw from the sampling distribution of the coefficients
draw.sim.cum <- function(setX, beta_hat, vcov_beta_hat, weights) {
  beta_tilde <- t(rmvnorm(n = 1,
                          mean = beta_hat,
                          sigma = vcov_beta_hat))
  cum_tilde <- get.pred.cum(setX = setX, beta = beta_tilde, weights = weights)
  return(cum_tilde)
}
draw.sim.cum(setX = X, beta_hat = opt$par,
             vcov_beta_hat = -solve(opt$hessian),
             weights = d$m1natwt)

## Simulate at least 10,000 draws from the sampling distribution of the coefficients
beta_hat <- opt$par
vcov_beta_hat <- -solve(opt$hessian)
set.seed(08544)
draws <- replicate(20000, draw.sim.cum(setX = X, beta_hat = beta_hat,
                                       vcov_beta_hat = vcov_beta_hat,
                                       weights = d$m1natwt))

## Make a plot of the distribution

dens <- density(draws)
data.frame(x = dens$x, y = dens$y) %>%
  mutate(ci = factor((x > quantile(draws, .025)) + 
                       (x > quantile(draws, .975)))) %>%
  ggplot(aes(x = x, ymin = 0, ymax = y, fill = ci)) +
  geom_ribbon(show.legend = F, alpha = .6) +
  scale_fill_manual(values = c("blue","seagreen4","blue")) +
  geom_vline(xintercept = cum_hat, linetype = "dashed") +
  xlab("Probability of eviction") +
  ylab("Density") +
  ggsave("figs/pred_cum.pdf",
         height = 3, width = 4)

## Look at our results in tabular form
cum_hat
quantile(draws, prob = c(.025, .975))
