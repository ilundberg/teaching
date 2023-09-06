

## Precept 6 code
## Soc 504
## Ian Lundberg
## Last updated: 13 March 2016

library(tidyverse)
library(reshape2)
library(pscl)

setwd("/Users/iandl/Dropbox/SOC504/2018 materials/Precept/Precept6/figs")

## Show Uniform distribution
data.frame(u = runif(10000)) %>%
  ggplot(aes(x = u)) +
  geom_density() +
  xlab("U") +
  ylab("Density") +
  ggsave("unif.pdf", height = 3, width = 4)

## Show log(U) distribution
data.frame(u = runif(10000)) %>%
  ggplot(aes(x = log(u))) +
  geom_density() +
  xlab("log(U)") +
  ylab("Density") +
  ggsave("logUnif.pdf", height = 3, width = 4)

## Show exponential distribution
data.frame(u = runif(10000)) %>%
  ggplot(aes(x = -log(u))) +
  geom_density() +
  xlab("-log(U)") +
  ylab("Density") +
  ggsave("minusLogUnif.pdf", height = 3, width = 4)

## Exponential hazard curves
data.frame(t = seq(0,3,.01)) %>%
  mutate(h1 = dexp(t) / pexp(t, lower.tail = F),
         h2 = dexp(t, rate = 2) / pexp(t, rate = 2, lower.tail = F),
         h3 = dexp(t, rate = 3) / pexp(t, rate = 3, lower.tail = F)) %>%
  melt(id = "t") %>%
  ggplot(aes(x = t, y = value, color = variable)) +
  geom_line() +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Hazard h(t)") +
  ggsave("ExpoHazards.pdf",
         height = 3, width = 4)

## Weibull hazard curves
data.frame(t = seq(0.1,3,.01)) %>%
  mutate(h1 = dweibull(t, shape = .7) / pweibull(t, shape = .7, lower.tail = F),
         h2 = dweibull(t, scale = 2, shape = .7) / pweibull(t, scale = 2, shape = .7, lower.tail = F),
         h3 = dweibull(t, scale = 3, shape = .7) / pweibull(t, scale = 3, shape = .7, lower.tail = F)) %>%
  melt(id = "t") %>%
  ggplot(aes(x = t, y = value, color = variable)) +
  geom_line() +
  theme(legend.position = "none") +
  xlab("Time") + ylab("Hazard h(t)") +
  ggsave("WeibullHazards.pdf",
         height = 3, width = 4)
  

## Show Gompertz hazard and log hazard
data.frame(Age = seq(0,50,1)) %>%
  mutate(Hazard = exp(-7 + .09*Age)) %>%
  ggplot(aes(x = Age, y = Hazard)) +
  geom_line() +
  ggtitle(expression(Gompertz~hazard~"\n("~alpha==-7~","~beta==.09~")")) +
  ggsave("GompertzHazard.pdf",
         height = 3, width = 4)

data.frame(Age = seq(0,50,1)) %>%
  mutate(Hazard = exp(-7 + .09*Age)) %>%
  ggplot(aes(x = Age, y = Hazard)) +
  geom_line() +
  ylab("Hazard of new break") +
  xlab("Minutes since last break") +
  scale_x_continuous()
  ggtitle(expression(Gompertz~hazard~"\n("~alpha==-7~","~beta==.09~")")) +
  ggsave("GompertzHazard_hiking.pdf",
         height = 3, width = 3)

data.frame(Age = seq(0,50,1)) %>%
  mutate(`Log hazard` = -7 + .09*Age) %>%
  ggplot(aes(x = Age, y = `Log hazard`)) +
  geom_line() +
  ggtitle(expression(Gompertz~log~hazard~"\n("~alpha==-7~","~beta==.09~")")) +
  ggsave("GompertzLogHazard.pdf",
         height = 3, width = 4)


## Lung cancer example
library(survival)
data(lung)
head(lung)
lung <- mutate(lung, event = as.numeric(status == 2))

library(Zelig)
fit <- zelig(Surv(time, event) ~ age + sex,
             model = "exp",
             data = lung)
## We will compare expected survival for
## men and women of average ages
men <- setx(fit, sex = 1, fn = mean)
women <- setx(fit, sex = 2, fn = mean)

sims <- sim(obj = fit, x = men, x1 = women)
summary(sims)
pdf("ZeligFigures.pdf",
    height = 10, width = 14)
plot(sims)
dev.off()

## Fitting an exponential model with survreg
library(survival)
fit <- survreg(Surv(time, event) ~ age + sex,
               dist = "exponential",
               data = lung)
summary(fit)
## Extract hazard ratios
round(exp(-coef(fit)),3)
## Plot hazards and survival curves
lambda <- exp(-predict(
  fit,
  newdata = data.frame(
    age = c(50,50),
    sex = c(1,2)
), type = "lp"))
data.frame(t = 1:365) %>%
  mutate(Men.Hazard = lambda[1],
         Women.Hazard = lambda[2],
         Men.Survival = exp(-lambda[1]*t),
         Women.Survival = exp(-lambda[2]*t)) %>%
  melt(id = "t") %>%
  separate(variable, into = c("Sex","QOI")) %>%
  ggplot(aes(x = t, y = value, color = Sex)) +
  geom_line() +
  facet_wrap(~QOI, scales = "free") + ylab("") + xlab("time") +
  ggtitle("Exponential survival fits\nfor 50-year-old men and women") +
  ggsave("ExpoFit.pdf",
         height = 3, width = 5)

## WEIBULL
fit <- survreg(Surv(time, event) ~ age + sex,
               dist = "weibull",
               data = lung)
summary(fit)
## Plot hazards and survival curves
lambda <- exp(-predict(
  fit,
  newdata = data.frame(
    age = c(50,50),
    sex = c(1,2)
  ), type = "lp"))
hweibull <- function(t, lambda, alpha) {
  hazard <- dweibull(x = t, shape = alpha, scale = 1 / lambda) /
    (1 - pweibull(q = t, shape = alpha, scale = 1 / lambda))
  return(hazard)
}
alpha <- 1/fit$scale
data.frame(t = 1:365) %>%
  mutate(Men.Hazard = hweibull(t,lambda[1],alpha),
         Women.Hazard = hweibull(t,lambda[2],alpha),
         Men.Survival = pweibull(q = t, shape = alpha, scale = 1 / lambda[1],
                                 lower.tail = F),
         Women.Survival = pweibull(q = t, shape = alpha, scale = 1 / lambda[2],
                                   lower.tail = F)) %>%
  melt(id = "t") %>%
  separate(variable, into = c("Sex","QOI")) %>%
  ggplot(aes(x = t, y = value, color = Sex)) +
  geom_line() +
  facet_wrap(~QOI, scales = "free") + ylab("") + xlab("time") +
  ggtitle("Weibull survival fits, for 50-year-old men and women") +
  ggsave("WeibullFit.pdf",
         height = 3, width = 5)

## LOGNORMAL
fit <- survreg(Surv(time, event) ~ age + sex,
               dist = "lognormal",
               data = lung)
summary(fit)
## Plot hazards and survival curves
mu <- predict(
  fit,
  newdata = data.frame(
    age = c(50,50),
    sex = c(1,2)
  ), type = "lp")
hlnorm <- function(t, meanlog, sdlog) {
  hazard <- dlnorm(x = t, meanlog, sdlog) /
    plnorm(q = t, meanlog, sdlog,
           lower.tail = F)
  return(hazard)
}
data.frame(t = 1:365) %>%
  mutate(Men.Hazard = hlnorm(t,mu[1],log(fit$scale)),
         Women.Hazard = hlnorm(t,mu[2],log(fit$scale)),
         Men.Survival = plnorm(t,mu[1],log(fit$scale),
                               lower.tail = F),
         Women.Survival = plnorm(t,mu[2],log(fit$scale),
                                 lower.tail = F)) %>%
  melt(id = "t") %>%
  separate(variable, into = c("Sex","QOI")) %>%
  ggplot(aes(x = t, y = value, color = Sex)) +
  geom_line() +
  facet_wrap(~QOI, scales = "free") + ylab("") + xlab("time") +
  ggtitle("LogNormal survival fits, for 50-year-old men and women") +
  ggsave("LogNormalFit.pdf",
         height = 3, width = 5)
## That looks bad, so just make an example to show
data.frame(Time = seq(0,10,.1)) %>%
  mutate(Hazard = hlnorm(Time,1.1,1),
         Survival = plnorm(Time,1.1,1,lower.tail = F)) %>%
  melt(id = "Time") %>%
  ggplot(aes(x = Time, y = value)) +
  geom_line() +
  facet_wrap(~variable) +
  ggsave("LogNormalExample.pdf",
         height = 3, width = 5)

## Horseshoe crab example for 0-inflation
d <- read.table("http://www.stat.ufl.edu/~aa/glm/data/Crabs.dat",
                header = T)
head(d)
ggplot(d, aes(x = y)) +
  geom_histogram(bins = 8) +
  xlab("Number of satellites") +
  ggtitle("Satellites around female horseshoe crabs") +
  ggsave("SatelliteHistogram.pdf",
         height = 3, width = 5)

d <- mutate(
  d,
  color = factor(color),
  spine = factor(spine)
)

## Define our predictors and outcome
X <- model.matrix(~weight + width,
                  data = d)
y <- d$y

## Likelihood function for negative binomial
nb.loglik <- function(par, y, X) {
  ## k will indicate the number of predictors
  k <- ncol(X)
  gamma <- par[1:k]
  theta <- exp(par[(k + 1)])
  lambda <- exp(X %*% gamma)
  ## Use the gamma-poisson mixture specification
  log.lik <- sum(lgamma(theta + y) - lgamma(theta) + 
                   y*log(lambda) - y*log(lambda + theta) +
                   theta*log(theta) - theta*(log(lambda + theta)))
  return(log.lik)
}

opt.nb <- optim(par = rep(0, ncol(X) + 1),
                y = y,
                X = X,
                fn = nb.loglik,
                method = "BFGS",
                control = list(fnscale = -1), 
                hessian = TRUE)

summary(glm.nb(y ~ -1 + X))
opt.nb$par

## Likelihood function for zero-inflated negative binomial
zinb.loglik <- function(par, y, X) {
  ## k will indicate the number of predictors
  k <- ncol(X)
  beta <- par[1:k]
  gamma <- par[(k + 1):(2*k)]
  theta <- exp(par[(2*k + 1)])
  p <- plogis(X %*% beta)
  lambda <- exp(X %*% gamma)
  log.lik <- sum(log(
    (y == 0)*(1 - p) + 
      dnbinom(y, size = theta, mu = lambda) * p
  ))
  return(log.lik)
}

zinb.loglik(rep(0, 2*ncol(X) + 1), y, X)

## Optimize the log-likelihood
opt.zinb <- optim(par = rep(0, 2*ncol(X) + 1),
                  y = y,
                  X = X,
                  fn = zinb.loglik,
                  method = "BFGS",
                  control = list(fnscale = -1), 
                  hessian = TRUE)

## Report coefficients and standard errors
results <- data.frame(
  Predictor = c("Intercept","Weight","Width"),
  Beta = opt.zinb$par[1:3],
  SE.Beta = sqrt(diag(-solve(opt.zinb$hessian)))[1:3],
  Gamma = opt.zinb$par[4:6],
  SE.Gamma = sqrt(diag(-solve(opt.zinb$hessian)))[4:6]
)
print(xtable(results),
      include.rownames = F)
theta <- exp(opt.zinb$par[7])

## Simulate
sim.par <- mvrnorm(
  10000,
  mu = opt.zinb$par,
  Sigma = -solve(opt.zinb$hessian)
)

## Get the standard error of theta hat
sim.theta <- exp(sim.par[,7])
sd(sim.theta)

## Compare results to pscl package
library(pscl)
canned.zinb <- zeroinfl(y ~ weight, # + width + factor(color) + factor(spine),
                        data = d,
                        dist = "negbin")


data.frame(x = seq(0,1,.01)) %>% 
  mutate(density = dbeta(x,3,1)) %>%
  ggplot(aes(x = x, ymax = density, ymin = 0)) + 
  geom_ribbon(alpha = .4, fill = "blue") +
  xlab("Value") +
  ylab("Density") +
  ggsave("/Users/iandl/Dropbox/SOC504/2018 materials/Precept/Precept6/figs/beta_3_1.pdf",
         height = 2, width = 3)

