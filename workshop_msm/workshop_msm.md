Workshop: Marginal Structural Modeling
================
Ian Lundberg
2023-11-17







This page introduces code for marginal structural models through an
intentionally simple application of marginal structural models for
treatments that unfold over two time periods. To get started, you will
want to prepare your R environment.

``` r
library(tidyverse)
```

> **Stata users.** Analogous code is
> [here](https://github.com/ilundberg/teaching/tree/master/workshop_msm/msm_stata.do)

## Causal goal and identification assumptions

Our causal estimand is the average outcome $E(Y^{a_1,a_2})$ that would
be realized under an intervention to set the period-1 treatment $A_1$ to
the value $a_1$ and the period-2 treatment $A_2$ to the value $a_2$.

For identification, we will assume sequential ignorability, which
effectively requires that treatment be unconfounded given the measured
past.

## Simulate data

The function below simulates data from a known data generating process.

``` r
simulate <- function(n = 100) {
  data.frame(X1 = rnorm(n)) %>%
    mutate(A1 = rbinom(n, 1, plogis(.25*X1)),
           U2 = rnorm(n),
           X2 = rnorm(n, mean = -.25 + .5*A1 + U2),
           A2 = rbinom(n, 1, plogis(.25*X2)),
           Y = rnorm(n, X1 + A1 + X2 + A2 + U2))
}
```

Here we generate one sample of size 100.

``` r
simulated <- simulate(n = 100)
```

``` r
print(head(simulated), digits = 1)
```

    ##     X1 A1   U2   X2 A2    Y
    ## 1 -1.2  0 -0.6 -0.7  0 -1.9
    ## 2 -0.4  0  0.6  0.2  0 -0.3
    ## 3 -1.5  1  0.7  2.6  1  4.6
    ## 4  0.9  1 -1.1 -1.4  0  0.4
    ## 5 -0.5  1  0.4 -0.3  1  1.6
    ## 6  1.0  1 -1.0  1.3  1  3.9

## Model treatment assignment

Using logistic regression, we model the treatment assignment
probabilities given the measured past.

``` r
fit_A1 <- glm(A1 ~ X1,
              family = binomial,
              data = simulated)
fit_A2 <- glm(A2 ~ A1 + X2,
              family = binomial,
              data = simulated)
```

We then create a new dataset with the probability of the observed
treatment at each period.

- when treatment = 1, this is the probability of treatment = 1
- when treatment = 0, this is the probability of treatment = 0

``` r
with_pi <- simulated %>%
  # First predict the probability that treatment equals 1
  mutate(P_A1_equals_1 = predict(fit_A1, type = "response"),
         P_A2_equals_1 = predict(fit_A2, type = "response")) %>%
  # Then modify to create pi1 and pi2, which are the probability
  # of whatever treatment was observed at time 1 and 2
  mutate(pi1 = case_when(A1 == 1 ~ P_A1_equals_1,
                         A1 == 0 ~ 1 - P_A1_equals_1),
         pi2 = case_when(A2 == 1 ~ P_A2_equals_1,
                         A2 == 0 ~ 1 - P_A2_equals_1))
```

## Create inverse probability weights

The inverse probability of treatment weight is

- the product over time periods of
- the inverse probability of treatment at each period

``` r
with_weight <- with_pi %>%
  mutate(weight1 = 1 / pi1,
         weight2 = 1 / pi2,
         weight = weight1 * weight2)
```

## Inverse probability weighted estimator

With no additional modeling, we can estimate potential outcomes by
inverse probability weighting.

Total effect of treatment at time 1,

``` r
with_weight %>%
  group_by(A1) %>%
  summarize(estimate = weighted.mean(Y, w = weight1))
```

    ## # A tibble: 2 × 2
    ##      A1 estimate
    ##   <int>    <dbl>
    ## 1     0   -0.126
    ## 2     1    1.34

Total effect of treatment at time 2,

``` r
with_weight %>%
  group_by(A2) %>%
  summarize(estimate = weighted.mean(Y, w = weight2))
```

    ## # A tibble: 2 × 2
    ##      A2 estimate
    ##   <int>    <dbl>
    ## 1     0   0.0746
    ## 2     1   1.43

Effect of joint intervention on $A_1$ and $A_2$,

``` r
with_weight %>%
  group_by(A1,A2) %>%
  summarize(estimate = weighted.mean(Y, w = weight))
```

    ## `summarise()` has grouped output by 'A1'. You can override using the `.groups`
    ## argument.

    ## # A tibble: 4 × 3
    ## # Groups:   A1 [2]
    ##      A1    A2 estimate
    ##   <int> <int>    <dbl>
    ## 1     0     0   -0.765
    ## 2     0     1    0.930
    ## 3     1     0    0.943
    ## 4     1     1    1.96

## Marginal structural modeling estimator

By imposing additional assumptions through a parametric model for the
potential outcomes, we can reduce our statistical uncertainty. To do so,
we estimate a weighted regression where

- the weights adjust for confounding
- the parametric model pools information for reduced uncertainty

You can use any functional form that you want, such as a linear effect
of the cumulative treatment. In our particular data generating process,
the functional form below is correctly specified.

``` r
msm_fit <- lm(Y ~ A1 + A2,
              data = with_weight,
              weights = weight)
```

We can then make predictions from this model for any population-average
counterfactual of interest.

``` r
to_predict <- data.frame(
  A1 = c(0,0,1,1),
  A2 = c(0,1,0,1)
)
to_predict %>%
  mutate(estimate = predict(msm_fit, newdata = to_predict))
```

    ##   A1 A2   estimate
    ## 1  0  0 -0.5943946
    ## 2  0  1  0.7581789
    ## 3  1  0  0.7757050
    ## 4  1  1  2.1282785

## When you finish

If time allows, you could discuss a few things:

- can you think of a data generating process under which the marginal
  structural model above be a poor parametric choice?
- how would you apply this with many (e.g., 10) treatment periods?
- how would you design a simulation to estimate the bias and variance of
  these estimators?

## Conclusion

When studying treatments that unfold over time, there are many possible
estimators. Inverse probability of treatment weighting is particularly
intuitive compared to others. Marginal structural models are an
extension to IPW that adds a parametric model for potential outcomes to
reduce statistical uncertainty.
