Exercise: Inverse Probability Weighting and Marginal Structural Models
================
Ian Lundberg
2022-10-06

Note for Python users: This exercise is set up for R users. The exercise
will also work for Python users. So that Python users do not have to run
the data generation functions, I have pre-generated 1,000 samples from
`generate_data()` and 10,000 samples from
`generate_data_for_stabilized()`. These are available in
[sim.csv](https://github.com/ilundberg/teaching/raw/master/info_6751_causal/class_exercises/marginal_structural_models/sim.csv)
and
[sim_stabilized.csv](https://github.com/ilundberg/teaching/raw/master/info_6751_causal/class_exercises/marginal_structural_models/sim_stabilized.csv).
In each file, an additional column
![r](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r "r")
indexes the simulated samples. You could run your estimator on each
sample defined by
![r](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;r "r").

# Setup: Prepare environment and generate data.

Load packages.

``` r
library(tidyverse)
library(doParallel)
library(doRNG)
```

Prepare for parallel computing to speed up simulations.

``` r
cl <- makeCluster(detectCores())
registerDoParallel(cl)
```

Below is a data generating process in which

-   Confounding exists:
    -   when
        ![L = 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;L%20%3D%200 "L = 0"),
        for 80% of cases
        ![A = 3](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A%20%3D%203 "A = 3")
    -   when
        ![L = 1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;L%20%3D%201 "L = 1"),
        for 80% of cases
        ![A = 1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A%20%3D%201 "A = 1")
-   The conditional mean response surface is highly nonlinear
    -   when
        ![L = 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;L%20%3D%200 "L = 0"),
        ![E(Y \| A, L) = - (A - 2)^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%20%7C%20A%2C%20L%29%20%3D%20-%20%28A%20-%202%29%5E2 "E(Y | A, L) = - (A - 2)^2")
    -   when
        ![L = 1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;L%20%3D%201 "L = 1"),
        ![E(Y \| A, L) = (A - 2)^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%20%7C%20A%2C%20L%29%20%3D%20%28A%20-%202%29%5E2 "E(Y | A, L) = (A - 2)^2")
-   The structural response surface marginalized over
    ![L](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;L "L")
    is linear
    -   ![E(Y^a) = 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5Ea%29%20%3D%200 "E(Y^a) = 0")
        for all
        ![a](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;a "a")

This is an intentionally-chosen setting designed so that marginal
structural models will perform well.

``` r
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
```

![](msm_exercise_solutions_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

In the data generating process above, the true
![E(Y^a) = 0](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5Ea%29%20%3D%200 "E(Y^a) = 0")
at each value of
![a \\in \\{1,2,3\\}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;a%20%5Cin%20%5C%7B1%2C2%2C3%5C%7D "a \in \{1,2,3\}").

``` r
truth <- data.frame(A = 1:3, truth = 0)
```

# 1. Define estimators

Define three estimators that will apply to samples drawn from
`generate_data()`.

1.  **Outcome estimator.** Estimate by modeling the outcome given
    treatment and confounders.
    -   Use saturated linear regression or conditional means to model
        ![Y](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y "Y")
        the outcome nonparametrically as a function of
        ![\\{L,A\\}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5C%7BL%2CA%5C%7D "\{L,A\}").
    -   Apply the
        ![g](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;g "g")-formula
        ![E(Y^a) = E(E(Y\\mid L,A = a))](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5Ea%29%20%3D%20E%28E%28Y%5Cmid%20L%2CA%20%3D%20a%29%29 "E(Y^a) = E(E(Y\mid L,A = a))").
2.  **Inverse probability weighting estimator.** Estimate by reweighting
    observed outcomes.
    -   Nonparametrically estimate the conditional probability of
        treatment,
        ![\\pi_i = P(A = a_i\\mid L = \\ell_i)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpi_i%20%3D%20P%28A%20%3D%20a_i%5Cmid%20L%20%3D%20%5Cell_i%29 "\pi_i = P(A = a_i\mid L = \ell_i)").
    -   Define a weight
        ![w_i = \\frac{1}{\\pi_i}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;w_i%20%3D%20%5Cfrac%7B1%7D%7B%5Cpi_i%7D "w_i = \frac{1}{\pi_i}")
    -   Estimate
        ![E(Y^a) = E\\left(\\frac{\\mathbb{I}(A = a)Y}{\\pi}\\right)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5Ea%29%20%3D%20E%5Cleft%28%5Cfrac%7B%5Cmathbb%7BI%7D%28A%20%3D%20a%29Y%7D%7B%5Cpi%7D%5Cright%29 "E(Y^a) = E\left(\frac{\mathbb{I}(A = a)Y}{\pi}\right)"),
        using the weighted mean of
        ![Y_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y_i "Y_i")
        among those with
        ![A_i = a](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A_i%20%3D%20a "A_i = a")
        weighted by
        ![w_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;w_i "w_i").
3.  **Marginal structural model estimator.** Estimate by a weighted
    linear regression.
    -   Nonparametrically estimate the conditional probability of
        treatment,
        ![\\pi_i = P(A = a_i\\mid L = \\ell_i)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cpi_i%20%3D%20P%28A%20%3D%20a_i%5Cmid%20L%20%3D%20%5Cell_i%29 "\pi_i = P(A = a_i\mid L = \ell_i)").
    -   Define a weight
        ![w_i = \\frac{1}{\\pi_i}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;w_i%20%3D%20%5Cfrac%7B1%7D%7B%5Cpi_i%7D "w_i = \frac{1}{\pi_i}")
    -   Estimate
        ![E(Y^a)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5Ea%29 "E(Y^a)"),
        by predicting at
        ![A = a](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A%20%3D%20a "A = a")
        from a weighted linear regression of
        ![Y_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;Y_i "Y_i")
        on
        ![A_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A_i "A_i"),
        weighted by
        ![w_i](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;w_i "w_i").

Note: Because both (2) and (3) require a weight, you might write a
function that takes a data frame and appends a weight.

``` r
# Function to conduct outcome modeling
outcome_estimator <- function(data) {
  fit <- lm(Y ~ factor(A)*L, data = data)
  result <- foreach(a = 1:3, .combine = "rbind") %do% {
    data.frame(A = a,
               estimate = mean(predict(fit, newdata = data %>% mutate(A = a))))
  }
  return(result)
}
# Function to create inverse probability weights
create_ipw <- function(data) {
  # 1. Estimate pi_i = P(A = a_i | L = l_i) for each unit
  data_with_weights <- data %>%
    # Define the size of L = l_i
    group_by(L) %>%
    mutate(L_num = n()) %>%
    # Define P(A = a_i | L = l_i) as the proportion with A = a_i
    # among the set L = l_i
    group_by(A,L) %>%
    mutate(pi = n() / L_num) %>%
    select(-L_num) %>%
    # 2. Create a weight 1 / pi_i
    mutate(w = 1 / pi) %>%
    select(-pi)
  return(data_with_weights)
}
# Function to create inverse probability weights
ipw_estimator <- function(data) {
  # Use your previous function to create inverse probability weights
  with_weights <- create_ipw(data)
  # 3. Return a data.frame with the estimated E(Y^a) for a = 1,2,3
  result <- with_weights %>%
    group_by(A) %>%
    summarize(estimate = weighted.mean(Y, w = w))
  return(result)
}
# Function to create inverse probability weights
msm_estimator <- function(data) {
  # Use your function to create inverse probability weights
  with_weights <- create_ipw(data)
  # Fit OLS for Y ~ A weighted by w_i
  fit <- lm(Y ~ A, data = with_weights, w = w)
  # Return a data.frame with the estimated E(Y^a) for a = 1,2,3
  result <- data.frame(A = 1:3)
  result$estimate <- predict(fit, newdata = result)
  return(result)
}
```

# 2. Apply your estimators to one sample from `generate_data()`

I suggest setting
![n = 100](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20%3D%20100 "n = 100").

Compare the results from the IPW estimator and the outcome estimator.
What do you notice?

Simulate one sample, apply the estimators, and report the result in a
table.

``` r
sim_data <- generate_data(100)
estimates <- outcome_estimator(sim_data) %>%
  mutate(estimator = "Outcome ") %>%
  bind_rows(ipw_estimator(sim_data) %>%
              mutate(estimator = "IPW ")) %>%
  bind_rows(msm_estimator(sim_data) %>%
              mutate(estimator = "MSM "))
# Look at results
estimates %>%
  pivot_wider(names_from = "estimator", values_from = "estimate")
```

    ## # A tibble: 3 × 4
    ##       A `Outcome ` `IPW ` `MSM `
    ##   <int>      <dbl>  <dbl>  <dbl>
    ## 1     1      0.206  0.206  0.191
    ## 2     2      0.260  0.260  0.291
    ## 3     3      0.406  0.406  0.390

IPW and outcome modeling yield the **same estimate**! This is because
these two are mathematically equivalent not only in expectation, but
also in every finite sample. This occurs only when both methods are
carried out nonparametrically.

# 3. Apply your estimators to many samples from `generate_data()`

I suggest
![R = 1000](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R%20%3D%201000 "R = 1000")
repetitions with
![n = 100](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20%3D%20100 "n = 100").

Calculate 9 mean squared error estimates for the

-   3 estimands:
    ![E(Y^1),E(Y^2),E(Y^3)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5E1%29%2CE%28Y%5E2%29%2CE%28Y%5E3%29 "E(Y^1),E(Y^2),E(Y^3)")
-   3 estimators: outcome modeling, IPW, MSM

Which estimator has the best MSE? Thinking about the data generating
process, why is the difference most dramatic for
![E(Y^2)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5E2%29 "E(Y^2)")?

Conduct the simulation.

``` r
R <- 1000
n <- 100
part3_sims <- foreach(r = 1:R, .combine = "rbind", .packages = "tidyverse") %dorng% {
  sim <- generate_data(n)
  ipw_estimator(sim) %>%
    mutate(estimator = "Outcome") %>%
    bind_rows(ipw_estimator(sim) %>%
                mutate(estimator = "IPW")) %>%
    bind_rows(msm_estimator(sim) %>%
                mutate(estimator = "MSM"))
} 
```

Calculate performance

``` r
part3_performance <- part3_sims %>%
  left_join(truth, by = "A") %>%
  group_by(A,estimator) %>%
  summarize(mse = mean((estimate - truth) ^ 2))
```

    ## `summarise()` has grouped output by 'A'. You can override using the `.groups`
    ## argument.

Visualize the result

``` r
part3_performance %>%
  ggplot(aes(x = A, y = mse, color = estimator)) +
  geom_point() +
  xlab("Treatment Value a") +
  scale_x_continuous(breaks = 1:3) +
  ylab(expression(atop('Mean Squared Error for','Estimator of '~E(Y^a)))) +
  scale_color_discrete(name = "Estimator") +
  ggtitle("Note: IPW and MSM are exactly on top of each other.") +
  theme(plot.title = element_text(size = 6))
```

![](msm_exercise_solutions_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

# 4. Vary the sample size

Repeat the simulation for
![n = 100,150,200,250,300](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20%3D%20100%2C150%2C200%2C250%2C300 "n = 100,150,200,250,300").
Visualize MSE as a function of sample size.

Think about which has better MSE: \* MSM with
![n = 100](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20%3D%20100 "n = 100")
observations \* IPW with
![n = 300](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20%3D%20300 "n = 300")
observations

Why do you think we see this result?

Conduct the simulation.

``` r
R <- 1000
part4_sims <- foreach(r = 1:R, .combine = "rbind", .packages = c("tidyverse","foreach")) %dorng% {
  foreach(n = c(100,150,200,250,300), .combine = "rbind") %do% {
    sim <- generate_data(n)
    ipw_estimator(sim) %>%
      mutate(estimator = "Outcome") %>%
      bind_rows(ipw_estimator(sim) %>%
                  mutate(estimator = "IPW")) %>%
      bind_rows(msm_estimator(sim) %>%
                  mutate(estimator = "MSM")) %>%
      mutate(n = n)
  }
} 
```

Calculate performance

``` r
part4_performance <- part4_sims %>%
  left_join(truth, by = "A") %>%
  group_by(A,estimator,n) %>%
  summarize(mse = mean((estimate - truth) ^ 2))
```

    ## `summarise()` has grouped output by 'A', 'estimator'. You can override using
    ## the `.groups` argument.

Visualize the result

``` r
part4_performance %>%
  mutate(A = paste0("a = ",A)) %>%
  ggplot(aes(x = n, y = mse, color = estimator)) +
  geom_point() +
  geom_line() +
  facet_wrap(~A) +
  xlab("Sample Size") +
  ylab(expression(atop('Mean Squared Error for','Estimator of '~E(Y^a)))) +
  scale_color_discrete(name = "Estimator") +
  ggtitle("Note: IPW and MSM are exactly on top of each other.") +
  theme(plot.title = element_text(size = 6))
```

![](msm_exercise_solutions_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

All estimators perform better as the sample size grows, but the MSM
advantage for
![E(Y^2)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5E2%29 "E(Y^2)")
is substantial. Performance of the MSM at
![n = 100](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20%3D%20100 "n = 100")
is superior to performance of the IPW estimator at
![n = 300](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20%3D%20300 "n = 300"),
despite the latter being three times the sample size.

# 5. Challenge. Stabilized weights.

Stabilized weights
![w = \\frac{P(A = a_i)}{P(A = a_i \\mid L = \\ell_i)}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;w%20%3D%20%5Cfrac%7BP%28A%20%3D%20a_i%29%7D%7BP%28A%20%3D%20a_i%20%5Cmid%20L%20%3D%20%5Cell_i%29%7D "w = \frac{P(A = a_i)}{P(A = a_i \mid L = \ell_i)}")
can yield improved efficiency when

-   ![P(A = a_i)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;P%28A%20%3D%20a_i%29 "P(A = a_i)")
    is not uniform
-   The MSM is not saturated

Below is a new data generating process to generate data that satisfy
these conditions:
![P(A = 1) \> P(A = 3)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;P%28A%20%3D%201%29%20%3E%20P%28A%20%3D%203%29 "P(A = 1) > P(A = 3)")
in this simulation.

``` r
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
```

This challenge question is to:

-   Write an estimator that calculates stabilized weights and estimates
    a MSM using the stabilized weights.
-   Apply your original MSM estimator and your stabilized estimator at
    ![n = 20](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;n%20%3D%2020 "n = 20")
    for
    ![R = 10,000](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;R%20%3D%2010%2C000 "R = 10,000")
    simulations.
-   Calculate mean squared error.

Do stabilized weights improve performance?

Define a function to create stabilized weights.

``` r
create_stabilized_ipw <- function(data) {
  # 1. Estimate pi_i = P(A = a_i | L = l_i) for each unit
  data_with_weights <- data %>%
    # Define the size of L = l_i
    group_by(L) %>%
    mutate(L_num = n()) %>%
    # Define P(A = a_i | L = l_i) as the proportion with A = a_i
    # among the set L = l_i
    group_by(A,L) %>%
    mutate(LA_num  = n()) %>%
    group_by(A) %>%
    mutate(A_num = n()) %>%
    ungroup() %>%
    mutate(numerator = A_num / n(),
           denominator = LA_num / L_num,
           w = numerator / denominator) %>%
    select(L, A, Y, w)
  return(data_with_weights)
}
```

Define an MSM estimator using the stabilized weight function.

``` r
msm_estimator_stabilized <- function(data) {
  # Use your function to create inverse probability weights
  with_weights <- create_stabilized_ipw(data)
  # Fit OLS for Y ~ A weighted by w_i
  fit <- lm(Y ~ A, data = with_weights, w = w)
  # Return a data.frame with the estimated E(Y^a) for a = 1,2,3
  result <- data.frame(A = 1:3)
  result$estimate <- predict(fit, newdata = result)
  return(result)
}
```

Conduct the simulation

``` r
R <- 10000
n <- 20
part5_sims <- foreach(r = 1:R, .combine = "rbind", .packages = c("tidyverse","foreach")) %dorng% {
  sim <- generate_data_for_stabilized(n)
  msm_estimator_stabilized(sim) %>%
    rename(stabilized = estimate) %>%
    left_join(msm_estimator(sim) %>%
                rename(unstabilized = estimate))
}
```

Visualize the result

``` r
part5_sims %>%
  mutate(diff = stabilized ^ 2 - unstabilized ^ 2) %>%
  group_by(A) %>%
  summarize(diff_mean = mean(diff),
            diff_se = sd(diff) / sqrt(n())) %>%
  mutate(diff_min = diff_mean - qnorm(.975) * diff_se,
         diff_max = diff_mean + qnorm(.975) * diff_se) %>%
  ggplot(aes(x = A, y = diff_mean,
             ymin = diff_min, ymax = diff_max)) +
  geom_point() +
  geom_errorbar() +
  ylab("Stabilized MSE - Unstabilized MSE") +
  xlab("Treatment Value a")
```

![](msm_exercise_solutions_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

The MSM with stabilized weights is superior for
![E(Y^1)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5E1%29 "E(Y^1)"),
somewhat superior for
![E(Y^2)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5E2%29 "E(Y^2)"),
and about the same for
![E(Y^3)](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;E%28Y%5E3%29 "E(Y^3)").
This makes sense because
![A = 1](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;A%20%3D%201 "A = 1")
is the most common treatment value in this setting, and stabilized
weights avoid down-weighting this treatment in the MSM model.
