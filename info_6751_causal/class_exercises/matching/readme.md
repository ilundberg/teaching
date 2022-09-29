# readme

*Matching* is a powerful tool for causal inference. *Simulation* is a powerful tool in methodology, when we want to study the performance of one or more estimators in a setting where the ground truth is known. The two learning goal of this exercise address these ideas together.

Learning goals:

1) Apply matching estimators for causal effects
2) Use simulation to evaluate the bias, variance, and mean squared error of an estimator.

# Data structure

The file [matching_prepare_data.R](https://github.com/ilundberg/teaching/tree/master/info_6751_causal/class_exercises/matching/matching_prepare_data.R) generates a simulated population of 100,000 observations. These data are also saved in [data_population.csv](https://github.com/ilundberg/teaching/tree/master/info_6751_causal/class_exercises/matching/data_population.csv).

Each observation contains several observed variables:

* `L1` A numeric confounder
* `L2` A numeric confounder
* `A` A binary treatment
* `Y` A numeric outcome

Each observation also contains outcomes that we know only because the data are simulated. These variables are useful as ground truth in simulations.

* `propensity_score` The true propensity score $P(A = 1 \mid \vec{L})$
* `Y0` The potential outcome under control
* `Y1` The potential outcome under treatment

# Exercise

The file [matching_exercise.R](https://github.com/ilundberg/teaching/tree/master/info_6751_causal/class_exercises/matching/matching_exercise.R) is a skeleton of code for the exercise. It conducts four steps:

1. Load the simulated population
2. Draw a sample from that population. Estimate the ATT using the sample using the [MatchIt](https://cran.r-project.org/web/packages/MatchIt/vignettes/MatchIt.html) package
3. Repeatedly sample and store an estimate. Summarize the performance of the estimator
4. Summarize the performance of the estimator

The code is currently structured to conduct a basic form of matching: 1:1 without replacement nearest neighbor propensity score matching.

# Your task

Change one aspect of the estimator. Possibilities include:

* a ratio other than 1:1
* a different distance metric, such as Mahalanobis
* add a caliper
* change from nearest neighbor to optimal matching
* estimating with weighted regression instead of a weighted mean after matching

Then repeat your estimator and estimate the bias, variance, and mean squared error. Examine performance.

After making one estimator, you might try several possible extensions:

* Write two or more estimator functions, with different options. Compare their performance via simulation.
* Evaluate performance at various sample sizes. Produce a visualization.

At the end of class, we will come back to discuss. Think about how you would summarize what you found for the class.



