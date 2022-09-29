
library(tidyverse)
library(MatchIt)
library(optmatch)
library(foreach)
set.seed(14850)

####################################
# 1. Load the simulated population #
####################################

data_population <- read_csv("data_population.csv")

# Get the true Average Treatment Effect on the Treated.
# We will compare all estimators to this.
ATT_truth <- data_population %>%
  filter(A == 1) %>%
  summarize(ATT_truth = mean(Y1 - Y0))

#########################################
# 2. Draw a sample from that population #
#    Estimate the ATT using the sample  #
#########################################

estimate_in_sample <- function(sample_size = 100) {
  # Draw a sample from the simulated population
  data_sample <- sample_n(data_population, 
                          size = sample_size)
  
  # Conduct matching
  # Example: 1:1 without replacement nearest neighbor propensity score matching
  # Exercise: Try some options here and see if you can get a better estimator
  # See ?matchit for help
  matchit.out <- matchit(A ~ L1 + L2,
                         data = data_sample, 
                         distance = "glm",
                         method = "nearest")
  
  # Summarize that match
  summary(matchit.out)
  
  # Store a data frame with the matched sample
  matched_sample <- match.data(matchit.out)
  
  # Produce an ATT estimate
  estimate <- matched_sample %>%
    # Group by the treatment
    group_by(A) %>%
    # Weighted mean of outcome, weighted by number of times this unit is a match
    summarize(ybar = weighted.mean(Y, w = weights)) %>%
    # Calculate the effect
    pivot_wider(names_from = "A", values_from = "ybar") %>%
    mutate(estimate = `1` - `0`)
    
  # Alternative you could consider: Estimate a weighted regression on the matched sample
  # matched_regression <- lm(Y ~ A + L1 + L2, 
  #                          data = matched_sample, 
  #                          weights = weights)
  # Get the treatment effect estimate
  # estimate <- coef(matched_regression)["A"]
  
  # Create a data frame with the result to return
  result <- data.frame(n = sample_size,
                       estimate = estimate$estimate)
  return(result)
}

# Run that function once
estimate_in_sample(sample_size = 100)

##################################################
# 3. Repeatedly sample and store an estimate.    #
##################################################

# Set a number of repetitions for our simulation
# Note: You might want to increase once you have a setting you like
n_repetitions <- 100
# Using a foreach loop, repeat the simulation many times.
# The .combine = "rbind" just says to append the results row-by-row in a big matrix.
simulations <- foreach(rep = 1:n_repetitions, .combine = "rbind") %do% {
  estimate_in_sample(sample_size = 100)
}

############################
# 4. Summarize performance #
############################

# Summarize the bias and variance
performance <- simulations %>%
  # Bring in the known truth (since this example is simulated)
  mutate(truth = ATT_truth$ATT_truth) %>%
  # Summarize the bias, variance, and mean squared error
  summarize(bias = mean(estimate - truth),
            variance = var(estimate),
            mse = mean((estimate - truth) ^ 2))

# Visualize the simulations and the truth
simulations %>%
  ggplot(aes(x = estimate)) +
  # Create a histogram
  geom_histogram(bins = 10, alpha = .4) +
  # Add a vertical line at the truth
  geom_vline(xintercept = ATT_truth$ATT_truth, 
             color = "blue", 
             linetype = "dashed") +
  # Put performance metrics in the title
  ggtitle(paste("Bias:",performance$bias,
                "\nVariance:",performance$variance,
                "\nMSE:",performance$mse))
