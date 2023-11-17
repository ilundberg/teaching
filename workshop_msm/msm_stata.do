
clear all

* Warning: I am an R user and not much of a Stata user.
* The code below could have errors, and could surely be improved!

* Set the sample size
set obs 100

* Generate simulated data
generate X1 = rnormal()
generate A1 = rbinomial(1,invlogit(.25 * X1))
generate U2 = rnormal()
generate X2 = rnormal(-.25 + .5 * A1 + U2)
generate A2 = rbinomial(1,invlogit(.25 * X2))
generate Y = rnormal(X1 + A1 + X2 + A2 + U2)

* Fit a period 1 propensity score model.
* Predict probability of A1 = 1 given X1
logit A1 X1
predict p_A1_equals_1

* Fit a period 2 propensity score model.
* Predict probability of A1 = 1 given X1
logit A2 A1 X2
predict p_A2_equals_1

* Predict the propensity score at each period
* Code as P(A = 1) for cases with A = 1
* Code as P(A = 0) for cases with A = 0

generate pi1 = p_A1_equals_1
replace pi1 = 1 - pi1 if A1 == 0

generate pi2 = p_A2_equals_1
replace pi2 = 1 - pi2 if A2 == 0

* Create an inverse probability weight:
* inverse of
  * probability of the observed treatment in period 1 times
  * probability of the observed treatment in period 2
generate weight1 = 1 / pi1
generate weight2 = 1 / pi2
generate weight = weight1 * weight2

* INVERSE PROBABILITY WEIGHTING ESTIMATOR
* We do this for intervention on A1, then for intervention on A2, then for a joint intervention
* Mean outcome under intervention on A1
tabstat Y [aw = weight1], by(A1)

* Mean outcome under intervention on A2
tabstat Y [aw = weight2], by(A2)

* Mean outcome under joint intervention on A1 and A2
by A1 A2, sort: summarize Y [aw = weight]

* MARGINAL STRUCTURAL MODEL ESTIMATOR
* Fit an additive linear regression of Y on A1 + A2, weighted by your weight
regress Y A1 A2 [w = weight]

* then need to make predictions in a new dataset with variables
* A1 = (0,0,1,1)
* A2 = (0,1,0,1)

* I do not use Stata, so there is probably an easier way to do that.
* One way is to modify the values of these variables and make predictions
replace A1 = 0
replace A2 = 0
predict Y00

replace A1 = 0
replace A2 = 1
predict Y01

replace A1 = 1
replace A2 = 0
predict Y10

replace A1 = 1
replace A2 = 1
predict Y11

summarize Y00 Y01 Y10 Y11

