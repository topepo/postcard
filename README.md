
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PostCard

<!-- badges: start -->

[![R-CMD-check](https://github.com/NNpackages/PostCard/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/NNpackages/PostCard/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

PostCard is a package for PrOgnoSTic CovARiate aDjustment in randomised
clinical trials. At the time of writing, the package features convenient
functions for conducting analysis using prognostic covariate adjustment
for GLMs. The package uses plug-in estimation for robust estimation of a
marginal effect estimand and influence functions for robust estimation
of the variance of the estimand.

## Installation

You can install the development version of PostCard from
[GitHub](https://github.com/) with:

``` r
pak::pak("NNpackages/PostCard")
```

``` r
library(PostCard)
withr::local_seed(1395878)
withr::local_options(list(PostCard.verbose = 0))
```

## Plug-in estimation of marginal effects and variance estimation using influence functions

First, we simulate some data to be able to showcase the functionalities

``` r
n <- 1000
w <- runif(n, min = -2, max = 2)
x <- sin(w)^2
a <- rbinom(n, 1, .5)
b0 <- 1
b1 <- 3
b2 <- 2

truemean_treat <- b0+b1*x+b2*a
y_treat <- rnorm(n, mean = truemean_treat, sd = 1)
dat_treat <- data.frame(Y = y_treat, W = w, A = a)
```

### Fitting `rctglm()` without prognostic covariate adjustment

The `rctglm()` function estimates any specified estimand using plug-in
estimation for randomised clinical trials and estimates the variance
using the influence function of the marginal effect estimand.

The interface of `rctglm()` is similar to that of the `stats::glm()`
function but with an added mandatory specification of

- The randomisation variable in data, usually being the (name of) the
  treatment variable
- The randomisation ratio - the probability of being allocated to group
  1 (rather than 0)
  - As a default, a ratio of 1’s in data is used
- An estimand function
  - As a default, the function takes the average treatment effect (ATE)
    as the estimand

Thus, we can estimate the ATE by simply writing the below:

> Note that as a default, information about the algorithm is printed in
> the console, but here we suppress this behavior. See more in
> `vignette("non-default")`.

``` r
ate <- rctglm(formula = Y ~ A * W,
              group_indicator = A,
              data = dat_treat,
              family = gaussian())
```

This creates an `rctglm()` object which prints as

``` r
ate
#> Object of class 'rctglm'
#> 
#> Call:  rctglm(formula = Y ~ A * W, group_indicator = A, family = gaussian(), 
#>     data = dat_treat)
#> 
#> - Counterfactual control mean (Psi_0=E[Y|X, A=0]) estimate: 2.775793
#> - Counterfactual control mean (Psi_1=E[Y|X, A=1]) estimate: 4.866888
#> - Estimand function r: psi1 - psi0
#> - Estimand (r(Psi_1, Psi_0)) estimate (SE): 2.091095 (0.09208528)
```

### Using prognostic covariate adjustment

The `rctglm_with_prognosticscore()` function uses the
`fit_best_learner()` function to fit a prognostic model to historical
data and then uses the prognostic model to predict

for all observations in the current data set. These *prognostic scores*
are then used as a covariate in the GLM when running `rctglm()`.

Allowing the use of complex non-linear models to create such a
prognostic score allows utilising information from potentially many
variables, “catching” non-linear relationships and then using all this
information in the GLM model using a single covariate adjustment.

We simulate some historical data to showcase the use of this function as
well:

``` r
truemean_notreat <- b0+b1*x
y_notreat <- rnorm(n, mean = truemean_notreat, sd = 1)
dat_notreat <- data.frame(Y = y_notreat, W = w)
```

The call to `rctglm_with_prognosticscore()` is the same as to `rctglm()`
but with an added specification of

- (Historical) data to fit the prognostic model using
  `fit_best_learner()`
- A formula used when fitting the prognostic model
  - Default uses all covariates in the data.
- (Optionally) number folds in cross validation and a list of learners
  for fitting the best learner

Thus, a simple call which estimates the average treatment effect,
adjusting for a prognostic score, is seen below:

``` r
ate_prog <- rctglm_with_prognosticscore(
  formula = Y ~ A * W,
  group_indicator = A,
  data = dat_treat,
  family = gaussian(),
  data_hist = dat_notreat)
```

Quick results of the fit can be seen by printing the object:

``` r
ate_prog
#> Object of class 'rctglm'
#> 
#> Call:  rctglm_with_prognosticscore(formula = Y ~ A * W, family = gaussian(), 
#>     data = dat_treat, group_indicator = A, data_hist = dat_notreat)
#> 
#> - Counterfactual control mean (Psi_0=E[Y|X, A=0]) estimate: 2.828515
#> - Counterfactual control mean (Psi_1=E[Y|X, A=1]) estimate: 4.819363
#> - Estimand function r: psi1 - psi0
#> - Estimand (r(Psi_1, Psi_0)) estimate (SE): 1.990849 (0.0641213)
```

It’s evident that in this case where there is a non-linear relationship
between the covariate we observe and the response, adjusting for the
prognostic score reduces the standard error of our estimand
approximation by quite a bit.
