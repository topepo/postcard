
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

## Showcasing the `rctglm` function

The `rctglm` function estimates any specified estimand using plug-in
estimation for randomised clinical trials and estimates the variance
using the influence function of the marginal effect estimand.

To showcase the use of the function, we need to simulate some data:

``` r
n <- 1000
x1 <- rnorm(n)
a <- rbinom(n, 1, .5)
b0 <- 1
b1 <- 1.5
b2 <- 2
b3 <- 1

lin_pred <- b0+b1*x1+b2*a+b3*x1*a
y_heterogeneous <- rnorm(n, mean = lin_pred, sd = 1)
dat_heterogeneous <- data.frame(Y = y_heterogeneous, X = x1, A = a)
```

The interface of `rctglm` is similar to that of the `stats::glm`
function but with an added mandatory specification of

- The randomisation variable in data, usually being the (name of) the
  treatment variable
- The randomisation ratio - the probability of being allocated to group
  1 (rather than 0)
  - As a default, a 1:1 randomisation ratio is assumed, meaning a
    probability of 1/2.
- An estimand function
  - As a default, the function takes the average treatment effect (ATE)
    as the estimand

Thus, we can estimate the ATE by simply writing

``` r
ate <- rctglm(formula = Y ~ A * X,
              group_indicator = A,
              data = dat_heterogeneous,
              family = gaussian())
#> ℹ Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi0' as: '-1'.
#> • Alternatively, specify the derivative through the argument
#> `estimand_fun_deriv0`
#> ℹ Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
#> • Alternatively, specify the derivative through the argument
#> `estimand_fun_deriv1`
```

This creates an `rctglm` object which prints as

``` r
ate
#> Object of class 'rctglm'
#> 
#> Call:  rctglm(formula = Y ~ A * X, group_indicator = A, family = gaussian(), 
#>     data = dat_heterogeneous)
#> 
#>   - Counterfactual control mean (Psi_0=E[Y|X, A=0]) estimate: 0.9658657
#>   - Counterfactual control mean (Psi_1=E[Y|X, A=1]) estimate: 2.975619
#>   - Estimand function r: psi1 - psi0
#>   - Estimand (r(Psi_1, Psi_0)) estimate (SE): 2.009753 (0.1412466)
```

## Showcasing the `rctglm_with_prognosticscore` function

The `rctglm_with_prognosticscore` function uses the `fit_best_learner`
function to fit a prognostic model to historical data and then uses the
prognostic model to predict $$
\mathbb{E}[Y|X,A=0]
$$ for all observations in the current data set. These *prognostic
scores* are then used as a covariate in the GLM when running `rctglm`.

Allowing the use of complex non-linear models to create such a
prognostic score allows utilising information from potentially many
variables, “catching” non-linear relationships and then using all this
information in the GLM model using a single covariate adjustment.

We simulate some data to showcase the use of this function as well:

``` r
n <- 100
x1 <- rnorm(n)
w2 <- runif(n, min = -2, max = 2)
x2 <- abs(sin(w2))
a <- rbinom(n, 1, .5)
b0 <- 1
b1 <- 1.5
b2 <- 2
b3 <- 1
b4 <- 0.5

truemean_treat <- b0+b1*x1+b2*a+b3*x1*a+b4*x2
y_treat <- rnorm(n, mean = truemean_treat, sd = 1)
dat_treat <- data.frame(Y = y_treat, X = x1, W = w2, A = a)

truemean_notreat <- b0+b1*x1+b4*x2
y_notreat <- rnorm(n, mean = truemean_notreat, sd = 1)
dat_notreat <- data.frame(Y = y_notreat, X = x1, W = w2, A = a)
```

The call to `rctglm_with_prognosticscore` is the same as to `rctglm` but
with an added specification of

- (Historical) data to fit the prognostic model using `fit_best_learner`
- A formula used when fitting the prognostic model
  - Default uses all covariates in the data.
- (Optionally) number folds in cross validation and a list of learners
  for fitting the best learner

Thus, a simple call which estimates the average treatment effect,
adjusting for a prognostic score, is seen below:

``` r
ate_prog <- rctglm_with_prognosticscore(
  formula = Y ~ .,
  group_indicator = A,
  data = dat_treat,
  family = gaussian(),
  estimand_fun = "ate",
  data_hist = dat_notreat)
#> [1] "mod_lm"
#> ℹ Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi0' as: '-1'.
#> • Alternatively, specify the derivative through the argument
#> `estimand_fun_deriv0`
#> ℹ Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
#> • Alternatively, specify the derivative through the argument
#> `estimand_fun_deriv1`
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
#> prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = if (type == :
#> prediction from rank-deficient fit; attr(*, "non-estim") has doubtful cases
```

Quick results of the fit can be seen by printing the object:

``` r
ate_prog
#> Object of class 'rctglm'
#> 
#> Call:  rctglm(formula = formula_with_prognosticscore, group_indicator = group_indicator, 
#>     family = family, data = data, group_allocation_prob = group_allocation_prob, 
#>     estimand_fun = estimand_fun, estimand_fun_deriv0 = estimand_fun_deriv0, 
#>     estimand_fun_deriv1 = estimand_fun_deriv1)
#> 
#>   - Counterfactual control mean (Psi_0=E[Y|X, A=0]) estimate: 1.471268
#>   - Counterfactual control mean (Psi_1=E[Y|X, A=1]) estimate: 3.522278
#>   - Estimand function r: psi1 - psi0
#>   - Estimand (r(Psi_1, Psi_0)) estimate (SE): 2.051011 (0.5157132)
```

<!-- For comparison's sake, we also fit a model in this scenario of non-linear effects of covariates not using a prognostic score and investigate the results: -->
<!-- ```{r} -->
<!-- ate_noprog <- rctglm( -->
<!--   formula = Y ~ ., -->
<!--   group_indicator = A, -->
<!--   data = dat_treat, -->
<!--   family = gaussian(), -->
<!--   estimand_fun = "ate") -->
<!-- ate_noprog -->
<!-- ``` -->
