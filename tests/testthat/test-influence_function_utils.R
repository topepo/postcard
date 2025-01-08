# if_counterfactual_mean_glm
test_that("`if_counterfactual_mean_glm` returns vector of evaluated IF", {
  withr::local_seed(13746)

  n <- 10
  al_prob <- 0.5
  treatindicator <- rbinom(n, size = 1, prob = al_prob)
  treateffect <- 2
  mean_treatgroup <- rep(treateffect, n)
  truemean <- treatindicator * mean_treatgroup
  y <- rnorm(n, mean = truemean)

  res <- if_counterfactual_mean_glm(
    response_variable = y,
    group_indicator = treatindicator,
    counterfactual_pred = mean_treatgroup,
    group_allocation_prob = al_prob
  )

  expect_length(res, n)
  expect_snapshot(res)
})

# if_marginaleffect
test_that("`if_marginaleffect` returns vector of evaluated IF", {
  withr::local_seed(13746)

  n <- 10
  al_prob <- 0.5
  treatindicator <- rbinom(n, size = 1, prob = al_prob)
  treateffect <- 2
  mean_treatgroup <- rep(treateffect, n)
  truemean <- treatindicator * mean_treatgroup
  y <- rnorm(n, mean = truemean)

  ate <- function(psi0, psi1) psi1 - psi0
  ate_deriv0 <- function(psi0, psi1) -1
  ate_deriv1 <- function(psi0, psi1) 1

  res <- if_marginaleffect(
    response_variable = y,
    group_indicator = treatindicator,
    counterfactual_pred0 = rep(0, n),
    counterfactual_pred1 = mean_treatgroup,
    group_allocation_prob = al_prob,
    estimand_fun_deriv0 = ate_deriv0,
    estimand_fun_deriv1 = ate_deriv1
  )

  expect_length(res, n)
  expect_snapshot(res)
})
