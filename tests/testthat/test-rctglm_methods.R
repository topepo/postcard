test_that("`estimand` method works", {
  withr::local_seed(42)
  n <- 10
  exposure_prob <- .5

  dat_gaus <- glm_data(
    1+1.5*X1+2*A,
    X1 = rnorm(n),
    A = rbinom(n, 1, exposure_prob),
    family = gaussian()
  )

  ate <- rctglm(formula = Y ~ .,
                exposure_indicator = A,
                exposure_prob = exposure_prob,
                data = dat_gaus,
                family = gaussian)

  est1 <- estimand(ate)
  est2 <- est(ate)
  expect_equal(est1, est2)
  expect_equal(est2, ate$estimand)
  expect_named(est1, c("Estimate", "Std. Error"))
  expect_snapshot(est1)

  ate_wo_cvvariance <- rctglm(formula = Y ~ .,
                              exposure_indicator = A,
                              exposure_prob = exposure_prob,
                              data = dat_gaus,
                              family = gaussian,
                              cv_variance = FALSE)
  expect_snapshot(estimand(ate_wo_cvvariance))
})

test_that("`coef` method works", {
  withr::local_seed(42)
  n <- 10
  exposure_prob <- .5

  dat_gaus <- glm_data(
    1+1.5*X1+2*A,
    X1 = rnorm(n),
    A = rbinom(n, 1, exposure_prob),
    family = gaussian()
  )

  ate <- rctglm(formula = Y ~ .,
                exposure_indicator = A,
                exposure_prob = exposure_prob,
                data = dat_gaus,
                family = gaussian)

  expect_equal(coef(ate$glm), coef(ate))
  expect_snapshot(coef(ate))
})

test_that("`print` method works", {
  n <- 10
  exposure_prob <- .5

  dat_gaus <- glm_data(
    1+1.5*X1+2*A,
    X1 = rnorm(n),
    A = rbinom(n, 1, exposure_prob),
    family = gaussian()
  )

  ate <- rctglm(formula = Y ~ .,
                exposure_indicator = A,
                exposure_prob = exposure_prob,
                data = dat_gaus,
                family = gaussian)

  expect_output(print(ate))
})
