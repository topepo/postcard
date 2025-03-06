test_that("`rctglm_with_prognosticscore` snapshot tests", {
  withr::local_seed(42)
  # Generate some data
  n <- 100
  b0 <- 1
  b1 <- 1.5
  b2 <- 2
  W1 <- runif(n, min = -2, max = 2)

  dat_treat <- glm_data(
    b0+b1*abs(sin(W1))+b2*A,
    W1 = W1,
    A = rbinom (n, 1, .5)
  )
  dat_notreat <- glm_data(
    b0+b1*abs(sin(W1)),
    W1 = W1
  )

  elapsed_time_pattern <- "\\d+\\.?\\d*m?s"
  expect_snapshot({
    ate <- withr::with_seed(42, {
      rctglm_with_prognosticscore(
        formula = Y ~ .,
        group_indicator = A,
        data = dat_treat,
        family = gaussian(),
        estimand_fun = "ate",
        data_hist = dat_notreat,
        verbose = 2)
    })
  },
  transform = function(x) gsub(elapsed_time_pattern, "", x))

  expect_s3_class(ate, "rctglm_prog")
  expect_s3_class(ate, "rctglm")

  ate_spec_prog <- withr::with_seed(42, {
    rctglm_with_prognosticscore(
      formula = Y ~ .,
      group_indicator = A,
      data = dat_treat,
      family = gaussian(),
      estimand_fun = "ate",
      data_hist = dat_notreat,
      prog_formula = "Y ~ W1")
  })

  expect_equal(est(ate), est(ate_spec_prog))

  ate_wo_cvvariance <- withr::with_seed(42, {
    rctglm_with_prognosticscore(
      formula = Y ~ .,
      group_indicator = A,
      data = dat_treat,
      family = gaussian(),
      estimand_fun = "ate",
      data_hist = dat_notreat,
      cv_variance = FALSE,
      verbose = 0)
  })
  expect_snapshot(ate_wo_cvvariance)

  dat_treat_pois <- glm_data(
    b0+b1*abs(sin(W1))+b2*A,
    W1 = W1,
    A = rbinom (n, 1, .5),
    family = poisson()
  )
  dat_notreat_pois <- glm_data(
    b0+b1*abs(sin(W1)),
    W1 = W1,
    family = poisson()
  )

  ate_pois <- withr::with_seed(42, {
    rctglm_with_prognosticscore(
      formula = Y ~ .,
      group_indicator = A,
      data = dat_treat_pois,
      family = poisson(),
      estimand_fun = "rate_ratio",
      data_hist = dat_notreat_pois,
      verbose = 0)
  })
  expect_snapshot(ate_pois)
})
