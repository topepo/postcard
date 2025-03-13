test_that("`rctglm_with_prognosticscore` snapshot tests", {
  withr::local_seed(42)

  n <- 100
  b0 <- 1
  b1 <- 1.5
  b2 <- 2
  W1 <- runif(n, min = -2, max = 2)
  exposure_prob <- .5
  
  dat_treat <- glm_data(
    Y ~ b0+b1*abs(sin(W1))+b2*A,
    W1 = W1,
    A = rbinom (n, 1, exposure_prob)
  )
  dat_notreat <- glm_data(
    Y ~ b0+b1*abs(sin(W1)),
    W1 = W1
  )

  elapsed_time_pattern <- "\\d+\\.?\\d*m?s"
  expect_snapshot({
    ate <- withr::with_seed(42, {
      rctglm_with_prognosticscore(
        formula = Y ~ .,
        exposure_indicator = A,
        exposure_prob = exposure_prob,
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

  expect_snapshot({
    ate_wo_cvvariance <- withr::with_seed(42, {
      rctglm_with_prognosticscore(
        formula = Y ~ .,
        exposure_indicator = A,
        exposure_prob = exposure_prob,
        data = dat_treat,
        family = gaussian(),
        estimand_fun = "ate",
        data_hist = dat_notreat,
        cv_variance = FALSE,
        verbose = 0)
    })
  },
  transform = function(x) gsub(elapsed_time_pattern, "", x))

  n <- 100
  b0 <- 1
  b1 <- 1.5
  b2 <- 2
  W1 <- runif(n, min = -2, max = 2)
  dat_treat_pois <- glm_data(
    Y ~ b0+b1*abs(sin(W1))+b2*A,
    W1 = W1,
    A = rbinom (n, 1, exposure_prob),
    family = poisson()
  )
  dat_notreat_pois <- glm_data(
    Y ~ b0+b1*abs(sin(W1)),
    W1 = W1,
    family = poisson()
  )

  rr_pois <- withr::with_seed(42, {
    rctglm_with_prognosticscore(
      formula = Y ~ .,
      exposure_indicator = A,
      exposure_prob = exposure_prob,
      data = dat_treat_pois,
      family = poisson(),
      estimand_fun = "rate_ratio",
      data_hist = dat_notreat_pois,
      verbose = 0)
  })
  expect_snapshot(rr_pois)

  rr_nb <- withr::with_seed(42, {
    rctglm_with_prognosticscore(
      formula = Y ~ .,
      exposure_indicator = A,
      exposure_prob = exposure_prob,
      data = dat_treat_pois,
      family = MASS::negative.binomial(2),
      estimand_fun = "rate_ratio",
      data_hist = dat_notreat_pois,
      verbose = 0)
  })
  expect_snapshot(rr_nb)
})

test_that("`cv_variance` produces same point estimates but different SE estimates", {
  withr::local_seed(42)

  n <- 100
  b0 <- 1
  b1 <- 1.5
  b2 <- 2
  W1 <- runif(n, min = -2, max = 2)
  exposure_prob <- .5

  dat_treat <- glm_data(
    Y ~ b0+b1*abs(sin(W1))+b2*A,
    W1 = W1,
    A = rbinom (n, 1, exposure_prob)
  )
  dat_notreat <- glm_data(
    Y ~ b0+b1*abs(sin(W1)),
    W1 = W1
  )

  ate_w_cvvariance <- withr::with_seed(42, {
      rctglm_with_prognosticscore(
        formula = Y ~ .,
        exposure_indicator = A,
        exposure_prob = exposure_prob,
        data = dat_treat,
        family = gaussian(),
        estimand_fun = "ate",
        data_hist = dat_notreat,
        cv_variance = TRUE,
        verbose = 0)
    })
  ate_wo_cvvariance <- withr::with_seed(42, {
      rctglm_with_prognosticscore(
        formula = Y ~ .,
        exposure_indicator = A,
        exposure_prob = exposure_prob,
        data = dat_treat,
        family = gaussian(),
        estimand_fun = "ate",
        data_hist = dat_notreat,
        cv_variance = FALSE,
        verbose = 0)
    })

  expect_equal(
    estimand(ate_wo_cvvariance)$Estimate,
    estimand(ate_w_cvvariance)$Estimate
  )
  expect_failure(
    expect_identical(
      estimand(ate_wo_cvvariance)$`Std. Error`,
      estimand(ate_w_cvvariance)$`Std. Error`
    )
  )
})

test_that("`prog_formula` manual specification consistent with default behavior", {
  withr::local_seed(42)

  n <- 100
  b0 <- 1
  b1 <- 1.5
  b2 <- 2
  W1 <- runif(n, min = -2, max = 2)
  exposure_prob <- .5

  dat_treat <- glm_data(
    Y ~ b0+b1*abs(sin(W1))+b2*A,
    W1 = W1,
    A = rbinom (n, 1, exposure_prob)
  )
  dat_notreat <- glm_data(
    Y ~ b0+b1*abs(sin(W1)),
    W1 = W1
  )

  # Note default behavior models response as all variables in data, in this case just W1
  ate_wo_prog_formula <- withr::with_seed(42, {
      rctglm_with_prognosticscore(
        formula = Y ~ .,
        exposure_indicator = A,
        exposure_prob = exposure_prob,
        data = dat_treat,
        family = gaussian(),
        estimand_fun = "ate",
        data_hist = dat_notreat,
        verbose = 2)
    })

  ate_w_prog_formula <- withr::with_seed(42, {
    rctglm_with_prognosticscore(
      formula = Y ~ .,
      exposure_indicator = A,
      exposure_prob = exposure_prob,
      data = dat_treat,
      family = gaussian(),
      estimand_fun = "ate",
      data_hist = dat_notreat,
      prog_formula = "Y ~ W1")
  })

  expect_equal(est(ate_wo_prog_formula), est(ate_w_prog_formula))
})
