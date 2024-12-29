# predict_counterfactual_means
test_that("`predict_counterfactual_means` predicts correctly", {
  treat_diff <- 10
  dat <- data.frame(
    Y = 1:(2*treat_diff),
    X = rep(1:treat_diff, 2),
    A = c(rep(0, treat_diff), rep(1, treat_diff)
    )
  )
  mod <- glm(Y ~ X + A, data = dat)
  pred0 <- predict_counterfactual_means(
    model = mod,
    group_indicator_name = "A",
    group_val = 0)

  pred1 <- predict_counterfactual_means(
    model = mod,
    group_indicator_name = "A",
    group_val = 1)

  expect_equal(pred0, pred1 - treat_diff)
})

test_that("`predict_counterfactual_means` gives error when `group_indicator_name` not in model or data", {
  dat <- data.frame(
    Y = 1:10,
    X = 1:10,
    A = c(rep(0, 5), rep(1, 5)
    )
  )
  mod <- glm(Y ~ X + A, data = dat)

  expect_error(
    predict_counterfactual_means(
      model = mod,
      group_indicator_name = "test",
      group_val = 0),
    regexp = "is not in"
  )
})

test_that("`predict_counterfactual_means` works with and without data specification", {
  dat_fit <- data.frame(
    Y = 1:10,
    X = 1:10,
    A = c(rep(0, 5), rep(1, 5)
    )
  )
  mod <- glm(Y ~ X + A, data = dat_fit)

  pred_nodatspec <- predict_counterfactual_means(
    model = mod,
    group_indicator_name = "A",
    group_val = 0)
  pred_datspec <- predict_counterfactual_means(
    model = mod,
    group_indicator_name = "A",
    group_val = 0,
    data = dat_fit)

  expect_equal(pred_nodatspec, pred_datspec)

  dat_pred <- data.frame(
    Y = 1:11,
    X = -5:5
  )
  pred_newdata <- predict_counterfactual_means(
    model = mod,
    group_indicator_name = "A",
    group_val = 0,
    data = dat_pred)

  expect_type(pred_newdata, "double")
})

# default_estimand_funs
test_that("`default_estimand_funs` switches correctly", {
  default_ate <- default_estimand_funs()
  specify_ate <- default_estimand_funs("ate")
  specify_rr <- default_estimand_funs("rate_ratio")

  expect_identical(default_ate, specify_ate)
  expect_equal(default_ate(2, 1), 1)
  expect_equal(specify_rr(2, .5), 4)
})

test_that("`default_estimand_funs` error when giving non-legal default", {
  expect_error(default_estimand_funs("test"),
               "should be one of")
})
