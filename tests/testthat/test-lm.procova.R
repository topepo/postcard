test_that("lm.procova", {
  data <- sim.lm(N.sim = 1, N.hist.control = 100, N.hist.treatment = 100, N.control = 50, N.treatment = 50)
  object <- lm.procova(data[[1]], method = "PROCOVA", pred.model = random.hist)

  expect_true(inherits(object, "lm"), "The object should be of class 'lm'")
  expect_true("test_margin" %in% names(object), "The object should have a component named 'test_margin'")

  # Test if $test_margin is a list with three elements
  expect_true(is.list(object$test_margin) && length(object$test_margin) == 3, "The component $test_margin should be a list with three elements")

  # Extract the critical value, t-test statistic value, and test result from $test_margin
  crit_val <- object$test_margin$crit.val.t
  test_stat <- object$test_margin$test_stat
  test_res <- object$test_margin$test_result

  # Test if the critical value is a numeric value
  expect_true(is.numeric(crit_val), "The critical value should be a numeric value")

  # Test if the t-test statistic value is a numeric value
  expect_true(is.numeric(test_stat), "The t-test statistic value should be a numeric value")

  # Test if the test result is logical
  expect_true(is.logical(test_res), "The t-test result should be a logival value")
  })
