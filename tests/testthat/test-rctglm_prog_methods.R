test_that("`rctglm_with_prognosticscore` returns object of correct class", {
  withr::local_seed(42)

    withr::with_environment(
      rlang::new_environment(
        data = list(b0 = 1,
                    b1 = 1.5,
                    b2 = 2,
                    W1 = runif(100, min = -2, max = 2),
                    A = rbinom (100, 1, .5))),
      {
        dat_treat <- glm_data(
          Y ~ b0+b1*abs(sin(W1))+b2*A,
          W1 = W1,
          A = A
        )
        dat_notreat <- glm_data(
          Y ~ b0+b1*abs(sin(W1)),
          W1 = W1
        )
      }
    )

  ate <- rctglm_with_prognosticscore(
    formula = Y ~ .,
    group_indicator = A,
    data = dat_treat,
    family = gaussian(),
    estimand_fun = "ate",
    data_hist = dat_notreat)

  expect_equal(ate$prognostic_info, prog(ate))
  expect_snapshot(prog(ate),
                  transform = function(x) gsub("^<environment:.*>$", "", x))
})

