# `rctglm` snapshot tests

    Code
      estimand(ate_with_cv)
    Output
        Estimate Std. Error
      1 1.762089  0.1882041

---

    Code
      estimand(ate_wo_cv)
    Output
        Estimate Std. Error
      1 1.762089  0.1846456

---

    Code
      estimand(rr_with_cv)
    Output
        Estimate Std. Error
      1 40.80363    8.51032

---

    Code
      estimand(rr_wo_cv)
    Output
        Estimate Std. Error
      1 40.80363    8.55476

# `estimand_fun_derivX` can be left as NULL or specified manually

    Code
      ate_auto <- withr::with_seed(42, {
        rctglm(formula = Y ~ ., exposure_indicator = A, exposure_prob = exposure_prob,
        data = dat_gaus, family = gaussian, estimand_fun = "ate", cv_variance = FALSE,
        verbose = 1)
      })
    Message
      
      -- Symbolic differentiation of estimand function --
      
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi0' as: '-1'.
      * Alternatively, specify the derivative through the argument
      `estimand_fun_deriv0`
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      * Alternatively, specify the derivative through the argument
      `estimand_fun_deriv1`

