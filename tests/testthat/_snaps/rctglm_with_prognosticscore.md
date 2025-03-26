# `rctglm_with_prognosticscore` snapshot tests

    Code
      ate <- withr::with_seed(42, {
        rctglm_with_prognosticscore(formula = Y ~ ., exposure_indicator = A,
        exposure_prob = exposure_prob, data = dat_treat, family = gaussian(),
        estimand_fun = "ate", data_hist = dat_notreat, cv_variance = TRUE, verbose = 2)
      })
    Message
      
      -- Fitting prognostic model --
      
      i Created formula for fitting prognostic model as: Y ~ .
      i Fitting learners
      * mod_mars
      * mod_lm
      * mod_gbt
      i	No tuning parameters. `fit_resamples()` will be attempted
      i 1 of 3 resampling: mod_mars
      v 1 of 3 resampling: mod_mars ()
      i	No tuning parameters. `fit_resamples()` will be attempted
      i 2 of 3 resampling: mod_lm
      v 2 of 3 resampling: mod_lm ()
      i 3 of 3 tuning:     mod_gbt
      v 3 of 3 tuning:     mod_gbt ()
      i Model with lowest RMSE: mod_gbt
      i Investigate trained learners and fitted model in `prognostic_info` list element
      
      -- Symbolic differentiation of estimand function --
      
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi0' as: '-1'.
      * Alternatively, specify the derivative through the argument
      `estimand_fun_deriv0`
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      * Alternatively, specify the derivative through the argument
      `estimand_fun_deriv1`

---

    Code
      ate_wo_cvvariance <- withr::with_seed(42, {
        rctglm_with_prognosticscore(formula = Y ~ ., exposure_indicator = A,
        exposure_prob = exposure_prob, data = dat_treat, family = gaussian(),
        estimand_fun = "ate", data_hist = dat_notreat, cv_variance = FALSE, verbose = 0)
      })

---

    Code
      rr_pois_wo_cvvariance
    Output
      
      Object of class rctglm_prog 
      
      Call:  rctglm_with_prognosticscore(formula = Y ~ ., exposure_indicator = A, 
          exposure_prob = exposure_prob, data = dat_treat_pois, family = poisson(), 
          estimand_fun = "rate_ratio", cv_variance = FALSE, data_hist = dat_notreat_pois, 
          verbose = 0)
      
      Counterfactual control mean (psi_0=E[Y|X, A=0]) estimate: 7.981
      Counterfactual control mean (psi_1=E[Y|X, A=1]) estimate: 58.48
      Estimand function r: psi1/psi0
      Estimand (r(psi_1, psi_0)) estimate (SE): 7.327 (0.518)

---

    Code
      rr_pois_with_cvvariance
    Output
      
      Object of class rctglm_prog 
      
      Call:  rctglm_with_prognosticscore(formula = Y ~ ., exposure_indicator = A, 
          exposure_prob = exposure_prob, data = dat_treat_pois, family = poisson(), 
          estimand_fun = "rate_ratio", cv_variance = TRUE, data_hist = dat_notreat_pois, 
          verbose = 0)
      
      Counterfactual control mean (psi_0=E[Y|X, A=0]) estimate: 7.981
      Counterfactual control mean (psi_1=E[Y|X, A=1]) estimate: 58.48
      Estimand function r: psi1/psi0
      Estimand (r(psi_1, psi_0)) estimate (SE): 7.327 (0.5271)

---

    Code
      rr_nb_wo_cvvariance
    Output
      
      Object of class rctglm_prog 
      
      Call:  rctglm_with_prognosticscore(formula = Y ~ ., exposure_indicator = A, 
          exposure_prob = exposure_prob, data = dat_treat_pois, family = MASS::negative.binomial(2), 
          estimand_fun = "rate_ratio", cv_variance = FALSE, data_hist = dat_notreat_pois, 
          verbose = 0)
      
      Counterfactual control mean (psi_0=E[Y|X, A=0]) estimate: 8.067
      Counterfactual control mean (psi_1=E[Y|X, A=1]) estimate: 57.7
      Estimand function r: psi1/psi0
      Estimand (r(psi_1, psi_0)) estimate (SE): 7.153 (0.5005)

---

    Code
      rr_nb_with_cvvariance
    Output
      
      Object of class rctglm_prog 
      
      Call:  rctglm_with_prognosticscore(formula = Y ~ ., exposure_indicator = A, 
          exposure_prob = exposure_prob, data = dat_treat_pois, family = MASS::negative.binomial(2), 
          estimand_fun = "rate_ratio", cv_variance = TRUE, data_hist = dat_notreat_pois, 
          verbose = 0)
      
      Counterfactual control mean (psi_0=E[Y|X, A=0]) estimate: 8.067
      Counterfactual control mean (psi_1=E[Y|X, A=1]) estimate: 57.7
      Estimand function r: psi1/psi0
      Estimand (r(psi_1, psi_0)) estimate (SE): 7.153 (0.5114)

