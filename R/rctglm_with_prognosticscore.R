#' Title
#'
#' @inheritParams rctglm
#' @inheritParams fit_best_learner
#'
#' @param data_hist a `data.frame` with historical data on which to fit a prognostic model
#' @param prog_formula a `character` or `numeric` with the formula for fitting the prognostic
#' model on the historical data `data_hist`. Default models the response (assumed same as in
#' `formula`) using all columns in the `data_hist` data
#'
#' @details
#' More details on prognostic models and scores being predictions of counterfactual means
#' in control group.
#'
#' @return an `rctglm` object, fitted with a prognostic score as a covariate in the model
#' @export
#'
#' @examples
#' # Generate some data
#' n <- 100
#' w1 <- runif(n, min = -2, max = 2)
#' x1 <- abs(sin(w1))
#' a <- rbinom (n, 1, .5)
#' b0 <- 1
#' b1 <- 1.5
#' b2 <- 2
#'
#' truemean_treat <- b0+b1*x1+b2*a
#' y_treat <- rnorm(n, mean = truemean_treat, sd = 1)
#' dat_treat <- data.frame(Y = y_norm, W = w1, A = a)
#'
#' truemean_notreat <- b0+b1*x1
#' y_notreat <- rnorm(n, mean = truemean_notreat, sd = 1)
#' dat_hist <- data.frame(Y = y_notreat, W = w1)
#'
#' ate <- rctglm_with_prognosticscore(
#'   formula = Y ~ .,
#'   group_indicator = A,
#'   data = dat_norm,
#'   family = gaussian(),
#'   estimand_fun = "ate",
#'   data_hist = dat_hist)
#'
rctglm_with_prognosticscore <- function(
    formula,
    family,
    data,
    group_indicator,
    group_allocation_prob = 1/2,
    estimand_fun = "ate",
    estimand_fun_deriv0 = NULL, estimand_fun_deriv1 = NULL,
    ...,
    data_hist,
    prog_formula = NULL,
    n_folds = 5,
    learners = default_learners()) {

  group_indicator <- rlang::enquo(group_indicator)
  named_args <- as.list(environment())
  extra_glm_args <- list(...)

  if (is.character(formula)) formula <- formula(formula)
  if (is.null(prog_formula)) {
    response_var_name <- get_response_from_formula(formula)
    prog_formula <- formula(paste0(response_var_name, " ~ ."))
  } else if (is.character(prog_formula)) {
    prog_formula <- formula(prog_formula)
  }

  data_hist <- do.call(
    model.frame,
    c(
      list(
        formula = prog_formula,
        data = data_hist
      ),
      extra_glm_args
    )
  )

  lrnr_fit <- fit_best_learner(formula = prog_formula,
                               data = data_hist,
                               n_folds = n_folds,
                               learners = learners)

  lrnr_pred <- predict(lrnr_fit, data) %>%
    dplyr::pull(.pred)
  data %<>%
    dplyr::mutate(prog = lrnr_pred)

  formula_with_prognosticscore <- paste0(deparse(formula), " + prog")

  rctglm_with_prognosticscore <- rctglm(
    formula = formula_with_prognosticscore,
    family = family,
    data = data,
    group_indicator = group_indicator,
    group_allocation_prob = group_allocation_prob,
    estimand_fun = estimand_fun,
    estimand_fun_deriv0 = estimand_fun_deriv0,
    estimand_fun_deriv1 = estimand_fun_deriv1,
    ...
  )

  return(rctglm_with_prognosticscore)
}
