#' Use prognostic covariate adjustment when fitting an [rctglm]
#'
#' The procedure uses [fit_best_learner] to fit a prognostic model to historical data and uses
#' the model to produce counterfactual predictions as a prognostic score that is then adjusted
#' for as a covariate in the [rctglm] procedure.
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
#' @return an [rctglm] object, fitted with a prognostic score as a covariate in the model
#' @export
#'
#' @examples
#' # Generate some data
#' n <- 100
#' b0 <- 1
#' b1 <- 1.5
#' b2 <- 2
#' W1 <- runif(n, min = -2, max = 2)
#'
#' dat_treat <- glm_data(
#'   b0+b1*abs(sin(W1))+b2*A,
#'   W1 = W1,
#'   A = rbinom (n, 1, .5)
#' )
#'
#' dat_notreat <- glm_data(
#'   b0+b1*abs(sin(W1)),
#'   W1 = W1
#' )
#'
#' ate <- rctglm_with_prognosticscore(
#'   formula = Y ~ .,
#'   group_indicator = A,
#'   data = dat_treat,
#'   family = gaussian(),
#'   estimand_fun = "ate",
#'   data_hist = dat_notreat)
#'
rctglm_with_prognosticscore <- function(
    formula,
    family,
    data,
    group_indicator,
    group_allocation_prob = NULL,
    estimand_fun = "ate",
    estimand_fun_deriv0 = NULL, estimand_fun_deriv1 = NULL,
    ...,
    data_hist,
    prog_formula = NULL,
    cv_folds = 5,
    learners = default_learners(),
    verbose = options::opt("verbose")) {

  call <- match.call()

  group_indicator <- rlang::enquo(group_indicator)
  named_args <- as.list(environment())
  extra_glm_args <- list(...)

  if (is.character(formula)) formula <- formula(formula)

  if (verbose >= 1) cli::cli_h2("Fitting prognostic model")
  if (is.null(prog_formula)) {
    is_response_in_data(formula, data = data_hist)
    prog_formula <- formula_everything(formula)
    if (verbose >= 1)
      cli::cli_alert_info("Created formula for fitting prognostic model as: {prog_formula}")
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
                               cv_folds = cv_folds,
                               learners = learners,
                               verbose = verbose)

  lrnr_pred <- predict(lrnr_fit, data) %>%
    dplyr::pull(.data$.pred)
  data %<>%
    dplyr::mutate(prog = lrnr_pred)

  if (verbose >= 2)
    cli::cli_alert_info("Investigate trained learners and fitted model in {.var prognostic_info} list element")

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
    verbose = verbose,
    ...
  )

  prog_info <- list(
    prognostic_info = list(
      model_fit = lrnr_fit,
      learners = learners,
      cv_folds = cv_folds
  ))

  list_with_prognostic_info <- c(rctglm_with_prognosticscore, prog_info)
  list_with_prognostic_info$call <- call

  out <- structure(list_with_prognostic_info,
                   class = class(rctglm_with_prognosticscore))

  return(out)
}
