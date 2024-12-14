#' Title
#'
#' @param formula
#' @param family
#' @param data
#' @param group_indicator
#' @param group_allocation_prob
#' @param estimand_fun
#' @param estimand_fun_deriv0
#' @param estimand_fun_deriv1
#' @param ...
#' @param data_hist
#' @param n_folds
#' @param learners
#' @param prog_formula
#'
#' @return
#' @export
#'
#' @examples
#' rctglm_with_prognosticscore(formula = Y ~ ., group_indicator = A,
#' data = dat_norm, family = gaussian(), estimand_fun = "ate",
#' data_hist = dat_norm)
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
    n_folds = 5,
    learners = default_learners(),
    prog_formula = NULL) {

  group_indicator <- rlang::enquo(group_indicator)
  named_args <- as.list(environment())
  extra_glm_args <- list(...)

  if (is.character(formula)) formula <- formula(formula)
  if (is.null(prog_formula)) {
    group_indicator_name <- rlang::quo_get_expr(group_indicator)
    prog_formula <- remove_groupindicator_from_formula(formula = formula,
                                                        group_indicator_name = group_indicator_name)
  }

  if (is.character(prog_formula)) prog_formula <- formula(prog_formula)



  data_from_formula <- do.call(
    model.frame,
    c(list(formula = prog_formula, data = data),
      extra_glm_args
    )
  )

  cv_folds <- rsample::vfold_cv(data_hist, v = n_folds)
  lrnr <- cv_folds %>%
    get_best_learner(learners = learners,
                     formula = prog_formula) %>%
    fit(data_hist)

  lrnr_pred <- predict(lrnr, data_from_formula) %>%
    dplyr::pull(.pred)
  data %<>% mutate(prog = lrnr_pred)

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
