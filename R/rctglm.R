#' Fit GLM and find any estimand (marginal effect) using plug-in estimation with variance estimation using
#' influence functions
#'
#' The procedure uses plug-in-estimation and influence functions to perform robust inference of any specified
#' estimand in the setting of a randomised clinical trial, even in the case of heterogeneous effect of
#' covariates in randomisation groups.
#'
#' @inheritParams stats::glm
#' @inheritParams options
#'
#' @param group_indicator (name of) the variable in `data` that identifies randomisation groups
#' @param group_allocation_prob a `numeric` with the probabiliy of being assigned "group 1" (rather than group 0).
#' As a default, the ratio of 1's in data is used.
#' @param estimand_fun a `character` specifying a default estimand function, or `function` with
#' arguments `psi0` and `psi1` specifying the estimand.
#' @param estimand_fun_deriv0 a `function` specifying the derivative of `estimand_fun` wrt. `psi0`. As a default
#' the algorithm will use symbolic differentiation to automatically find the derivative from `estimand_fun`
#' @param estimand_fun_deriv1 a `function` specifying the derivative of `estimand_fun` wrt. `psi1`. As a default
#' the algorithm will use symbolic differentiation to automatically find the derivative from `estimand_fun`
#' @param ... Additional arguments passed to [stats::glm()]
#'
#' @details
#' The procedure assumes the setup of a randomised clinical trial with observations grouped by a binary
#' `group_indicator` variable, allocated randomly with probability `group_allocation_prob`. A GLM is
#' fit and then used to predict the response of all observations in the event that the `group_indicator`
#' is 0 and 1, respectively. Taking means of these predictions prodeuce the *counterfactual means*
#' `psi0` and `psi1`, and an estimand `r(psi0, psi1)` is calculated using any specified `estimand_fun`.
#'
#' The variance of the estimand is found by taking the variance of the influence function of the estimand.
#'
#' This method of inference using plug-in estimation and influence functions for the variance produces a
#' causal estimate of the estimand, as stated by articles XXXX.
#'
#' @section Estimands:
#' As noted in the description, `psi0` and `psi1` are the counterfactual means found by prediction using
#' a fitted GLM in the binary groups defined by `group_indicator`.
#'
#' Default estimand functions can be specified via `"ate"` (taking the difference `psi1 - psi0`) and
#' `"rate_ratio"` (taking the ratio `psi1 / psi0`)
#'
#' As a default, the `Deriv` package is used to perform symbolic differentiation to find the derivatives of
#' the `estimand_fun`.
#'
#' @return an `rctglm` object
#' @export
#'
#' @examples
#' # Generate some data to showcase example
#' n <- 100
#' dat_binom <- glm_data(
#'   1+1.5*X1+2*A,
#'   X1 = rnorm(n),
#'   A = rbinom(n, 1, .5),
#'   family = binomial()
#' )
#'
#' # Fit the model
#' ate <- rctglm(formula = Y ~ .,
#'               group_indicator = A,
#'               data = dat_binom,
#'               family = binomial,
#'               estimand_fun = "ate")
rctglm <- function(formula,
                   group_indicator,
                   family,
                   data,
                   group_allocation_prob = NULL,
                   estimand_fun = "ate",
                   estimand_fun_deriv0 = NULL, estimand_fun_deriv1 = NULL,
                   verbose = options::opt("verbose"),
                   ...
) {

  # TODO: Right now, estimand_fun needs to have arguments with 0 and 1 in them, and the group_indicator
  # needs to take values 0 and 1. Generalise this to work for factors and be able to set reference level

  group_indicator <- rlang::enquo(group_indicator)
  args <- as.list(environment())
  call <- match.call()

  ind_expr <- rlang::quo_get_expr(group_indicator)
  called_within_prognosticscore <- ind_expr == "group_indicator"
  if (called_within_prognosticscore) {
    group_indicator_name <- as.character(rlang::quo_get_expr(rlang::eval_tidy(group_indicator)))
  } else {
    group_indicator_name <- as.character(ind_expr)
  }

  group_vals <- data[, group_indicator_name]
  group_vals_unique <- unique(group_vals)
  if (!all(c(0,1) %in% group_vals)) cli::cli_abort("{.var group_indicator} column can only have 1's and 0's")

  if (is.null(group_allocation_prob)) {
    group_allocation_prob <- mean(group_vals)
    if (verbose >= 1)
      cli::cli_alert_info("Setting the group allocation probability {.var group_allocation_prob} as the mean of column {.var {group_indicator_name}} in data: {group_allocation_prob}")
  }

  if (is.character(estimand_fun)) estimand_fun <- default_estimand_funs(estimand_fun)

  if (is.null(estimand_fun_deriv0) | is.null(estimand_fun_deriv1)) {
    args01 <- get01args(fun = estimand_fun)

    if (verbose >= 1) cli::cli_h2("Symbolic differentiation of estimand function")
    if (is.null(estimand_fun_deriv0)) {
      estimand_fun_deriv0 <- print_symbolic_differentiation(
        arg = args01[["arg0"]],
        fun = estimand_fun,
        add_string = "Alternatively, specify the derivative through the argument {.var estimand_fun_deriv0}\n",
        verbose = verbose)
    }
    if (is.null(estimand_fun_deriv1)) {
      estimand_fun_deriv1 <- print_symbolic_differentiation(
        arg = args01[["arg1"]],
        fun = estimand_fun,
        add_string = "Alternatively, specify the derivative through the argument {.var estimand_fun_deriv1}\n",
        verbose = verbose)
    }
  }

  args_glm <- c(args[names(args) %in% names(formals(glm))], list(...))
  model <- do.call(glm, args = args_glm)

  response_var <- model$y
  group_indicator_var <- data %>%
    dplyr::pull(group_indicator_name)


  counterfactual_pred0 <- predict_counterfactual_means(group_val = 0,
                                                       model = model,
                                                       group_indicator_name = group_indicator_name)
  counterfactual_pred1 <- predict_counterfactual_means(group_val = 1,
                                                       model = model,
                                                       group_indicator_name = group_indicator_name)

  counterfactual_mean0 <- mean(counterfactual_pred0)
  counterfactual_mean1 <- mean(counterfactual_pred1)

  estimand <- estimand_fun(counterfactual_mean1, counterfactual_mean0)

  if_marginaleffect_val <- if_marginaleffect(
    response_variable = response_var,
    group_indicator = group_indicator_var,
    group_allocation_prob = group_allocation_prob,
    counterfactual_pred0 = counterfactual_pred0,
    counterfactual_pred1 = counterfactual_pred1,
    estimand_fun_deriv0 = estimand_fun_deriv0,
    estimand_fun_deriv1 = estimand_fun_deriv1)

  var_estimand <- as.numeric(var(if_marginaleffect_val))
  se_estimand <- sqrt(var_estimand/nrow(data))

  out <- list(
    estimand = estimand,
    se_estimand = se_estimand,
    var_estimand = var_estimand,
    estimand_fun = estimand_fun,
    counterfactual_mean0 = counterfactual_mean0,
    counterfactual_mean1 = counterfactual_mean1,
    counterfactual_pred0 = counterfactual_pred0,
    counterfactual_pred1 = counterfactual_pred1,
    group_indicator = group_indicator_name,
    call = call,
    glm = model
  )

  return(structure(out, class = c("rctglm", class(out))))
}
