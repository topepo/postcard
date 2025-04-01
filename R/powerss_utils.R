# Check if lower_upper default of -1e3 and 1e3 gives an error and if so set
# lower to be positive
check_lower_upper <- function(f, f_arg, lower = NULL, upper = NULL,
                              default_lu_times, default_lu_scale = 1e4,
                              ...) {
  lower_upper_notgiven <- is.null(lower) || is.null(upper)
  if (lower_upper_notgiven) {
    onegiven <- !is.null(lower) || !is.null(upper)
    if (onegiven)
      cli::cli_abort("You have specified `lower` or `upper` but not the other. Please specify both or none to get the default.")

    lu_range <- (1 + default_lu_times) * default_lu_scale
    lower <- -lu_range
    upper <- lu_range

    inv_f <- inverse(f, lower = lower, upper = upper, ...)
    lower_upper_bad <- tryCatch(
      inv_f(f_arg),
      error = function(e) TRUE,
      warning = function(w) TRUE
    )

    if (lower_upper_bad)
      lower <- 1 / lu_range
  }

  list(lower = lower, upper = upper)
}

# Find inverse function using uniroot
inverse <- function(f, lower, upper, ...){
  function(y){
    stats::uniroot(function(x){f(x) - y}, lower = lower, upper = upper, ...)$root
  }
}

# Check lower and upper, find the inverse and evaluate it
inverse_val <- function(
    f, f_arg, lower = NULL, upper = NULL,
    ...){
  args <- c(as.list(environment()), list(...))
  lower_upper <- do.call(check_lower_upper, args)
  inverse <- do.call(inverse, c(list(f = f), lower_upper))

  return(inverse(f_arg))
}

# Derive psi1 from psi0 and a target_effect using the inverse of the estimand_fun
# If inv_estimand_fun is not given, find it using functionalities from above
derive_check_psi1 <- function(
    psi0,
    target_effect,
    estimand_fun,
    inv_estimand_fun = NULL,
    tolerance = sqrt(.Machine$double.eps),
    ...) {
  if (!is.null(inv_estimand_fun))
    psi1 <- inv_estimand_fun(psi0 = psi0, target_effect = target_effect)
  else {
    args_to_uniroot <- list(...)
    estimand_fun1 <- function(psi1) {
      estimand_fun(psi1 = psi1, psi0 = psi0)
    }

    psi1 <- do.call(
      inverse_val,
      c(list(f = estimand_fun1, f_arg = target_effect, default_lu_times = psi0),
        args_to_uniroot)
    )
  }

  calc_target_effect <- estimand_fun(psi0 = psi0, psi1 = psi1)
  diff_target_effect <- calc_target_effect - target_effect
  psi1_correct <- all.equal(diff_target_effect, 0, tolerance = tolerance)
  if (!psi1_correct) {
    bullets <- c(
      "The inverse of the estimand fun `inv_estimand_fun` did not
      produce a good result. The calculated `target_effect` varies from the
      assumed/given `target_effect` by {.arg {diff_target_effect}}. Either",
      i = "specify `inv_estimand_fun` manually, or",
      i = "specify arguments `lower` and `upper` passed to `uniroot` that finds the inverse"
    )
    cli::cli_warn(bullets)
  }

  return(psi1)
}
