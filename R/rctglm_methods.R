#' @export
print.rctglm = function(x, ...) {
  cat("Object of class 'rctglm'\n\n")
  cat("Call:  ",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("  - Counterfactual control mean (Psi_0=E[Y|X, A=0]) estimate: ",
      x$counterfactual_mean0,
      "\n",
      sep = "")
  cat("  - Counterfactual control mean (Psi_1=E[Y|X, A=1]) estimate: ",
      x$counterfactual_mean1,
      "\n",
      sep = "")
  cat("  - Estimand function r: ",
      deparse_fun_body(x$estimand_fun),
      "\n",
      sep = "")
  cat("  - Estimand (r(Psi_1, Psi_0)) estimate (SE): ",
      x$estimand,
      " (",
      x$se_estimand,
      ")\n",
      sep = "")

  return(invisible())
}
