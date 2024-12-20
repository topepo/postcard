#' create_glm_data <- function(formula_eta, ..., family = gaussian(),
#'                             extra_args_rfun = list(sd = 1)) {
#'   family = check_family(family)
#'
#'   data_cols <- list(...)
#'   data <- as.data.frame(data_cols)
#'   n <- nrow(data)
#'
#'   cols_env <- rlang::new_environment(data_cols)
#'   linear_predictor <- withr::with_environment(
#'     env = cols_env,
#'     code = {
#'       ifelse(is.character(formula_eta),
#'              eval(parse(text = formula_eta)),
#'              formula_eta)
#'     }
#'   )
#'   mu <- family$linkinv(linear_predictor)
#'
#'   args_to_rfun <- c(list(family = family), extra_args_rfun)
#'   rfun <- do.call(family_to_rfun, args_to_rfun)
#'   deparse_add_arg(rfun)
#'   y <- rfun(n, mu)
#'
#'   out <- data %>%
#'     dplyr::mutate(y = y, .before = everything())
#'
#'   return(out)
#' }
#'
#' family_to_rfun <- function(family, ...) {
#'   family <- check_family(family)
#'   family_name_only_letters <- gsub("[^a-zA-Z\\.]", "", family$family)
#'
#'   family_with_dist_as_class <- structure(
#'     family,
#'     class = c(
#'       family_name_only_letters,
#'       class(family)
#'     )
#'   )
#'
#'   UseMethod("family_to_rfun", object = family_with_dist_as_class)
#' }
#'
#' deparse_add_arg <- function(fun) {
#'
#'   extra_args <- parent.frame()$extra_args_rfun
#'
#'   copy <- fun
#'   body_copy <- body(copy)
#'
#'   browser()
#'   cur_env <- rlang::current_env()
#'   body(copy) <- substitute(
#'     body_copy,
#'     c(cur_env, list(sd = extra_args$sd))
#'   )
#'
#' }
#'
#' #' @export
#' family_to_rfun.gaussian <- function(family, sd = 1, ...) {
#'   # # eval(
#'   # #   parse(
#'   # #     text = paste0(
#'   # #       "function(n, mu) rnorm(n = n, mean = mu, sd = ", sd, ")"
#'   # #     )
#'   # #   )
#'   # # )
#'   #
#'
#'   fun <- function(n, mu) rnorm(n = n, mean = mu, sd = sd)
#'   deparse_add_arg(fun)
#'
#'   out <- function(n, mu) NULL
#'
#'   # Substitute the placeholder with the actual `sd` value
#'   body(out) <- substitute(
#'     rnorm(n = n, mean = mu, sd = sd_placeholder),
#'     list(sd_placeholder = sd)
#'   )
#'
#'   out
#'
#'   function(n, mu) rnorm(n = n, mean = mu, sd = sd)
#' }
#'
#' #' @export
#' family_to_rfun.binomial <- function(family, ...) {
#'   function(n, mu) rbinom(n = n, size = 1, prob = mu)
#' }
#'
#' #' @export
#' family_to_rfun.poisson <- function(family, ...) {
#'   function(n, mu) rpois(n = n, lambda = mu)
#' }
#'
#' check_family <- function(family) {
#'   if (is.character(family)) {
#'     family <- get(family, mode = "function", envir = parent.frame())
#'   }
#'   if (is.function(family)) {
#'     family <- family()
#'   }
#'   if (is.null(family$family)) {
#'     print(family)
#'     stop("'family' not recognized")
#'   }
#'
#'   return(family)
#' }
