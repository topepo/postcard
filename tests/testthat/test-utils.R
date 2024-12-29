# get_fun_args
test_that("`get_fun_args` gives argument names as vector", {
  extpkg_args <- get_fun_args(base::t)
  thispkg_args <- get_fun_args(get01args)
  f <- function(x, y) x+y
  predef_args <- get_fun_args(f)
  anon_args <- get_fun_args(\(a, b) a-b)

  expect_equal(extpkg_args, "x")
  expect_equal(thispkg_args, "fun")
  expect_equal(predef_args, c("x", "y"))
  expect_equal(anon_args, c("a", "b"))
})

# deparse_fun_body
test_that("`deparse_fun_body` produces character string of function body", {
  extpkg_body <- deparse_fun_body(base::t)
  thispkg_body <- deparse_fun_body(get01args)
  f <- function(x, y) {
    z <- x+y
    return(z)
  }
  predef_body <- deparse_fun_body(f)
  anon_body <- deparse_fun_body(\(a, b) a-b)

  expect_type(extpkg_body, "character")
  expect_length(extpkg_body, 1)
  expect_snapshot(extpkg_body)

  expect_type(thispkg_body, "character")
  expect_length(thispkg_body, 1)
  expect_snapshot(thispkg_body)

  expect_type(predef_body, "character")
  expect_length(predef_body, 1)
  expect_snapshot(predef_body)

  expect_type(anon_body, "character")
  expect_length(anon_body, 1)
  expect_equal(anon_body, "a - b")
})

# get_response_from_formula
test_that("`get_response_from_formula` error when arg not formula class", {
  expect_error(get_response_from_formula("Y ~ A"),
               "needs to have class")
})

test_that("`get_response_from_formula` works for different formula specifiations", {
  expect_equal(get_response_from_formula(Y ~ A),
               "Y")
  expect_equal(get_response_from_formula(formula("testing~bla")),
               "testing")
})

# is_response_in_data


# formula_everything
test_that("`formula_everything`", {
  formula_everything(Y ~ A)
})
