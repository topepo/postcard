# `deparse_fun_body` produces character string of function body

    Code
      extpkg_body
    Output
      [1] "UseMethod(\"t\")"

---

    Code
      thispkg_body
    Output
      [1] "{\n    arg0 <- grep(\"0$\", get_fun_args(fun), value = TRUE)\n    arg1 <- grep(\"1$\", get_fun_args(fun), value = TRUE)\n    if (length(arg0) == 0 | length(arg1) == 0) {\n        cli::cli_abort(\"Arguments of the {.var estimand_fun} need to end in {.code 0} and {.code 1} to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, {.var estimand_fun_deriv0} and {.var estimand_fun_deriv1}, manually.\")\n    }\n    return(list(arg0 = arg0, arg1 = arg1))\n}"

---

    Code
      predef_body
    Output
      [1] "{\n    z <- x + y\n    return(z)\n}"

# `is_response_in_data` correctly gives error when column not in data

    Code
      is_response_in_data(formula = C ~ A, data = dat)
    Condition
      Error in `is_response_in_data()`:
      ! Tried to create formula to fit prognostic model but did not find the response variable `C` specified in the primary formula. Provide a formula manually through the argument `prog_formula`.

# `get01args` gives error when arguments with 0 and 1 are missing

    Code
      get01args(argsnotendingwith0and1)
    Condition
      Error in `get01args()`:
      ! Arguments of the `estimand_fun` need to end in `0` and `1` to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, `estimand_fun_deriv0` and `estimand_fun_deriv1`, manually.

---

    Code
      get01args(missing1)
    Condition
      Error in `get01args()`:
      ! Arguments of the `estimand_fun` need to end in `0` and `1` to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, `estimand_fun_deriv0` and `estimand_fun_deriv1`, manually.

---

    Code
      get01args(missing0)
    Condition
      Error in `get01args()`:
      ! Arguments of the `estimand_fun` need to end in `0` and `1` to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, `estimand_fun_deriv0` and `estimand_fun_deriv1`, manually.

---

    Code
      get01args(sum)
    Condition
      Error in `get01args()`:
      ! Arguments of the `estimand_fun` need to end in `0` and `1` to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, `estimand_fun_deriv0` and `estimand_fun_deriv1`, manually.

# `print_symbolic_differentiation` provides message

    Code
      print_symbolic_differentiation(ate, "psi1", add_string = "test string add")
    Message
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      * test string add
    Output
      function (psi0, psi1) 
      1
      

