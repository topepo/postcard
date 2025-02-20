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

# `is_response_in_data` correctly gives error when column not in data [plain]

    Code
      is_response_in_data(formula = C ~ A, data = dat)
    Condition
      Error in `is_response_in_data()`:
      ! Tried to create formula to fit prognostic model but did not find the response variable `C` specified in the primary formula. Provide a formula manually through the argument `prog_formula`.

# `is_response_in_data` correctly gives error when column not in data [ansi]

    Code
      is_response_in_data(formula = C ~ A, data = dat)
    Condition
      [1m[33mError[39m in `is_response_in_data()`:[22m
      [1m[22m[33m![39m Tried to create formula to fit prognostic model but did not find the response variable `C` specified in the primary formula. Provide a formula manually through the argument `prog_formula`.

# `is_response_in_data` correctly gives error when column not in data [unicode]

    Code
      is_response_in_data(formula = C ~ A, data = dat)
    Condition
      Error in `is_response_in_data()`:
      ! Tried to create formula to fit prognostic model but did not find the response variable `C` specified in the primary formula. Provide a formula manually through the argument `prog_formula`.

# `is_response_in_data` correctly gives error when column not in data [fancy]

    Code
      is_response_in_data(formula = C ~ A, data = dat)
    Condition
      [1m[33mError[39m in `is_response_in_data()`:[22m
      [1m[22m[33m![39m Tried to create formula to fit prognostic model but did not find the response variable `C` specified in the primary formula. Provide a formula manually through the argument `prog_formula`.

# `print_symbolic_differentiation` provides message [plain]

    Code
      print_symbolic_differentiation(ate, "psi1", add_string = "test string add")
    Message
      i Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      * test string add
    Output
      function (psi0, psi1) 
      1
      

# `print_symbolic_differentiation` provides message [ansi]

    Code
      print_symbolic_differentiation(ate, "psi1", add_string = "test string add")
    Message
      [36mi[39m Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      * test string add
    Output
      function (psi0, psi1) 
      1
      

# `print_symbolic_differentiation` provides message [unicode]

    Code
      print_symbolic_differentiation(ate, "psi1", add_string = "test string add")
    Message
      ℹ Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      • test string add
    Output
      function (psi0, psi1) 
      1
      

# `print_symbolic_differentiation` provides message [fancy]

    Code
      print_symbolic_differentiation(ate, "psi1", add_string = "test string add")
    Message
      [36mℹ[39m Symbolically deriving partial derivative of the function 'psi1 - psi0' with respect to 'psi1' as: '1'.
      • test string add
    Output
      function (psi0, psi1) 
      1
      

