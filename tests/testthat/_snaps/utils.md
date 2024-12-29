# `deparse_fun_body` produces character string of function body

    Code
      extpkg_body
    Output
      [1] "UseMethod(\"t\")"

---

    Code
      thispkg_body
    Output
      [1] "{\n    arg0 <- grep(\"0\", get_fun_args(fun), value = TRUE)\n    arg1 <- grep(\"1\", get_fun_args(fun), value = TRUE)\n    if (length(arg0) == 0 | length(arg1) == 0) {\n        cli::cli_abort(\"Arguments of the {.var fun} need {.code 0} and {.code 1} in their names to be able to perform automatic symbolic differentiation. Alternatively, specify the partial derivatives, {.var estimand_fun_deriv0} and {.var estimand_fun_deriv1}, manually.\")\n    }\n    return(list(arg0 = arg0, arg1 = arg1))\n}"

---

    Code
      predef_body
    Output
      [1] "{\n    z <- x + y\n    return(z)\n}"

