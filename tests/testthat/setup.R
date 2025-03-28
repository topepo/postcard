# op <- options(reprex.clipboard = FALSE, reprex.html_preview = FALSE)
#
# withr::defer(options(op), teardown_env())

withr::local_options(
  list(postcard.verbose = 0),
  .local_envir = teardown_env()
)
