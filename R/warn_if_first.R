warn_once <- function(..., a = FALSE) {
  .warnings_seen <- character()
  if (a) {
    exprs <- list(...)
  } else {
    exprs <- c(as.list(match.call(expand.dots = FALSE)$...))
  }
  sapply(exprs, eval, envir = parent.frame())
}
warn_if_first <- function(reason, ...) {
  ## Look for .warnings_seen
  for (i in sys.nframe():0) {
    warn_env <- parent.frame(i)
    found_it <- exists(".warnings_seen", warn_env)
    if (found_it) { break }
  }
  if (!found_it) { stop("'warn_if_first not inside 'warn_once'") }
  ## Warn if first, and mark the reason
  .warnings_seen <- get(".warnings_seen", warn_env)
  if (! reason %in% .warnings_seen) {
    warning(...)
    .warnings_seen <- c(.warnings_seen, reason)
    assign(".warnings_seen", .warnings_seen, warn_env)
  }
}