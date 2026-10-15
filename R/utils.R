# Internal utility: null-coalescing operator
# Used internally by the package.
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}
