#' Extract coefficients from an NNP model
#'
#' Returns the estimated coefficient matrix from a fitted
#' nuclear norm penalized regression model.
#'
#' @param object An object of class \code{"nnp"} returned by \code{\link{nnp}}.
#' @param ... Additional arguments. Currently ignored.
#'
#' @return A numeric matrix containing the estimated coefficient matrix.
#'
#' @export
coef.nnp <- function(object, ...) {
  object$coefficients
}
