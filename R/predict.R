#' Predict method for NNP objects
#'
#' Computes fitted responses from a nuclear norm penalized regression
#' model using the supplied new design matrix.
#'
#' @param object An object of class \code{"nnp"}.
#' @param newx New design matrix with the same number of columns as the
#'   design matrix used to fit the model.
#' @param ... Additional arguments.
#'
#' @return A numeric matrix of predicted responses.
#'
#' @export
predict.nnp <- function(object, newx, ...) {

  if (missing(newx)) {
    stop("'newx' is required.")
  }

  newx <- as.matrix(newx)

  if (!is.numeric(newx)) {
    stop("'newx' must be numeric.")
  }

  if (any(!is.finite(newx))) {
    stop("'newx' must contain only finite values.")
  }

  if (is.null(object$coefficients)) {
    stop("Model does not contain coefficients.")
  }

  B <- object$coefficients

  if (ncol(newx) != nrow(B)) {
    stop(
      "Dimension mismatch: ncol(newx) must match ",
      "nrow(coefficients)."
    )
  }

  newx %*% B
}
