#' Nuclear norm solution path
#'
#' Computes the nuclear norm penalized estimator over a sequence of
#' tuning parameters using warm starts.
#'
#' The lambda sequence is sorted in decreasing order so that the
#' solution at the previous lambda can be used as the initial value
#' for the next FISTA optimization.
#'
#' @param X Numeric design matrix of dimension n x p.
#' @param Y Numeric response matrix of dimension n x q.
#' @param lambdas Numeric vector of positive tuning parameters.
#' @param maxit Maximum number of FISTA iterations for each lambda.
#' @param tol Convergence tolerance.
#' @param eps Small positive constant used in the convergence criterion.
#'
#' @return An object of class \code{"nnp.path"}, containing the fitted
#' coefficient matrices and convergence information for each lambda.
#'
nnp_solver_path <- function(X, Y, lambdas,
                            maxit = 150,
                            tol = 1e-7,
                            eps = 1e-8) {

  # -----------------------------
  # Basic input checks
  # -----------------------------

  if (!is.matrix(X) || !is.matrix(Y)) {
    stop("'X' and 'Y' must be matrices.")
  }

  if (nrow(X) != nrow(Y)) {
    stop("'X' and 'Y' must have the same number of rows.")
  }

  if (missing(lambdas) || length(lambdas) == 0) {
    stop("'lambdas' must contain at least one value.")
  }

  if (!is.numeric(lambdas)) {
    stop("'lambdas' must be numeric.")
  }

  if (any(!is.finite(lambdas))) {
    stop("'lambdas' must contain only finite values.")
  }

  if (any(lambdas <= 0)) {
    stop("'lambdas' must be strictly positive.")
  }

  if (length(maxit) != 1L || !is.numeric(maxit) ||
      !is.finite(maxit) || maxit < 1) {
    stop("'maxit' must be a positive integer.")
  }

  if (length(tol) != 1L || !is.numeric(tol) ||
      !is.finite(tol) || tol <= 0) {
    stop("'tol' must be a positive number.")
  }

  if (length(eps) != 1L || !is.numeric(eps) ||
      !is.finite(eps) || eps <= 0) {
    stop("'eps' must be a positive number.")
  }

  # Make sure maxit is an integer internally.
  maxit <- as.integer(maxit)

  # -----------------------------
  # Lambda ordering
  # -----------------------------

  # Decreasing order is used so that the previous solution
  # provides a warm start for the next lambda.
  lambdas <- sort(lambdas, decreasing = TRUE)

  # -----------------------------
  # Compute solution path
  # -----------------------------

  path <- vector("list", length(lambdas))

  # No warm start for the first lambda.
  A0 <- NULL

  for (i in seq_along(lambdas)) {

    fit <- nnp_solver(
      X = X,
      Y = Y,
      lambda = lambdas[i],
      A0 = A0,
      maxit = maxit,
      tol = tol,
      eps = eps
    )

    path[[i]] <- fit

    # Warm start for the next lambda.
    A0 <- fit$coefficients
  }

  # -----------------------------
  # Store lambda sequence
  # -----------------------------

  attr(path, "lambdas") <- lambdas

  class(path) <- "nnp.path"

  path
}
