#' Nuclear norm penalized regression (glmnet-style interface)
#'
#' Fits a nuclear norm penalized regression model either at a fixed
#' value of the penalty parameter or using K-fold cross-validation.
#'
#' @param X Numeric design matrix of dimension n x p.
#' @param Y Numeric response matrix of dimension n x q.
#' @param lambda Penalty parameter. If \code{NULL}, K-fold cross-validation
#'   is used with a default logarithmic lambda sequence. If a single
#'   value is supplied, the model is fitted at that value. If a vector
#'   is supplied, K-fold cross-validation is performed over that sequence.
#' @param nfold Number of cross-validation folds.
#' @param nlambda Number of lambda values when \code{lambda = NULL}.
#' @param rule Selection rule for cross-validation: \code{"min"} or
#'   \code{"1se"}.
#' @param maxit Maximum number of FISTA iterations.
#' @param tol Convergence tolerance.
#' @param eps Small positive constant used in the convergence criterion.
#' @param seed Optional random seed used when generating CV folds.
#'
#' @return An object of class \code{"nnp"} containing the fitted model
#'   and relevant information.
#'
#' @export
nnp <- function(
    X,
    Y,
    lambda = NULL,
    nfold = 5,
    nlambda = 20,
    rule = c("min", "1se"),
    maxit = 150,
    tol = 1e-7,
    eps = 1e-8,
    seed = NULL
) {

  rule <- match.arg(rule)

  X <- as.matrix(X)
  Y <- as.matrix(Y)

  # ---------------------------------------
  # Input checks
  # ---------------------------------------

  if (!is.numeric(X) || !is.numeric(Y)) {
    stop("'X' and 'Y' must be numeric.")
  }

  if (any(!is.finite(X)) || any(!is.finite(Y))) {
    stop("'X' and 'Y' must contain only finite values.")
  }

  if (nrow(X) != nrow(Y)) {
    stop("'X' and 'Y' must have the same number of rows.")
  }

  if (nrow(X) < 2) {
    stop("'X' and 'Y' must contain at least two observations.")
  }

  if (length(nfold) != 1L || !is.numeric(nfold) ||
      !is.finite(nfold) || nfold < 2 ||
      nfold != as.integer(nfold)) {
    stop("'nfold' must be an integer >= 2.")
  }

  nfold <- as.integer(nfold)

  if (nfold > nrow(X)) {
    stop("'nfold' cannot exceed the number of observations.")
  }

  if (length(nlambda) != 1L || !is.numeric(nlambda) ||
      !is.finite(nlambda) || nlambda < 2 ||
      nlambda != as.integer(nlambda)) {
    stop("'nlambda' must be an integer >= 2.")
  }

  nlambda <- as.integer(nlambda)

  if (length(maxit) != 1L || !is.numeric(maxit) ||
      !is.finite(maxit) || maxit < 1 ||
      maxit != as.integer(maxit)) {
    stop("'maxit' must be a positive integer.")
  }

  maxit <- as.integer(maxit)

  if (length(tol) != 1L || !is.numeric(tol) ||
      !is.finite(tol) || tol <= 0) {
    stop("'tol' must be a positive number.")
  }

  if (length(eps) != 1L || !is.numeric(eps) ||
      !is.finite(eps) || eps <= 0) {
    stop("'eps' must be a positive number.")
  }

  if (!is.null(seed)) {
    if (length(seed) != 1L || !is.numeric(seed) ||
        !is.finite(seed)) {
      stop("'seed' must be a single finite numeric value.")
    }
  }

  # ---------------------------------------
  # MODE 1: lambda = NULL -> default CV
  # ---------------------------------------

  if (is.null(lambda)) {

    fit <- nnp_cv(
      X = X,
      Y = Y,
      nfold = nfold,
      nlambda = nlambda,
      rule = rule,
      maxit = maxit,
      tol = tol,
      eps = eps,
      seed = seed
    )

    fit$type <- "cv"
    fit$lambda <- fit$lambda_opt
  }

  # ---------------------------------------
  # MODE 2: fixed lambda
  # ---------------------------------------

  else if (length(lambda) == 1L) {

    if (!is.numeric(lambda) ||
        !is.finite(lambda) ||
        lambda <= 0) {
      stop("'lambda' must be a positive finite number.")
    }

    fit <- nnp_solver(
      X = X,
      Y = Y,
      lambda = lambda,
      maxit = maxit,
      tol = tol,
      eps = eps
    )

    fit$type <- "fixed"
    fit$lambda_opt <- lambda
    fit$lambda <- lambda
  }

  # ---------------------------------------
  # MODE 3: user-supplied lambda grid
  # ---------------------------------------

  else {

    if (!is.numeric(lambda)) {
      stop("'lambda' must be numeric.")
    }

    if (any(!is.finite(lambda)) ||
        any(lambda <= 0)) {
      stop("'lambda' must contain only positive finite values.")
    }

    if (length(unique(lambda)) < 2) {
      stop("A lambda grid must contain at least two distinct values.")
    }

    lambda_seq <- sort(
      unique(lambda),
      decreasing = TRUE
    )

    fit <- nnp_cv(
      X = X,
      Y = Y,
      nfold = nfold,
      lambda_seq = lambda_seq,
      rule = rule,
      maxit = maxit,
      tol = tol,
      eps = eps,
      seed = seed
    )

    fit$type <- "cv"
    fit$lambda <- fit$lambda_opt
  }

  # ---------------------------------------
  # Final object
  # ---------------------------------------

  fit$call <- match.call()

  class(fit) <- "nnp"

  fit
}
