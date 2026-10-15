#' FISTA solver for nuclear norm penalized regression
#'
#' Solves the nuclear norm penalized regression problem
#'
#'   min_B ||Y - X B||_F^2 + lambda ||B||_*
#'
#' using the FISTA algorithm.
#'
#' @param X Numeric design matrix of dimension n x p.
#' @param Y Numeric response matrix of dimension n x T.
#' @param lambda Non-negative nuclear-norm penalty parameter.
#' @param A0 Optional initial value for the coefficient matrix.
#'   Defaults to the zero matrix.
#' @param maxit Maximum number of FISTA iterations.
#' @param tol Relative convergence tolerance.
#' @param eps Small positive constant used in the denominator
#'   of the relative convergence criterion.
#'
#' @return An object of class \code{"nnp.fit"} containing the
#'   estimated coefficient matrix, penalty parameter, number of
#'   iterations, and convergence information.
#'
nnp_solver <- function(X, Y, lambda,
                       A0 = NULL,
                       maxit = 150,
                       tol = 1e-7,
                       eps = 1e-8) {

  # ------------------------------------------------------------
  # 1. Basic checks
  # ------------------------------------------------------------

  if (!is.matrix(X) || !is.matrix(Y)) {
    stop("'X' and 'Y' must be matrices.")
  }

  if (nrow(X) != nrow(Y)) {
    stop("'X' and 'Y' must have the same number of rows.")
  }

  if (length(lambda) != 1L ||
      !is.finite(lambda) ||
      lambda < 0) {
    stop("'lambda' must be a single non-negative finite number.")
  }

  if (length(maxit) != 1L ||
      !is.finite(maxit) ||
      maxit < 1) {
    stop("'maxit' must be a positive integer.")
  }

  if (length(tol) != 1L ||
      !is.finite(tol) ||
      tol <= 0) {
    stop("'tol' must be a positive finite number.")
  }

  if (length(eps) != 1L ||
      !is.finite(eps) ||
      eps <= 0) {
    stop("'eps' must be a positive finite number.")
  }

  X <- as.matrix(X)
  Y <- as.matrix(Y)

  p <- ncol(X)
  T <- ncol(Y)

  # ------------------------------------------------------------
  # 2. Initial value
  # ------------------------------------------------------------

  if (is.null(A0)) {

    A0 <- matrix(0, nrow = p, ncol = T)

  } else {

    A0 <- as.matrix(A0)

    if (!all(dim(A0) == c(p, T))) {
      stop("'A0' must have dimension ncol(X) x ncol(Y).")
    }

    if (any(!is.finite(A0))) {
      stop("'A0' contains non-finite values.")
    }
  }

  # ------------------------------------------------------------
  # 3. Lipschitz constant
  #
  # f(A) = ||Y - XA||_F^2
  #
  # grad f(A) = 2 X' (XA - Y)
  #
  # L = 2 ||X||_2^2
  # ------------------------------------------------------------

  X_norm <- svd(X, nu = 0, nv = 0)$d[1]

  if (!is.finite(X_norm)) {
    stop("Could not compute the spectral norm of 'X'.")
  }

  L <- 2 * X_norm^2

  # ------------------------------------------------------------
  # 4. Handle the degenerate case X = 0
  # ------------------------------------------------------------

  if (L == 0) {

    # If X = 0, the objective is
    #
    #   ||Y||_F^2 + lambda ||A||_*
    #
    # and therefore A = 0 for lambda > 0.
    #
    # For lambda = 0, every A is a minimizer. We return
    # the supplied starting value in that case.

    if (lambda > 0) {
      A <- matrix(0, nrow = p, ncol = T)
    } else {
      A <- A0
    }

    fit <- list(
      coefficients = A,
      lambda = lambda,
      iterations = 0L,
      converged = TRUE,
      objective = sum((Y - X %*% A)^2) +
        lambda * sum(svd(A, nu = 0, nv = 0)$d),
      call = match.call()
    )

    class(fit) <- "nnp.fit"

    return(fit)
  }

  # ------------------------------------------------------------
  # 5. FISTA initialization
  #
  # Algorithm 1:
  #
  # A_0 = A0
  # Z_1 = A_0
  # t_1 = 1
  # ------------------------------------------------------------

  A <- A0
  Z <- A0
  t_k <- 1

  converged <- FALSE

  # ------------------------------------------------------------
  # 6. FISTA iterations
  # ------------------------------------------------------------

  for (iter in seq_len(maxit)) {

    A_old <- A

    # ----------------------------------------------------------
    # Gradient computation
    #
    # grad f(Z_k) = 2 X' (X Z_k - Y)
    # ----------------------------------------------------------

    grad <- 2 * crossprod(X, X %*% Z - Y)

    # ----------------------------------------------------------
    # Gradient step
    #
    # W_k = Z_k - (1/L) grad f(Z_k)
    # ----------------------------------------------------------

    W <- Z - grad / L

    # ----------------------------------------------------------
    # Singular value decomposition
    # ----------------------------------------------------------

    s <- svd(W)

    # ----------------------------------------------------------
    # Proximal step / singular-value thresholding
    #
    # A_{k+1}
    #   = U diag((sigma_i(W_k) - lambda/L)_+) V'
    # ----------------------------------------------------------

    d <- pmax(s$d - lambda / L, 0)

    if (length(d) == 0L) {
      A <- matrix(0, nrow = p, ncol = T)
    } else {
      A <- s$u %*% (d * t(s$v))
    }

    # ----------------------------------------------------------
    # Stopping criterion from the paper:
    #
    # ||A_{k+1} - A_k||_F
    # --------------------------- < tol
    #       ||A_k||_F + eps
    # ----------------------------------------------------------

    rel_change <- norm(A - A_old, type = "F") /
      (norm(A_old, type = "F") + eps)

    if (rel_change < tol) {
      converged <- TRUE
      break
    }

    # ----------------------------------------------------------
    # Nesterov acceleration
    #
    # t_{k+1} =
    #   (1 + sqrt(1 + 4 t_k^2)) / 2
    #
    # Z_{k+1} =
    #   A_{k+1} +
    #   ((t_k - 1) / t_{k+1})
    #   (A_{k+1} - A_k)
    # ----------------------------------------------------------

    t_new <- (1 + sqrt(1 + 4 * t_k^2)) / 2

    Z <- A + ((t_k - 1) / t_new) * (A - A_old)

    t_k <- t_new
  }

  # ------------------------------------------------------------
  # 7. Objective value at the final iterate
  # ------------------------------------------------------------

  residual <- Y - X %*% A

  nuclear_norm <- sum(
    svd(A, nu = 0, nv = 0)$d
  )

  objective <- sum(residual^2) +
    lambda * nuclear_norm

  # ------------------------------------------------------------
  # 8. Return fitted object
  # ------------------------------------------------------------

  fit <- list(
    coefficients = A,
    lambda = lambda,
    iterations = iter,
    converged = converged,
    objective = objective,
    relative_change = rel_change,
    call = match.call()
  )

  class(fit) <- "nnp.fit"

  fit
}
