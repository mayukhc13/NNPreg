#' Generate a lambda sequence for nuclear norm penalized regression
#'
#' Generates a logarithmically spaced, decreasing sequence of positive
#' tuning parameters for nuclear norm penalized regression.
#'
#' By default, the sequence follows the convention used in the paper:
#' from 2 * lambda_max down to 0.001 * lambda_max, where
#' lambda_max = ||X'Y||_2.
#'
#' @param X Numeric design matrix of dimension n x p.
#' @param Y Numeric response matrix of dimension n x q.
#' @param nlambda Number of lambda values.
#' @param lambda_min_ratio Smallest lambda as a fraction of lambda_max.
#' @param lambda_max_factor Largest lambda as a multiple of lambda_max.
#'
#' @return A decreasing numeric vector of lambda values.
#'
nnp_lambda_seq <- function(X, Y,
                           nlambda = 30,
                           lambda_min_ratio = 0.001,
                           lambda_max_factor = 2) {

  # -----------------------------
  # Input checks
  # -----------------------------

  if (!is.matrix(X) || !is.matrix(Y)) {
    stop("'X' and 'Y' must be matrices.")
  }

  if (nrow(X) != nrow(Y)) {
    stop("'X' and 'Y' must have the same number of rows.")
  }

  if (length(nlambda) != 1L || !is.numeric(nlambda) ||
      !is.finite(nlambda) || nlambda < 1 ||
      nlambda != as.integer(nlambda)) {
    stop("'nlambda' must be a positive integer.")
  }

  if (length(lambda_min_ratio) != 1L ||
      !is.numeric(lambda_min_ratio) ||
      !is.finite(lambda_min_ratio) ||
      lambda_min_ratio <= 0) {
    stop("'lambda_min_ratio' must be a positive number.")
  }

  if (length(lambda_max_factor) != 1L ||
      !is.numeric(lambda_max_factor) ||
      !is.finite(lambda_max_factor) ||
      lambda_max_factor <= 0) {
    stop("'lambda_max_factor' must be a positive number.")
  }

  if (lambda_min_ratio >= lambda_max_factor) {
    stop("'lambda_min_ratio' must be smaller than 'lambda_max_factor'.")
  }

  nlambda <- as.integer(nlambda)

  # -----------------------------
  # Compute lambda_max
  # -----------------------------

  # Paper convention:
  # lambda_max = ||X'Y||_2
  sv <- svd(t(X) %*% Y, nu = 0, nv = 0)

  lambda_max <- sv$d[1]

  if (!is.finite(lambda_max) || lambda_max <= 0) {
    stop("Invalid lambda_max computed. Check X and Y.")
  }

  # -----------------------------
  # Construct logarithmic grid
  # -----------------------------

  lambda_min <- lambda_min_ratio * lambda_max
  lambda_upper <- lambda_max_factor * lambda_max

  lambda_seq <- exp(
    seq(
      log(lambda_upper),
      log(lambda_min),
      length.out = nlambda
    )
  )

  lambda_seq
}
