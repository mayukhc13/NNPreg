#' Cross-validated nuclear norm penalized regression
#'
#' Performs K-fold cross-validation for nuclear norm penalized
#' regression and selects the tuning parameter by either the minimum
#' cross-validation error or the one-standard-error rule.
#'
#' @param X Numeric design matrix of dimension n x p.
#' @param Y Numeric response matrix of dimension n x q.
#' @param nfold Number of cross-validation folds.
#' @param nlambda Number of lambda values when \code{lambda_seq = NULL}.
#' @param lambda_seq Optional user-supplied lambda sequence.
#' @param rule Selection rule: \code{"min"} or \code{"1se"}.
#' @param maxit Maximum number of FISTA iterations.
#' @param tol Convergence tolerance.
#' @param eps Small positive constant used in the convergence criterion.
#' @param foldid Optional vector specifying the fold assignment for each
#'   observation. If \code{NULL}, folds are generated randomly.
#' @param seed Optional random seed used when generating folds.
#'
#' @return An object of class \code{"nnp.cv"} containing the selected
#'   model, cross-validation errors, lambda values, fold assignments,
#'   and related information.
#'
nnp_cv <- function(X, Y,
                   nfold = 5,
                   nlambda = 30,
                   lambda_seq = NULL,
                   rule = c("min", "1se"),
                   maxit = 150,
                   tol = 1e-7,
                   eps = 1e-8,
                   foldid = NULL,
                   seed = NULL) {

  rule <- match.arg(rule)

  # -----------------------------
  # Input checks
  # -----------------------------

  if (!is.matrix(X) || !is.matrix(Y)) {
    stop("'X' and 'Y' must be matrices.")
  }

  if (nrow(X) != nrow(Y)) {
    stop("'X' and 'Y' must have the same number of rows.")
  }

  n <- nrow(X)

  if (length(nfold) != 1L || !is.numeric(nfold) ||
      !is.finite(nfold) || nfold < 2 ||
      nfold > n || nfold != as.integer(nfold)) {
    stop("'nfold' must be an integer between 2 and n.")
  }

  nfold <- as.integer(nfold)

  if (length(maxit) != 1L || !is.numeric(maxit) ||
      !is.finite(maxit) || maxit < 1) {
    stop("'maxit' must be a positive number.")
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

  # -----------------------------
  # 1. Lambda sequence
  # -----------------------------

  if (is.null(lambda_seq)) {

    lambda_seq <- nnp_lambda_seq(
      X = X,
      Y = Y,
      nlambda = nlambda
    )

  } else {

    if (!is.numeric(lambda_seq) ||
        length(lambda_seq) < 2) {
      stop("'lambda_seq' must contain at least two numeric values.")
    }

    if (any(!is.finite(lambda_seq)) ||
        any(lambda_seq <= 0)) {
      stop("'lambda_seq' must contain only positive finite values.")
    }
  }

  # Decreasing order for warm starts.
  lambda_seq <- sort(unique(lambda_seq), decreasing = TRUE)

  if (length(lambda_seq) < 2) {
    stop("At least two distinct lambda values are required.")
  }

  # -----------------------------
  # 2. Construct folds
  # -----------------------------

  if (is.null(foldid)) {

    if (!is.null(seed)) {
      if (length(seed) != 1L || !is.numeric(seed) ||
          !is.finite(seed)) {
        stop("'seed' must be a single finite numeric value.")
      }

      set.seed(seed)
    }

    foldid <- sample(rep(seq_len(nfold), length.out = n))

  } else {

    if (length(foldid) != n) {
      stop("'foldid' must have length n.")
    }

    if (anyNA(foldid)) {
      stop("'foldid' must not contain missing values.")
    }

    if (!is.numeric(foldid) && !is.integer(foldid)) {
      stop("'foldid' must be numeric or integer.")
    }

    foldid <- as.integer(foldid)

    if (any(foldid < 1 | foldid > nfold)) {
      stop("'foldid' must contain values between 1 and nfold.")
    }

    if (length(unique(foldid)) < 2) {
      stop("'foldid' must contain at least two distinct folds.")
    }

    nfold <- length(unique(foldid))
  }

  # -----------------------------
  # 3. Cross-validation
  # -----------------------------

  cv_err <- matrix(
    NA_real_,
    nrow = length(lambda_seq),
    ncol = nfold
  )

  for (k in seq_len(nfold)) {

    test <- which(foldid == k)
    train <- which(foldid != k)

    Xtr <- X[train, , drop = FALSE]
    Ytr <- Y[train, , drop = FALSE]

    Xte <- X[test, , drop = FALSE]
    Yte <- Y[test, , drop = FALSE]

    # Fit the entire lambda path on the training data.
    path <- nnp_solver_path(
      X = Xtr,
      Y = Ytr,
      lambdas = lambda_seq,
      maxit = maxit,
      tol = tol,
      eps = eps
    )

    for (j in seq_along(lambda_seq)) {

      Bhat <- path[[j]]$coefficients

      pred <- Xte %*% Bhat

      # Mean squared Frobenius prediction error per
      # held-out observation.
      cv_err[j, k] <- mean(
        rowSums((Yte - pred)^2)
      )
    }
  }

  # -----------------------------
  # 4. CV statistics
  # -----------------------------

  cv_mean <- rowMeans(cv_err)

  cv_se <- apply(cv_err, 1, stats::sd)

  idx_min <- which.min(cv_mean)

  lambda_min <- lambda_seq[idx_min]

  # One-standard-error rule:
  # choose the largest lambda whose CV error is within
  # one standard error of the minimum.
  threshold <- cv_mean[idx_min] + cv_se[idx_min]

  admissible <- which(cv_mean <= threshold)

  idx_1se <- min(admissible)

  lambda_1se <- lambda_seq[idx_1se]

  lambda_opt <- if (rule == "min") {
    lambda_min
  } else {
    lambda_1se
  }

  # -----------------------------
  # 5. Final model fit
  # -----------------------------

  final_fit <- nnp_solver(
    X = X,
    Y = Y,
    lambda = lambda_opt,
    maxit = maxit,
    tol = tol,
    eps = eps
  )

  # -----------------------------
  # 6. Construct returned object
  # -----------------------------

  final_fit$type <- "cv"

  final_fit$lambda_seq <- lambda_seq

  final_fit$cv_mean <- cv_mean
  final_fit$cv_se <- cv_se
  final_fit$cv_errors <- cv_err

  final_fit$lambda_min <- lambda_min
  final_fit$lambda_1se <- lambda_1se
  final_fit$lambda_opt <- lambda_opt
  final_fit$lambda <- lambda_opt

  final_fit$cv_index_min <- idx_min
  final_fit$cv_index_1se <- idx_1se

  final_fit$rule <- rule

  final_fit$foldid <- foldid
  final_fit$nfold <- nfold

  final_fit$X <- X
  final_fit$Y <- Y

  final_fit$call <- match.call()

  class(final_fit) <- "nnp.cv"

  final_fit
}
