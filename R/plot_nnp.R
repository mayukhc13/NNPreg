# =========================================================
# NNP Plotting System (glmnet-style unified module)
# =========================================================

# =========================================================
# 1. MAIN DISPATCHER
# =========================================================

#' Plot method for NNP objects
#'
#' Produces cross-validation, rank, coefficient-norm, or combined
#' diagnostic plots for a fitted NNP model.
#'
#' @param x An object of class \code{"nnp"} returned by \code{\link{nnp}}.
#' @param type Type of plot. One of \code{"cv"}, \code{"rank"},
#'   \code{"coef"}, or \code{"all"}.
#' @param log_lambda Logical; if \code{TRUE}, use \code{log(lambda)}
#'   on the horizontal axis.
#' @param add_rug Logical; if \code{TRUE}, add a rug showing the
#'   lambda grid to the CV plot.
#' @param ... Additional graphical parameters.
#'
#' @export
plot.nnp <- function(x,
                     type = c("cv", "rank", "coef", "all"),
                     log_lambda = TRUE,
                     add_rug = FALSE,
                     ...) {

  if (!inherits(x, "nnp")) {
    stop("Object must be of class 'nnp'.")
  }

  type <- match.arg(type)

  switch(
    type,
    cv = plot_nnp_cv(
      x,
      log_lambda = log_lambda,
      add_rug = add_rug,
      ...
    ),
    rank = plot_nnp_rank(
      x,
      log_lambda = log_lambda,
      ...
    ),
    coef = plot_nnp_coef_path(
      x,
      log_lambda = log_lambda,
      ...
    ),
    all = plot_nnp_all(
      x,
      log_lambda = log_lambda,
      add_rug = add_rug,
      ...
    )
  )

  invisible(NULL)
}


# =========================================================
# 2. CV PLOT
# =========================================================

#' Plot cross-validation curve for an NNP model
#'
#' Plots the mean K-fold cross-validation error and its standard
#' error across the supplied lambda grid.
#'
#' @param x An object of class \code{"nnp"} returned by \code{\link{nnp}}.
#' @param log_lambda Logical; if \code{TRUE}, use \code{log(lambda)}
#'   on the horizontal axis.
#' @param add_rug Logical; if \code{TRUE}, add a rug showing the
#'   lambda grid.
#' @param main Character string giving the plot title.
#' @param lwd Line width.
#' @param pch Plotting character.
#' @param cex Character expansion factor.
#' @param ... Additional graphical parameters passed to \code{plot}.
#'
#' @keywords internal
plot_nnp_cv <- function(x,
                        log_lambda = TRUE,
                        add_rug = FALSE,
                        main = "NNP Cross-Validation Curve",
                        lwd = 2,
                        pch = 19,
                        cex = 1,
                        ...) {

  lambda <- x$lambda_seq

  if (is.null(lambda)) {
    stop("lambda_seq missing")
  }

  xvals <- if (log_lambda) log(lambda) else lambda

  graphics::plot(
    xvals,
    x$cv_mean,
    type = "b",
    pch = pch,
    lwd = lwd,
    cex = cex,
    xlab = if (log_lambda) "log(lambda)" else "lambda",
    ylab = "CV error",
    main = main,
    ...
  )

  graphics::arrows(
    xvals,
    x$cv_mean - x$cv_se,
    xvals,
    x$cv_mean + x$cv_se,
    angle = 90,
    code = 3,
    length = 0.03
  )

  if (!is.null(x$lambda_min)) {
    graphics::abline(
      v = if (log_lambda) log(x$lambda_min) else x$lambda_min,
      col = "#D55E00",
      lty = 2,
      lwd = 2
    )
  }

  if (!is.null(x$lambda_1se)) {
    graphics::abline(
      v = if (log_lambda) log(x$lambda_1se) else x$lambda_1se,
      col = "#0072B2",
      lty = 2,
      lwd = 2
    )
  }

  if (add_rug) {
    graphics::rug(xvals, col = "gray50")
  }

  graphics::legend(
    "topright",
    legend = c(
      "CV mean",
      "+/- 1 SE",
      "lambda.min",
      "lambda.1se"
    ),
    col = c(
      "black",
      "black",
      "#D55E00",
      "#0072B2"
    ),
    lty = c(1, 1, 2, 2),
    pch = c(19, NA, NA, NA),
    bty = "n",
    cex = 0.9
  )
}


# =========================================================
# 3. RANK PATH PLOT
# =========================================================

#' Plot estimated rank along the NNP regularization path
#'
#' Computes the estimated rank of the fitted coefficient matrix
#' for each value of lambda and displays it as a function of
#' the regularization parameter.
#'
#' @param x An object of class \code{"nnp"} containing a lambda grid
#'   and the original data.
#' @param log_lambda Logical; if \code{TRUE}, use \code{log(lambda)}
#'   on the horizontal axis.
#' @param main Character string giving the plot title.
#' @param lwd Line width.
#' @param pch Plotting character.
#' @param ... Additional graphical parameters passed to \code{plot}.
#'
#' @keywords internal
plot_nnp_rank <- function(x,
                          log_lambda = TRUE,
                          main = "Rank Regularization Path",
                          lwd = 2,
                          pch = 19,
                          ...) {

  if (is.null(x$X) || is.null(x$Y)) {
    stop("Need X and Y stored in object")
  }

  lambda <- x$lambda_seq

  if (is.null(lambda)) {
    stop("lambda_seq missing")
  }

  xvals <- if (log_lambda) log(lambda) else lambda

  path <- nnp_solver_path(
    X = x$X,
    Y = x$Y,
    lambdas = lambda
  )

  rank_vals <- sapply(path, function(obj) {
    sv <- svd(obj$coefficients)$d
    sum(sv > 1e-3)
  })

  graphics::plot(
    xvals,
    rank_vals,
    type = "b",
    pch = pch,
    lwd = lwd,
    xlab = if (log_lambda) "log(lambda)" else "lambda",
    ylab = "Estimated rank",
    main = main,
    ...
  )

  if (!is.null(x$true_rank)) {
    graphics::abline(
      h = x$true_rank,
      col = "darkgreen",
      lty = 2,
      lwd = 2
    )
  }
}


# =========================================================
# 4. COEFFICIENT / NORM PATH
# =========================================================

#' Plot Frobenius norm along the NNP regularization path
#'
#' Computes the Frobenius norm of the coefficient matrix for each
#' value of lambda and displays it as a function of the
#' regularization parameter.
#'
#' @param x An object of class \code{"nnp"} containing a lambda grid
#'   and the original data.
#' @param log_lambda Logical; if \code{TRUE}, use \code{log(lambda)}
#'   on the horizontal axis.
#' @param main Character string giving the plot title.
#' @param lwd Line width.
#' @param pch Plotting character.
#' @param ... Additional graphical parameters passed to \code{plot}.
#'
#' @keywords internal
plot_nnp_coef_path <- function(x,
                               log_lambda = TRUE,
                               main = "NNP Coefficient Path (Frobenius norm)",
                               lwd = 2,
                               pch = 19,
                               ...) {

  if (is.null(x$X) || is.null(x$Y)) {
    stop("Need X and Y stored in object")
  }

  lambda <- x$lambda_seq

  if (is.null(lambda)) {
    stop("lambda_seq missing")
  }

  xvals <- if (log_lambda) log(lambda) else lambda

  path <- nnp_solver_path(
    X = x$X,
    Y = x$Y,
    lambdas = lambda
  )

  norm_vals <- sapply(path, function(obj) {
    norm(obj$coefficients, "F")
  })

  graphics::plot(
    xvals,
    norm_vals,
    type = "b",
    pch = pch,
    lwd = lwd,
    xlab = if (log_lambda) "log(lambda)" else "lambda",
    ylab = "Frobenius norm of B",
    main = main,
    ...
  )
}


# =========================================================
# 5. COMBINED GLMNET-STYLE DASHBOARD
# =========================================================

#' Plot a combined NNP diagnostic dashboard
#'
#' Produces three diagnostic plots: the cross-validation curve,
#' estimated rank path, and Frobenius norm path.
#'
#' @param x An object of class \code{"nnp"} returned by \code{\link{nnp}}.
#' @param log_lambda Logical; if \code{TRUE}, use \code{log(lambda)}
#'   on the horizontal axes.
#' @param add_rug Logical; if \code{TRUE}, add a rug showing the
#'   lambda grid to the CV plot.
#' @param ... Additional graphical parameters.
#'
#' @keywords internal
plot_nnp_all <- function(x,
                         log_lambda = TRUE,
                         add_rug = FALSE,
                         ...) {

  op <- graphics::par(
    mfrow = c(1, 3),
    mar = c(5, 4, 4, 2) + 0.1
  )

  on.exit(graphics::par(op))

  plot_nnp_cv(
    x,
    log_lambda = log_lambda,
    add_rug = add_rug,
    main = "CV Curve",
    ...
  )

  plot_nnp_rank(
    x,
    log_lambda = log_lambda,
    main = "Rank Path",
    ...
  )

  plot_nnp_coef_path(
    x,
    log_lambda = log_lambda,
    main = "Frobenius Norm Path",
    ...
  )
}
