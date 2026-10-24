#' Print method for NNP objects
#'
#' Prints information about fixed-lambda and cross-validated
#' nuclear norm penalized regression fits.
#'
#' @param x An object of class \code{"nnp"} or \code{"nnp.path"}.
#' @param ... Additional arguments.
#'
#' @return The input object \code{x}, invisibly.
#'
#' @export
print.nnp <- function(x, ...) {

  # ---------------------------------------
  # Cross-validated fit
  # ---------------------------------------

  if (identical(x$type, "cv")) {

    cat("\nCross-Validated Nuclear Norm Regression\n")
    cat("--------------------------------------\n")

    cat("Selected lambda:", x$lambda_opt, "\n")
    cat("lambda.min     :", x$lambda_min, "\n")
    cat("lambda.1se     :", x$lambda_1se, "\n")
    cat("CV rule        :", x$rule, "\n")

    cat("nfolds         :", x$nfold, "\n")
    cat("nlambda        :", length(x$lambda_seq), "\n")

    cat("Converged fit  :", x$converged, "\n")
    cat("Iterations     :", x$iterations, "\n")

    # ---------------------------------------
    # Fixed-lambda fit
    # ---------------------------------------

  } else if (identical(x$type, "fixed")) {

    cat("\nFixed-Lambda Nuclear Norm Regression\n")
    cat("------------------------------------\n")

    cat("Lambda         :", x$lambda, "\n")
    cat("Iterations     :", x$iterations, "\n")
    cat("Converged      :", x$converged, "\n")

    # ---------------------------------------
    # Unknown nnp object
    # ---------------------------------------

  } else {

    cat("\nNuclear Norm Penalized Regression\n")
    cat("---------------------------------\n")

    if (!is.null(x$lambda)) {
      cat("Lambda         :", x$lambda, "\n")
    }

    if (!is.null(x$iterations)) {
      cat("Iterations     :", x$iterations, "\n")
    }

    if (!is.null(x$converged)) {
      cat("Converged      :", x$converged, "\n")
    }
  }

  invisible(x)
}


#' Print method for NNP solution paths
#'
#' @param x An object of class \code{"nnp.path"}.
#' @param ... Additional arguments.
#'
#' @return The input object \code{x}, invisibly.
#'
#' @export
print.nnp.path <- function(x, ...) {

  cat("\nNNP Solution Path\n")
  cat("-----------------\n")

  cat("Number of lambdas:", length(x), "\n")

  if (length(x) > 0L) {
    cat("Largest lambda   :", x[[1]]$lambda, "\n")
    cat(
      "Smallest lambda  :",
      x[[length(x)]]$lambda,
      "\n"
    )
  }

  invisible(x)
}

