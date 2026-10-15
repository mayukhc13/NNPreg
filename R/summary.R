#' Summary method for NNP objects
#'
#' Provides a summary of fixed-lambda and cross-validated
#' nuclear norm penalized regression fits.
#'
#' @param object An object of class \code{"nnp"}.
#' @param ... Additional arguments.
#'
#' @return The supplied object, invisibly.
#'
#' @export
summary.nnp <- function(object, ...) {

  # ---------------------------------------
  # Cross-validated fit
  # ---------------------------------------

  if (identical(object$type, "cv")) {

    cat("\nCross-Validated Nuclear Norm Penalized Regression\n")
    cat("-------------------------------------------------\n\n")

    cat("Type           : CV\n")
    cat("Selected lambda:", object$lambda_opt, "\n")
    cat("lambda.min     :", object$lambda_min, "\n")
    cat("lambda.1se     :", object$lambda_1se, "\n")
    cat("CV rule        :", object$rule, "\n\n")

    cat("Folds          :", object$nfold, "\n")
    cat("Lambda grid    :", length(object$lambda_seq), "\n\n")

    cat("Iterations     :", object$iterations, "\n")
    cat("Converged      :", object$converged, "\n")

    # ---------------------------------------
    # Fixed-lambda fit
    # ---------------------------------------

  } else if (identical(object$type, "fixed")) {

    cat("\nFixed-Lambda Nuclear Norm Penalized Regression\n")
    cat("---------------------------------------------\n\n")

    cat("Type           : Fixed\n")
    cat("Lambda         :", object$lambda, "\n")
    cat("Iterations     :", object$iterations, "\n")
    cat("Converged      :", object$converged, "\n")

    # ---------------------------------------
    # Unknown nnp object
    # ---------------------------------------

  } else {

    cat("\nNuclear Norm Penalized Regression\n")
    cat("---------------------------------\n")

    if (!is.null(object$lambda)) {
      cat("Lambda         :", object$lambda, "\n")
    }

    if (!is.null(object$iterations)) {
      cat("Iterations     :", object$iterations, "\n")
    }

    if (!is.null(object$converged)) {
      cat("Converged      :", object$converged, "\n")
    }
  }

  invisible(object)
}
