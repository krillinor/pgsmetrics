#' Print method for pgsmetrics objects
#'
#' @param x An object of class "pgsmetrics".
#' @param n Number of rows to print.
#' @param ... Additional arguments passed to print.
#' @export
print.pgsmetrics <- function(x, n = 10, ...) {
    cat("Dependent variable:", x$dependent_variable, "\n")
    cat("Covariates:", x$covariates, "\n")
    cat("Bootstrap method:", x$bootstrap_method, "\n")
    if (x$bootstrap_method == "standard") {
        cat("Bootstrap iterations:", max(x$boot_output$boot_i), "\n")
    }
    if (x$bootstrap_method == "blb") {
        cat("Bootstrap parameters:", paste0("blb_b: ", x$blb_b, "; blb_s: ", x$blb_s, "; blb_r: ", x$blb_r), "\n")
    }
    cat("Results ($metrics):\n")
    print(x$metrics, n, ...)
    cat("Other results for ranks/differences are available under $ranks/$diff\n")
}


#' Summary method for pgsmetrics objects
#'
#' @param object An object of class "pgsmetrics".
#' @param ... Additional arguments passed to summary.
#' @export
summary.pgsmetrics <- function(object, ...) {
    cat("\nBootstrap information:\n")
    cat("Number of iterations:", nrow(object$boot_output$t), "\n")
    cat("Original sample size:", nrow(object$boot_output$data), "\n")

    cat("Metrics summary:\n")
    print(summary(object$metrics))
}

#' Plot method for pgsmetrics objects
#'
#' @param x An object of class "pgsmetrics".
#' @param type What results to plot? Options: metrics/ranks/diff.
#' @param ... Additional arguments passed to plot.
#' @export
plot.pgsmetrics <- function(x, type = "metrics", ...) {
    checkmate::assert_choice(type, c("metrics", "ranks", "diff"))
    plot_metrics(x, type = type, ...)
}
