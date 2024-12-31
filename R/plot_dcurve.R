#' Plot Decision Curve
#'
#' @param df A data.frame object, must contain pgs and dep.
#' @param pgs PGS to include in model.
#' @param dep Outcome variable, must be binary.
#' @param covars Additional covariates to include in the model. Default: NULL.
#' @param dca_thresholds Vector of threshold probabilities, x-axis ('thresholds' argument in dcurves::dca.)
#' @param dca_plot_smooth Smooth lines? ('smooth' argument in dcurves:::plot.dca.)
#' @return A ggplot object.
#' @export
plot_dcurve <- function(
    df,
    pgs,
    dep,
    covars = NULL,
    dca_thresholds = seq(0, 0.5, by = 0.01),
    dca_plot_smooth = TRUE) {
  checkmate::assert_class(df, "data.frame")
  checkmate::assert_character(pgs, len = 1)
  checkmate::assert_string(dep)
  checkmate::assert_character(covars, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
  checkmate::assert_subset(c(pgs, dep, covars), colnames(df))
  checkmate::assert_true(is.numeric(df[[pgs]]))
  checkmate::assert_true(all(df[[dep]] %in% c(0, 1)))

  if (is.null(covars)) {
    x_cov <- matrix(1, nrow = nrow(df))
    colnames(x_cov) <- "intercept"
    label <- list(p_no_pgs = "No covariates", p_pgs = c("PGS"))
  } else {
    x_cov <- cbind(intercept = 1, as.matrix(df[, ..covars]))
    label <- list(p_no_pgs = "Covariates", p_pgs = "Covariates + PGS")
  }
  x_cov_pgs <- cbind(x_cov, pgs = df[[pgs]])
  m_no_pgs <- fastglm::fastglm(x = x_cov, y = df[[dep]], family = binomial())
  m_pgs <- fastglm::fastglm(x = x_cov_pgs, y = df[[dep]], family = binomial())
  p_no_pgs <- stats::predict(m_no_pgs, newdata = x_cov, type = "response")
  p_pgs <- stats::predict(m_pgs, newdata = x_cov_pgs, type = "response")
  df$p_no_pgs <- p_no_pgs
  df$p_pgs <- p_pgs
  df[[dep]] <- as.logical(df[[dep]])
  dcurves::dca(
    y_bin ~ p_no_pgs + p_pgs,
    data = df,
    thresholds = dca_thresholds,
    label = label
  ) |>
    plot(smooth = dca_plot_smooth)
}
