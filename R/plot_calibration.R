#' Plot Calibration Curve with Prediction Intervals
#'
#' @param df A data.frame object, must contain pgs and dep.
#' @param pgs PGS to include in model.
#' @param dep Outcome variable, must be binary.
#' @param covars Additional covariates to include in the model. Default: NULL.
#' @param x_lab Text, x-axis. Default: "Predicted".
#' @param y_lab Text, y-axis. Default: "Observed".
#' @param conf_level Confidence level for prediction interval. Default: 0.95.
#' @param annotation_size Font size in  calibration metric text box. Default: 3.
#' @param annotation_x x-axis position of the calibration metric text box. Default: 0.05.
#' @param annotation_y y-axis position of the calibration metric text box. Default: 0.85.
#' @return A ggplot object with the following annotated calibration metrics:
#'   \item{ICI}{Integrated calibration index (average absolute difference)}
#'   \item{E50}{Median absolute difference}
#'   \item{E90}{90th percentile absolute difference}
#'   \item{Emax}{Maximum absolute difference}
#'   \item{ECI}{Estimated calibration index (average square difference)}
#'
#' @export
plot_calibration <- function(
    df,
    pgs,
    dep,
    covars = NULL,
    x_lab = "Predicted",
    y_lab = "Observed",
    conf_level = 0.95,
    annotation_size = 3,
    annotation_x = 0.05,
    annotation_y = 0.85) {
  checkmate::assert_class(df, "data.frame")
  checkmate::assert_character(pgs, len = 1)
  checkmate::assert_string(dep)
  checkmate::assert_character(covars, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
  checkmate::assert_subset(c(pgs, dep, covars), colnames(df))
  checkmate::assert_true(is.numeric(df[[pgs]]))
  checkmate::assert_true(all(df[[dep]] %in% c(0, 1)))
  checkmate::assert_string(x_lab)
  checkmate::assert_string(y_lab)
  checkmate::assert_number(conf_level, lower = 0, upper = 1)
  checkmate::assert_number(annotation_size, lower = 0)
  checkmate::assert_number(annotation_x, lower = 0, upper = 1)
  checkmate::assert_number(annotation_y, lower = 0, upper = 1)

  qnorm_p <- conf_level / 2

  m_covars <- c(pgs)
  if (!is.null(covars)) {
    m_covars <- c(pgs, covars)
  }
  m_x <- cbind(1, as.matrix(df[, ..m_covars]))
  m <- fastglm::fastglm(
    x = m_x,
    y = df[[dep]],
    family = binomial()
  )
  p <- stats::predict(m, newdata = m_x, type = "response")

  get_gam_pred <- function(df, dep, p) {
    s <- mgcv::s
    form <- stats::as.formula("df[[dep]] ~ s(p)")
    m_gam <- mgcv::gam(formula = form, family = "binomial")
    stats::predict(
      m_gam,
      type = "response",
      se.fit = TRUE
    )
  }
  pp <- get_gam_pred(df, dep, p)
  dt <- data.table(
    p,
    pp = pp$fit,
    pp_l = pp$fit + stats::qnorm(qnorm_p) * pp$se.fit,
    pp_u = pp$fit + stats::qnorm(qnorm_p / 2) * pp$se.fit,
    dep = df[[dep]]
  )

  abs_diff <- abs(dt$p - dt$pp)
  sq_diff <- (dt$p - dt$pp)^2
  n_signif <- 2
  ici <- signif(mean(abs_diff), n_signif)
  e_50 <- signif(stats::median(abs_diff), n_signif)
  e_90 <- signif(stats::quantile(abs_diff, probs = 0.9), n_signif)
  e_max <- signif(max(abs_diff), n_signif)
  eci <- signif(mean(sq_diff), n_signif)

  annot_text <- paste0(
    "ICI: ", ici, "\n",
    "E50: ", e_50, "\n",
    "E90: ", e_90, "\n",
    "Emax: ", e_max, "\n",
    "ECI: ", eci, "\n"
  )

  ggplot(dt, aes(p, pp)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#999999") +
    geom_ribbon(aes(ymin = pp_l, ymax = pp_u), alpha = 1 / 4) +
    geom_line() +
    annotate("text", label = annot_text, x = annotation_x, y = annotation_y, hjust = 0, size = annotation_size) +
    labs(x = x_lab, y = y_lab) +
    theme_bw() +
    coord_fixed() +
    xlim(c(0, 1.1)) +
    ylim(c(0, 1.1))
}
