#' AIC
#'
#' @param m glm or lm model
#' @param w Weights, used when boot_method="blb"
#' @param ... Additional arguments
#' @return Numeric
#' @export
aic <- function(m = m, w = NULL, ...) {
  if (m$family$family == "binomial" || is.null(w)) {
    stats::AIC(m)
  } else {
    k <- length(m$coefficients) + 1
    weights <- m$weights
    residuals <- m$residuals
    sigma2_weighted <- sum(weights * residuals^2) / sum(weights)
    log_likelihood_weighted <- -0.5 * sum(weights * (log(2 * pi * sigma2_weighted) + residuals^2 / sigma2_weighted))
    2 * k - 2 * log_likelihood_weighted
  }
}


#' Brier score
#'
#' @param p Probabilities from glm
#' @param y Outcome variable
#' @param w Weights, used when boot_method="blb"
#' @param ... Additional arguments
#' @return Numeric
#' @export
brier_score <- function(p, y, w, ...) {
  inner <- (p - y)^2
  if (is.null(w)) {
    mean(inner)
  } else {
    stats::weighted.mean(inner, w)
  }
}


#' $R^2$
#'
#' @param m lm model
#' @param w Weights, used when boot_method="blb"
#' @param ... Additional arguments
#' @return Numeric
#' @export
r2 <- function(m = m, w = w, ...) {
  residuals <- m$residuals
  observed <- m$y
  if (is.null(w)) {
    rss <- sum(residuals^2)
    y_mean <- mean(observed)
    tss <- sum((observed - y_mean)^2)
  } else {
    rss <- sum(w * residuals^2)
    tss <- sum(w * (observed - stats::weighted.mean(observed, w))^2)
  }
  1 - (rss / tss)
}


#' Prep data for `r2_liability`
#'
#' @param dep Outcome vector
#' @param K population prevalence, if NULL then =P
#' @param w Weights, used when boot_method="blb"
#' @keywords internal
r2_liability_prep <- function(dep, K = NULL, w = NULL) {
  if (is.null(w)) {
    ncase <- sum(dep)
    ncont <- sum(!dep)
  } else {
    ncase <- sum(w * dep)
    ncont <- sum(w * !dep)
  }
  nt <- ncase + ncont
  P <- ncase / nt
  if (is.null(K)) {
    K <- P
  }
  thd <- stats::qnorm(1 - K)
  zv <- stats::dnorm(thd)
  mv <- zv / K
  mv2 <- -mv * K / (1 - K)
  theta <- mv * (P - K) / (1 - K) * (mv * (P - K) / (1 - K) - thd)
  cv <- K * (1 - K) / zv^2 * K * (1 - K) / (P * (1 - P))
  list(nt = nt, ncase = ncase, ncont = ncont, cv = cv, theta = theta)
}


#' $R^2$ on the liability scale
#'
#' @param m_lm lm model (family="gaussian")
#' @param liab_prep Data from `r2_liability_prep`
#' @param w Weights, used when boot_method="blb"
#' @param ... Additional arguments
#' @return Numeric
#' @export
r2_liability <- function(m_lm = m_lm, liab_prep = liab_prep, w = w, ...) {
  if (is.null(w)) {
    v <- stats::var(m_lm$fitted.values)
  } else {
    wm <- stats::weighted.mean(m_lm$fitted.values, w)
    v <- sum(w * (m_lm$fitted.values - wm)^2) / (sum(w) - 1)
  }
  r2o <- v / (liab_prep$ncase / liab_prep$nt * liab_prep$ncont / liab_prep$nt)
  r2 <- r2o * liab_prep$cv / (1 + r2o * liab_prep$theta * liab_prep$cv)
  r2
}


#' Nagelkerke $R^2$
#'
#' @param m glm or lm model (family="binomial" or family="gaussian")
#' @param n Total number of observations
#' @param ... Additional arguments
#' @return Numeric
#' @export
r2_nagelkerke <- function(m = m, n = n, ...) {
  (1 - exp((m$deviance - m$null.deviance) / n)) / (1 - exp(-m$null.deviance / n))
}


#' Weighted rank sum
#'
#' @keywords internal
weighted_rank_sum <- function(p, y, w) {
  df <- data.frame(p = p, y = y, w = w)
  df_sorted <- df[order(df$p), ]
  cumulative_weights <- cumsum(df_sorted$w)
  start_ranks <- c(0, cumulative_weights[-length(cumulative_weights)]) + 1
  end_ranks <- cumulative_weights
  avg_ranks <- (start_ranks + end_ranks) / 2
  sum(avg_ranks[df_sorted$y == 0] * df_sorted$w[df_sorted$y == 0])
}


#' ROC AUC
#'
#' @param p Probabilities from glm
#' @param y Outcome vector
#' @param w Weights, used when boot_method="blb"
#' @param ... Additional arguments
#' @return Numeric
#' @export
# NB, taken from Miron Kursa https://mbq.me
roc_auc <- function(p = p, y = y, w = w, ...) {
  if (!is.null(w)) {
    n1 <- sum(w * !y)
    n2 <- sum(w * y)
    U <- weighted_rank_sum(p, y, w) - n1 * (n1 + 1) / 2
    1 - U / n1 / n2
  } else {
    n1 <- sum(!y)
    n2 <- sum(y)
    U <- sum(frank(p)[!y]) - n1 * (n1 + 1) / 2
    1 - U / n1 / n2
  }
}


#' Mean Absolute Error
#'
#' @param p Predictions from lm
#' @param y Outcome vector
#' @param w Weights, used when boot_method="blb"
#' @param ... Additional arguments
#' @return Numeric
#' @export
mae <- function(p = p, y = y, w = w, ...) {
  inner <- abs(p - y)
  if (is.null(w)) {
    mean(inner)
  } else {
    stats::weighted.mean(inner, w)
  }
}
