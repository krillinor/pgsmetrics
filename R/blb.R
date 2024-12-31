boot_blb <- function(
    j = j,
    ind = ind,
    data = data,
    n = n,
    b = b,
    r = r,
    K = NULL,
    pgs = pgs,
    quant = FALSE,
    dep = dep,
    covars = covars) {
  metrics_type <- ifelse(quant, "continuous", "binary")
  metrics_list <- get_default_metrics() |>
      filter_metrics(metrics_type)
  n_pgs <- length(pgs)
  n_metrics <- length(metrics_list)
  liab_prep <- NULL
  if (is.null(covars)) {
    x_cov <- matrix(1, nrow = length(ind))
  } else {
    x_cov <- cbind(intercept = 1, as.matrix(data[ind, ..covars]))
  }
  y <- data[[dep]][ind]
  w <- stats::rmultinom(1, n, rep(1, b))[, 1]
  if (quant) {
    m_lm_cov <- NULL
    m_glm_cov <- fit_glm(x_cov, y, weights = w, family = stats::gaussian())
  } else {
    liab_prep <- r2_liability_prep(data[[dep]][ind], K = K, w = w)
    m_lm_cov <- fit_glm(x_cov, y, weights = w, family = stats::gaussian())
    m_glm_cov <- fit_glm(x_cov, y, weights = w)
  }
  p_glm_cov <- stats::predict(m_glm_cov, newdata = x_cov, type = "response")
  cov <- list()
  for (ii in seq_len(n_metrics)) {
    metric <- metrics_list[[ii]]
    tmp_cov <- metric$calculate(
      p = p_glm_cov,
      y = y,
      m = m_glm_cov,
      m_lm = m_lm_cov,
      liab_prep = liab_prep,
      n = n,
      w = w
    )
    cov[[ii]] <- tmp_cov
  }
  res <- list()
  for (i in seq_len(n_pgs)) {
    pgs_i <- pgs[i]
    x_full <- cbind(x_cov, data[[pgs_i]][ind])
    if (quant) {
      m_lm_full <- NULL
      m_glm_full <- fit_glm(x_full, y, weights = w, family = stats::gaussian())
    } else {
      m_lm_full <- fit_glm(x_full, y, weights = w, family = stats::gaussian())
      m_glm_full <- fit_glm(x_full, y, weights = w)
    }
    p_glm_full <- stats::predict(m_glm_full, newdata = x_full, type = "response")
    for (ii in seq_len(n_metrics)) {
      metric <- metrics_list[[ii]]
      tmp_full <- metric$calculate(p = p_glm_full, y = y, m = m_glm_full, m_lm = m_lm_full, liab_prep = liab_prep, n = n, w = w)
      # TODO make a test for this? cov was tmp_cov -> bug
      tmp_res <- data.table::data.table(
        s = j, r = r,
        pgs = pgs_i,
        metric = metric$name,
        cov = cov[[ii]],
        full = tmp_full,
        partial = tmp_full - cov[[ii]]
      )
      res <- append(res, list(tmp_res))
    }
  }
  rbindlist(res)
}
