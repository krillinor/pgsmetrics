test_that("metrics are correct for models with weights", {
  n <- 100
  d <- simulate_data(n = n, seed = 1)
  w <- rep(1:(n / 10), 10)
  dd <- d[rep(1:n, w), ]

  ###
  # binary
  ###
  x1 <- as.matrix(cbind(1, dd$g))
  x2 <- as.matrix(cbind(1, d$g))
  m1 <- fit_glm(x1, dd$y_bin, family = binomial())
  m2 <- fit_glm(x2, d$y_bin, weights = w, family = binomial())
  m_lm1 <- fit_glm(x1, dd$y, family = gaussian())
  m_lm2 <- fit_glm(x2, d$y, weights = w, family = gaussian())

  p1 <- stats::predict(m1, newdata = x1, type = "response")
  p2 <- stats::predict(m2, newdata = x2, type = "response")

  expect_equal(
    aic(m1),
    aic(m2)
  )

  expect_equal(
    brier_score(p = p1, y = dd$y_bin, w = NULL),
    brier_score(p = p2, y = d$y_bin, w = w)
  )

  expect_equal(
    r2_liability(m_lm = m_lm1, liab_prep = r2_liability_prep(dd$y_bin, K = 1 / 4), w = NULL),
    r2_liability(m_lm = m_lm2, liab_prep = r2_liability_prep(d$y_bin, K = 1 / 4, w = w), w = w)
  )

  expect_equal(
    r2_nagelkerke(m = m1, n = nrow(dd)),
    r2_nagelkerke(m = m2, n = nrow(dd))
  )

  expect_equal(
    roc_auc(p = p1, y = dd$y_bin, w = NULL),
    roc_auc(p = p2, y = d$y_bin, w = w)
  )

  ###
  # continuous
  ###
  expect_equal(
    aic(m_lm1, NULL),
    aic(m_lm2, w = w)
  )

  expect_equal(
    r2(m = m1, w = NULL),
    r2(m = m2, w = w)
  )

  # NB brier=mse for binary
  expect_equal(
    brier_score(p = p1, y = dd$y, w = NULL),
    brier_score(p = p2, y = d$y, w = w)
  )

  expect_equal(
    mae(p = p1, y = dd$y, w = NULL),
    mae(p = p2, y = d$y, w = w)
  )
})
