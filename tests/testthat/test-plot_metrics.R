test_that("plot_metrics returns a plot", {
  d <- simulate_data(n = 100)
  n_boot <- 1
  pgs <- c("pgs1", "pgs2")
  dep <- "y_bin"
  covars <- c("age", "sex", "pc1", "pc2")
  res <- pgsmetrics(d, n_boot = n_boot, pgs = pgs, dep = dep, covars = covars)
  out <- plot_metrics(res)
  expect_s3_class(out, "ggplot")
  out <- plot_metrics(res, type = "ranks")
  expect_s3_class(out, "ggplot")
  # TODO test that at least 2 pgs for diff
  out <- plot_metrics(res, type = "diff")
  expect_s3_class(out, "ggplot")
  out <- plot_metrics(res, type = "diff", plot_type = "heatmap")
  expect_s3_class(out, "ggplot")
  # no boot
  res_noboot <- pgsmetrics(d, n_boot = n_boot, pgs = pgs, dep = dep, covars = covars, boot = FALSE)
  out <- plot_metrics(res_noboot)
  expect_s3_class(out, "ggplot")
  out <- plot_metrics(res_noboot, type = "diff")
  expect_s3_class(out, "ggplot")
})

test_that("error if no data in pgsmetrics object", {
  d <- simulate_data(n = 100)
  n_boot <- 1
  pgs <- c("pgs1", "pgs2")
  dep <- "y_bin"
  covars <- c("age", "sex", "pc1", "pc2")
  res <- pgsmetrics(d, n_boot = n_boot, pgs = pgs, dep = dep, covars = covars)
  res$metrics <- NULL
  res$ranks <- NULL
  res$diff <- NULL
  expect_error(plot_metrics(res, type = "metrics"))
  expect_error(plot_metrics(res, type = "ranks"))
  expect_error(plot_metrics(res, type = "diff"))
})
