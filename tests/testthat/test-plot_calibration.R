test_that("plot_calibration returns a plot", {
  d <- simulate_data(n = 100)
  out <- plot_calibration(df = d, pgs = "pgs1", dep = "y_bin", covars = c("age", "pc1"))
  expect_s3_class(out, "ggplot")
})
