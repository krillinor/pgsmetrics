test_that("boot_method='blb' and boot=FALSE not possible", {
  d <- simulate_data(n = 100)
  n_boot <- 1
  pgs <- c("pgs1", "pgs2")
  dep <- "y_bin"
  expect_error(pgsmetrics(d, boot_method = "blb", boot = F, n_boot = n_boot, pgs = pgs, dep = dep))
})


test_that("error if missing values and missing='warn'", {
  d <- simulate_data(n = 100)
  n_boot <- 1
  pgs <- c("pgs1", "pgs2")
  dep <- "y"
  d$y[1:10] <- NA
  expect_error(pgsmetrics(d, n_boot = n_boot, pgs = pgs, dep = dep))
})
