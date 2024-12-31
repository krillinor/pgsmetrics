# TODO test covars = NULL

test_that("pgsmetrics arguments type", {
  d <- simulate_data(n = 100)
  n_boot <- 1
  pgs <- "pgs1"
  dep <- "y_bin"
  covars <- c("age", "sex")
  expect_error(pgsmetrics(d, type = "invalid", n_boot = n_boot, pgs = pgs, dep = dep, covars = covars))
  # ?
  # expect_error(pgsmetrics(d, n_boot = n_boot, pgs = pgs, dep = dep, covars = covars), NA)
})

test_that("pgsmetrics arguments R", {
  d <- simulate_data(n = 100)
  n_boot <- -1
  pgs <- "pgs1"
  dep <- "y_bin"
  covars <- c("age", "sex")
  expect_error(pgsmetrics(d, n_boot = n_boot, pgs = pgs, dep = dep, covars = covars))
})

test_that("pgsmetrics arguments pgs", {
  d <- simulate_data(n = 100)
  dep <- "y_bin"
  covars <- c("age", "sex")
  expect_error(pgsmetrics(d, pgs = c("pgs1", NA), dep = dep, covars = covars)) # missing
  expect_error(pgsmetrics(d, pgs = c("pgs1", "pgs1"), dep = dep, covars = covars)) # non-unique
})

test_that("pgsmetrics arguments dep", {
  d <- simulate_data(n = 100)
  pgs <- "pgs1"
  covars <- c("age", "sex")
  expect_error(pgsmetrics(d, pgs = pgs, dep = c("dep1", "dep2"), covars = covars)) # two
  expect_error(pgsmetrics(d, pgs = pgs, dep = c("dep1", NA), covars = covars)) # two
})

test_that("pgsmetrics arguments dep", {
  d <- simulate_data(n = 100)
  pgs <- "pgs1"
  dep <- "y_bin"
  expect_error(pgsmetrics(d, pgs = pgs, dep = dep, covars = c("age", NA))) # two
  expect_error(pgsmetrics(d, pgs = pgs, dep = dep, covars = c("age", "age"))) # missing
})

test_that("pgsmetrics handles standard bootstrap", {
  set.seed(123)

  # Simulate data
  data <- data.table(
    covar1 = rnorm(200),
    pgs1 = rnorm(200),
    dep = rnorm(200)
  )

  # Run function
  result <- pgsmetrics(
    data = data,
    pgs = "pgs1",
    dep = "dep",
    covars = "covar1",
    boot = TRUE,
    boot_method = "standard",
    n_boot = 10,
    n_cores = 1
  )

  # Validate result
  expect_s3_class(result, "pgsmetrics")
  expect_true(all(c("metrics", "ranks", "boot_output", "call") %in% names(result)))
})

test_that("pgsmetrics handles multiple PGS", {
  set.seed(789)

  # Simulate data
  data <- data.table(
    covar1 = rnorm(200),
    pgs1 = rnorm(200),
    pgs2 = rnorm(200),
    dep = rnorm(200)
  )

  # Run function
  result <- pgsmetrics(
    data = data,
    pgs = c("pgs1", "pgs2"),
    dep = "dep",
    covars = "covar1",
    boot = TRUE,
    boot_method = "standard",
    n_boot = 10,
    n_cores = 1
  )

  # Validate result
  expect_s3_class(result, "pgsmetrics")
  expect_true(all(c("metrics", "ranks", "boot_output", "call") %in% names(result)))
})


test_that("all columns exist in output tables", {
  set.seed(789)
  # Simulate data
  n <- 500
  d <- data.table(
    covar1 = rnorm(n),
    pgs1 = rnorm(n),
    pgs2 = rnorm(n),
    dep = rnorm(n)
  )
  d$dep_bin <- as.numeric(d$dep > 0)
  pgs <- c("pgs1", "pgs2")
  out_cols <- c("pgs", "metric", "type", "observed", "ci_lower", "ci_upper")
  out_cols_diff <- c("pgs1", "pgs2", "pgs1_pgs2", "metric", "observed", "ci_lower", "ci_upper")
  ##############
  # continuous #
  ##############
  r_standard <- pgsmetrics(data = d, pgs = pgs, dep = "dep", n_boot = 10, n_cores = 1)
  r_blb <- pgsmetrics(data = d, boot_method = "blb", pgs = pgs, dep = "dep", blb_b = 0.8, n_cores = 1) |> suppressWarnings()
  r_standard_noboot <- pgsmetrics(data = d, pgs = pgs, dep = "dep", n_boot = 10, n_cores = 1, boot = FALSE)
  ###
  # standard
  ###
  expect_equal(names(r_standard$metrics), out_cols)
  expect_equal(names(r_standard$ranks), out_cols)
  expect_equal(names(r_standard$diff), out_cols_diff)
  # boot=F
  expect_equal(names(r_standard_noboot$metrics), out_cols)
  expect_equal(names(r_standard_noboot$ranks), out_cols)
  expect_equal(names(r_standard_noboot$diff), out_cols_diff)
  ###
  # blb
  ###
  expect_equal(names(r_blb$metrics), out_cols)
  expect_equal(names(r_blb$ranks), out_cols)
  expect_equal(names(r_blb$diff), out_cols_diff)
  ##########
  # binary #
  ##########
  r_standard <- pgsmetrics(data = d, pgs = pgs, dep = "dep_bin", n_boot = 20, n_cores = 1)
  r_blb <- pgsmetrics(data = d, boot_method = "blb", pgs = pgs, dep = "dep_bin", blb_b = 0.8, n_cores = 1) |> suppressWarnings()
  r_standard_noboot <- pgsmetrics(data = d, pgs = pgs, dep = "dep_bin", n_boot = 20, n_cores = 1, boot = FALSE)
  ###
  # standard
  ###
  expect_equal(names(r_standard$metrics), out_cols)
  expect_equal(names(r_standard$ranks), out_cols)
  expect_equal(names(r_standard$diff), out_cols_diff)
  # boot=F
  expect_equal(names(r_standard_noboot$metrics), out_cols)
  expect_equal(names(r_standard_noboot$ranks), out_cols)
  expect_equal(names(r_standard_noboot$diff), out_cols_diff)
  ###
  # blb
  ###
  expect_equal(names(r_blb$metrics), out_cols)
  expect_equal(names(r_blb$ranks), out_cols)
  expect_equal(names(r_blb$diff), out_cols_diff)
})

test_that("missing='drop' works", {
  d <- simulate_data(n = 100)
  n_boot <- 1
  pgs <- c("pgs1", "pgs2")
  dep <- "y"
  d$y[1:10] <- NA
  expect_message({
    res <- pgsmetrics(d, n_boot = n_boot, pgs = pgs, dep = dep, missing = "drop")
  })
  expect_equal(res$sample_size, 90)
})

test_that("error if binary variable but not 0/1", {
  d <- simulate_data(n = 100)
  n_boot <- 1
  pgs <- c("pgs1", "pgs2")
  dep <- "y_bin"
  d$y_bin <- d$y_bin + 1
  expect_error({
    res <- pgsmetrics(d, n_boot = n_boot, pgs = pgs, dep = dep)
  })
})

# TODO error if binary but only single value
# test_that("", {})

test_that("S3 methods work", {
  d <- simulate_data(n = 100)
  n_boot <- 1
  pgs <- c("pgs1", "pgs2")
  dep <- "y_bin"
  res <- pgsmetrics(d, n_boot = n_boot, pgs = pgs, dep = dep)
  expect_no_error(print(res))
  expect_no_error(summary(res))
  res_blb <- pgsmetrics(d, boot_method = "blb", n_boot = n_boot, pgs = pgs, dep = dep) |> suppressWarnings()
  expect_no_error(print(res_blb))
  expect_no_error(summary(res_blb))
  # plot
  expect_s3_class(plot(res), "ggplot")
})
