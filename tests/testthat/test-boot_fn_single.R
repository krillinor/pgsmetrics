test_that("boot_standard handles quantitative outcome", {
  set.seed(123)

  metrics_list <- get_default_metrics() |>
      filter_metrics("continuous")

  # Simulate test data
  n <- 200
  data <- simulate_data(n)
  data_m <- data |> as.matrix()

  y <- data$y
  covars <- c("age")
  pgs <- c("pgs1", "pgs2")

  # Bootstrapping indices
  ind <- sample(n, n, replace = TRUE)

  # Run function
  result <- boot_standard(
    data_m,
    ind,
    boot_i = 1,
    quant = TRUE,
    K = NULL,
    pgs,
    covars,
    y,
    w = NULL,
    metrics_list = metrics_list
  )

  # print(metrics)

  # Validate result
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), length(pgs) * length(metrics_list))
  expect_true(all(c("pgs", "metric", "cov", "full", "partial", "boot_i") %in% names(result)))
})

test_that("boot_standard handles binary outcome", {
  set.seed(456)

  metrics_list <- get_default_metrics() |>
      filter_metrics("binary")

  # Simulate test data
  n <- 200
  data <- simulate_data(n)
  data_m <- data |> as.matrix()

  y <- data$y_bin
  covars <- c("age")
  pgs <- c("pgs1", "pgs2")

  # Bootstrapping indices
  ind <- sample(n, n, replace = TRUE)

  # Run function
  result <- boot_standard(
    data_m,
    ind,
    boot_i = 2,
    quant = FALSE,
    K = 0.2,
    pgs,
    covars,
    y,
    w = NULL,
    metrics_list = metrics_list
  )

  # Validate result
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), length(pgs) * length(metrics_list))
  expect_true(all(c("pgs", "metric", "cov", "full", "partial", "boot_i") %in% names(result)))
})

test_that("boot_standard handles edge cases", {
  set.seed(789)

  metrics_list <- get_default_metrics() |>
      filter_metrics("continuous")

  # Minimal data
  n <- 50
  data <- simulate_data(n)
  data_m <- data |> as.matrix()

  y <- data$y
  covars <- c("age")
  pgs <- c("pgs1")

  # Bootstrapping indices
  ind <- sample(n, n, replace = TRUE)

  # Run function
  result <- boot_standard(
    data_m,
    ind,
    boot_i = 3,
    quant = TRUE,
    K = NULL,
    pgs,
    covars,
    y,
    w = NULL,
    metrics_list = metrics_list
  )

  # Validate result
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), length(pgs) * length(metrics_list))
})
