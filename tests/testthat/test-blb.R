test_that("boot_blb handles quantitative outcome", {
  set.seed(123)
  metrics_list <- get_default_metrics() |>
      filter_metrics("continuous")
  n <- 200
  data <- simulate_data(n)
  data_m <- data |> as.matrix()
  dep <- "y"
  y <- data[[dep]]
  covars <- c("age")
  pgs <- c("pgs1")
  # Parameters
  j <- 1
  blb_b <- 0.8
  b <- floor(nrow(data)^blb_b)
  ind <- sample(nrow(data), b)
  r <- 2
  covars <- c("age")
  pgs <- c("pgs1", "pgs2")
  # Run function
  result <- boot_blb(
    j = j,
    ind = ind,
    data = data,
    n = n,
    b = b,
    r = r,
    pgs = pgs,
    quant = TRUE,
    dep = dep,
    covars = covars
  ) |> suppressWarnings()
  # Validate result
  expect_s3_class(result, "data.table")
  expect_equal(c("s", "r", "pgs", "metric", "cov", "full", "partial"), names(result))
  expect_equal(nrow(result), length(pgs) * length(metrics_list))
})
