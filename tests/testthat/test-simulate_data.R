# library(testthat)
# library(data.table)

test_that("get_error_variance returns correct calculations", {
  # Test basic calculation
  expect_equal(
    get_error_variance(var_true_pgs = 0.25, r2_true_pgs = 0.25, r2_target = 0.125),
    0.25
  )

  # Test with different input values
  expect_equal(
    get_error_variance(var_true_pgs = 0.3, r2_true_pgs = 0.3, r2_target = 0.1),
    0.6
  )

  # Test zero case
  expect_equal(
    get_error_variance(var_true_pgs = 0.1, r2_true_pgs = 0.1, r2_target = 0.1),
    0
  )
})

test_that("simulate_data generates correct dataset", {
  # Test default parameters
  set.seed(2)
  data <- simulate_data()

  # Check dimensions
  expect_equal(nrow(data), 10000)
  expect_equal(ncol(data), 15) # 4 base + 5 PGS + 2 outcome + 4 PC columns

  # Check column names
  expected_columns <- c(
    "g", "e", "age", "sex", "pc1", "pc2", "pc3", "pc4",
    paste0("pgs", 1:5), "y", "y_bin"
  )
  expect_true(all(expected_columns %in% names(data)))

  # Validate binary outcome
  expect_true(all(data$y_bin %in% c(0, 1)))
  expect_equal(mean(data$y_bin), 0.25, tolerance = 0.05)
})

test_that("simulate_data handles parameter variations", {
  # Test different number of PGS
  data_3_pgs <- simulate_data(n_pgs = 3)
  expect_equal(sum(grepl("^pgs", names(data_3_pgs))), 3)

  # Test different sample size
  data_small <- simulate_data(n = 1000)
  expect_equal(nrow(data_small), 1000)

  # Test different seed
  set.seed(42)
  data_seed <- simulate_data(seed = 42)
  expect_true(exists("data_seed"))
})

test_that("simulate_data parameter validations", {
  # Test invalid parameters throw errors
  expect_error(simulate_data(n = -100), "Assertion on 'n'")
  expect_error(simulate_data(pgs_true_her_explained = 1.5), "Assertion on 'pgs_true_her_explained'")
  expect_error(simulate_data(dep_prevalence = 2), "Assertion on 'dep_prevalence'")
})
