test_that("process_metrics boot_method = 'standard'", {
  n_boot <- 20
  exp_boot_output <- data.table(
    pgs = rep("pgs1", n_boot),
    metric = rep("test_metric", n_boot),
    cov = rnorm(n_boot),
    full = rnorm(n_boot),
    boot_i = 1:n_boot
  )
  exp_boot_output$partial <- exp_boot_output$full - exp_boot_output$cov
  proc <- process_metrics(exp_boot_output, boot_method = "standard")
  # ?
  exp_boot_output_proc <- data.table(
    pgs = "pgs1",
    metric = "test_metric",
    type = c("cov", "full", "partial") |> as.factor(),
    ci_lower = c(
      quantile(exp_boot_output$cov, probs = c(0.025)),
      quantile(exp_boot_output$full, probs = c(0.025)),
      quantile(exp_boot_output$partial, probs = c(0.025))
    ),
    ci_upper = c(
      quantile(exp_boot_output$cov, probs = c(0.975)),
      quantile(exp_boot_output$full, probs = c(0.975)),
      quantile(exp_boot_output$partial, probs = c(0.975))
    )
  )
  setkey(exp_boot_output_proc, pgs, metric, type)
  expect_equal(proc$metrics, exp_boot_output_proc)
})
