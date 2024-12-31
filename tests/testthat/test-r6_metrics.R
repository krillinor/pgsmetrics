test_that("register_metric works", {
    test_metric <- function(m = m, ...) {
        AIC(m)
    }
    pre <- get_default_metrics() |>
        filter_metrics("binary")
    pre_length <- length(pre)
    # add a new metric
    post <- register_metric(pre, "test_metric", test_metric, type = "binary", description = "Test metric")
    post_length <- length(post)
    expect_equal(pre_length + 1, post_length)
    # not duplicated
    post2 <- register_metric(post, "test_metric", test_metric, type = "binary", description = "Test metric")
    post2_length <- length(post2)
    expect_equal(post_length, post2_length)
})
