#' @keywords internal
get_error_variance <- function(var_true_pgs, r2_true_pgs, r2_target) {
  var_true_pgs * (r2_true_pgs / r2_target - 1)
}

#' Simulate data for testing
#'
#' This function simulates a dataset with PGS, covariates, and dependent variables
#' for testing purposes.
#'
#' @param n The number of observations to simulate. Default: 10000.
#' @param seed The random seed for reproducibility. Default: NULL.
#' @param n_pgs The number of PGS to simulate. Default: 5.
#' @param pgs_true_her_explained Has to be >= 0 and <= 0.5. Default: 0.25.
#' @param pgs_her_explained_ub Lower bound of heritability explained by PGS.
#' @param pgs_her_explained_lb Upper bound of heritability explained by PGS.
#' @param age_prop_explained The proportion of `y` and `y_bin` explained by age. Default: 0.25.
#' @param env_prop_explained The proportion of `y` and `y_bin` explained by other environmental factors. Default: 0.5.
#' @param dep_prevalence Prevalence of the binary trait. Default: 0.25.
#' @return A data.table with simulated data
#'
#' @examples
#' # Simulate data
#' data <- simulate_data(n = 1000, n_pgs = 3)
#' print(data)
#'
#' @import data.table
#' @importFrom stats rnorm
#' @importFrom stats rbinom
#' @importFrom stats quantile
#' @export
simulate_data <- function(
    n = 1e4,
    seed = NULL,
    n_pgs = 5,
    pgs_true_her_explained = 0.25,
    pgs_her_explained_ub = pgs_true_her_explained / 2,
    pgs_her_explained_lb = pgs_true_her_explained / 4,
    age_prop_explained = 0.25,
    env_prop_explained = 0.5,
    dep_prevalence = 0.25) {
  checkmate::assert_integerish(n, lower = 1)
  checkmate::assert_integerish(seed, null.ok = TRUE)
  checkmate::assert_integerish(n_pgs)
  checkmate::assert_number(pgs_true_her_explained, lower = 0, upper = 0.5)
  checkmate::assert_number(pgs_her_explained_ub, lower = pgs_her_explained_lb, upper = pgs_true_her_explained)
  checkmate::assert_number(pgs_her_explained_lb, lower = 0, upper = pgs_her_explained_ub)
  checkmate::assert_number(dep_prevalence, lower = 0, upper = 1)

  if (!is.null(seed)) set.seed(seed)

  g <- rnorm(n, sd = sqrt(pgs_true_her_explained))
  e <- rnorm(n, sd = sqrt(env_prop_explained))
  age <- rnorm(n, sd = sqrt(age_prop_explained))
  # NB the trait doesn't depend on sex rn or pcs
  sex <- rbinom(n, 1, 0.5)
  pc1 <- stats::runif(n)
  pc2 <- stats::runif(n)
  pc3 <- stats::runif(n)
  pc4 <- stats::runif(n)
  d <- data.table::data.table(g, e, age, sex, pc1, pc2, pc3, pc4)

  error_term_variances <- get_error_variance(
    pgs_true_her_explained,
    pgs_true_her_explained,
    seq(from = pgs_her_explained_ub, to = pgs_her_explained_lb, length.out = n_pgs)
  )

  for (i in 1:n_pgs) {
    d[[paste0("pgs", i)]] <- d$g + rnorm(n, sd = sqrt(error_term_variances[i]))
  }
  d$y <- d$age + d$g + d$e
  d$y_bin <- as.integer(d$y > quantile(d$y, probs = 1 - dep_prevalence))

  d
}
