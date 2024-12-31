
# pgsmetrics

<!-- badges: start -->
<!-- badges: end -->

`pgsmetrics` makes it easy to compute metrics for polygenic scores and to bootstrap uncertainty intervals.

These metrics are:

1. Overall fit / model comparison
    - Nagelkerke's $R^2$ (binary)
    - Liability scale $R^2$ (binary)
    - Brier score (binary)
    - $R^2$ (continuous)
    - MSE (continuous)
    - MAE (continuous)
    - AIC
2. Discrimination:
    - ROC AUC (binary)
3. Calibration:
    - `plot_calibration`
4. Clinical utility
    - Net benefit, decision curve plot (`plot_dcurve`)

(It is also possible to add custom metrics → see `Metric` `R6` class.)

It is possible to use an alternative bootstrap method, the [Bag of Little Bootstraps](https://arxiv.org/abs/1112.5016) (BLB, `boot_type = "blb"`), which is useful when the number of samples and PGS is large (f.x., 500K samples and 50 PGS).

TODO link More: Get started vignette...

## Installation

``` r
devtools::install_github("krillinor/pgsmetrics")
```

## Example

``` r
library(pgsmetrics)

pgs_performance <- pgsmetrics(
    df,
    pgs = names_of_pgs,
    dep = name_of_outcome,
    covars = names_of_covariates,
    K = outcome_prevalence,
    n_boot = 1000,
    n_cores = 4
)
```

