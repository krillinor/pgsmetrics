#' Process metrics from bootstrap output
#'
#' @param boot_stats Output from the bootstrapping.
#' @param boot_method standard/blb.
#' @return A data.table containing the processed metrics for each PGS.
#' @keywords internal
process_metrics <- function(boot_stats, boot_method = "standard") {
  checkmate::assert_choice(boot_method, c("standard", "blb"))
  if (boot_method == "standard") {
    boot_stats_processed <- boot_stats[, .(
      cov = quantile(cov, probs = c(0.025, 0.975)),
      full = quantile(full, probs = c(0.025, 0.975)),
      partial = quantile(partial, probs = c(0.025, 0.975))
    ), by = c("pgs", "metric")]
    boot_stats_processed2 <- melt(boot_stats_processed, id.vars = c("pgs", "metric"), variable.name = "type")
    boot_stats_processed2$ci_type <- rep_len(c("ci_lower", "ci_upper"), length.out = nrow(boot_stats_processed2))

    ranks <- boot_stats[, .(
      pgs = pgs,
      rank_full = frank(full, ties.method = "average"),
      rank_partial = frank(partial, ties.method = "average")
    ),
    by = c("boot_i", "metric")
    ]
    ranks_processed <- ranks[, .(
      full = quantile(rank_full, probs = c(0.025, 0.975)),
      partial = quantile(rank_partial, probs = c(0.025, 0.975))
    ), by = c("pgs", "metric")]
    ranks_processed2 <- melt(ranks_processed, id.vars = c("pgs", "metric"), variable.name = "type")
    ranks_processed2$ci_type <- rep_len(c("ci_lower", "ci_upper"), length.out = nrow(ranks_processed2))

    metrics <- dcast(boot_stats_processed2, pgs + metric + type ~ ci_type, value.var = "value", value.factor = FALSE)
    ranks <- dcast(ranks_processed2, pgs + metric + type ~ ci_type, value.var = "value", value.factor = FALSE)
  }
  if (boot_method == "blb") {
    # factor?
    boot_stats <- melt(boot_stats, id.vars = c("s", "r", "pgs", "metric"), variable.name = "type", variable.factor = FALSE)
    ranks <- boot_stats[,
      .(rank = frank(value, ties.method = "average"), pgs = pgs),
      by = c("s", "r", "metric", "type")
    ]
    ranks2 <- ranks[, .(
      ci_lower = signif(quantile(rank, probs = 0.025), 4),
      ci_upper = signif(quantile(rank, probs = 0.975), 4),
      observed = signif(mean(rank), 4)
    ),
    by = c("s", "pgs", "metric", "type")
    ]
    ranks3 <- ranks2[, .(
      ci_lower = mean(ci_lower),
      ci_upper = mean(ci_upper),
      observed = mean(observed)
    ),
    by = c("pgs", "metric", "type")
    ]
    res3 <- boot_stats[, .(
      ci_lower = signif(quantile(value, probs = 0.025), 4),
      ci_upper = signif(quantile(value, probs = 0.975), 4),
      observed = signif(mean(value), 4)
    ),
    by = c("s", "pgs", "metric", "type")
    ]
    metrics <- res3[, .(
      ci_lower = mean(ci_lower),
      ci_upper = mean(ci_upper),
      observed = mean(observed)
    ),
    by = c("pgs", "metric", "type")
    ]
    ranks <- ranks3
  }

  list(metrics = metrics, ranks = ranks)
}
