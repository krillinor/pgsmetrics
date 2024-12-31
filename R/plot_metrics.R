plot_metrics_diff_heatmap <- function(pgsmetrics_object, ncol, nrow, title) {
  glist <- list()
  ns_metrics <- unique(pgsmetrics_object$metric)
  for (i in seq_len(length(ns_metrics))) {
    metric_i <- as.character(ns_metrics[i])
    g <- ggplot(
      pgsmetrics_object[metric == metric_i, ],
      aes(x = pgs1, y = pgs2, fill = observed)
    ) +
      geom_tile() +
      labs(
        title = title,
        x = expression(PGS[1]),
        y = expression(PGS[2]),
        fill = metric_i
      ) +
      theme_bw() +
      scale_fill_gradient2(low = "blue", midpoint = 0, high = "red") +
      theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
    glist[[i]] <- g
  }
  cowplot::plot_grid(plotlist = glist, nrow = nrow, ncol = ncol)
}


plot_metrics_diff <- function(pgsmetrics_object, ncol, nrow, title) {
  if (all(is.na(pgsmetrics_object$ci_lower)) && all(is.na(pgsmetrics_object$ci_lower))) {
    g <- ggplot(pgsmetrics_object, aes(x = observed, y = pgs1_pgs2)) +
      geom_point(size = 1)
  } else {
    g <- ggplot(pgsmetrics_object, aes(x = observed, y = pgs1_pgs2, xmin = ci_lower, xmax = ci_upper)) +
      geom_pointrange(size = 1 / 4)
  }

  g +
    geom_vline(xintercept = 0, color = "#999999", linetype = "dashed") +
    facet_wrap(~description, scales = "free_x", ncol = ncol, nrow = nrow, labeller = label_parsed) +
    labs(
      title = title,
      x = "Difference",
      y = expression(PGS[1] - PGS[2]),
      color = expression(PGS[2])
    ) +
    theme_bw() +
    scale_color_discrete() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


#' Plot PGS Metrics
#'
#' @param pgsmetrics_object TODO.
#' @param type TODO.
#' @param plot_type TODO.
#' @param model_type TODO.
#' @param order_by_metric TODO.
#' @param title TODO.
#' @param nrow TODO.
#' @param ncol TODO.
#' @return A ggplot object
#' @import data.table
#' @import ggplot2
#' @export
plot_metrics <- function(
    pgsmetrics_object,
    type = "metrics",
    plot_type = "points",
    model_type = "partial",
    order_by_metric = "aic",
    title = NULL,
    nrow = NULL,
    ncol = 1) {
  checkmate::assert_class(pgsmetrics_object, "pgsmetrics")
  checkmate::assert_choice(type, c("metrics", "ranks", "diff"))
  checkmate::assert_choice(plot_type, c("points", "heatmap"))
  checkmate::assert_choice(model_type, c("cov", "full", "partial"))
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_number(nrow, lower = 1, null.ok = TRUE)
  checkmate::assert_number(ncol, lower = 1, null.ok = TRUE)

  if (is.null(pgsmetrics_object[[type]])) stop(paste0("Something went wrong. There's no data under 'pgsmetrics_object[[type]]'."))

  name_description_map <- get_name_description_map(pgsmetrics_object$metrics_list)
  pgsmetrics_object <- merge(pgsmetrics_object[[type]], name_description_map, by.x = "metric", by.y = "name")

  if (type == "diff") {
    if (plot_type == "heatmap") {
      return(plot_metrics_diff_heatmap(pgsmetrics_object, ncol, nrow, title))
    } else {
      return(plot_metrics_diff(pgsmetrics_object, ncol, nrow, title))
    }
  }

  if ("type" %in% names(pgsmetrics_object)) {
    pgsmetrics_object <- pgsmetrics_object[type == model_type, ]
    partial_aic <- pgsmetrics_object[metric == order_by_metric & type == model_type, ]
  } else {
    partial_aic <- pgsmetrics_object[metric == order_by_metric, ]
  }
  partial_aic_order <- partial_aic$pgs[order(partial_aic$observed)]
  pgsmetrics_object$pgs <- factor(pgsmetrics_object$pgs, levels = rev(partial_aic_order))

  if (model_type == "partial") {
    labs_x <- "Incremental value (full - cov)"
  } else if (model_type == "cov") {
    labs_x <- expression(Value[cov])
  } else {
    labs_x <- expression(Value[full])
  }

  if (all(is.na(pgsmetrics_object$ci_lower)) && all(is.na(pgsmetrics_object$ci_lower))) {
    g <- ggplot(pgsmetrics_object, aes(x = observed, y = pgs)) +
      geom_point(size = 1)
  } else {
    g <- ggplot(pgsmetrics_object, aes(x = observed, y = pgs, xmin = ci_lower, xmax = ci_upper)) +
      geom_pointrange(size = 1 / 4)
  }

  g <- g +
    facet_wrap(~description, scales = "free_x", ncol = ncol, nrow = nrow, labeller = label_parsed) +
    labs(
      title = title,
      x = labs_x,
      y = "PGS"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 6, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8)
    )

  g
}
