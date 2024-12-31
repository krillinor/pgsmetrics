process_diff <- function(boot_stats, pgs, boot_method) {
    pw2 <- NULL
    pw <- CJ(pgs1 = pgs, pgs2 = pgs)
    pw <- pw[pgs1 != pgs2 & pgs1 < pgs2, ]
    pw2 <- merge(pw, boot_stats, by.x = "pgs1", by.y = "pgs", allow.cartesian = TRUE)
    if (boot_method == "standard") {
        by_x <- c("metric", "boot_i", "pgs2")
        by_y <- c("metric", "boot_i", "pgs")
        by_quantile <- c("pgs1_pgs2", "metric")
    }
    if (boot_method == "blb") {
        by_x <- c("metric", "s", "r", "pgs2")
        by_y <- c("metric", "s", "r", "pgs")
        by_quantile <- c("pgs1_pgs2", "metric", "s")
    }
    pw2 <- merge(pw2, boot_stats, by.x = by_x, by.y = by_y, suffixes = c("_pgs1", "_pgs2"))
    setkey(pw2, pgs1, pgs2)
    pw2[, diff_full := full_pgs1 - full_pgs2]
    cols_to_drop <- names(pw2)[grep("*_pgs[1-2]", names(pw2))]
    pw2[, (cols_to_drop) := NULL]
    pw2[, pgs1_pgs2 := paste(pgs1, "minus", pgs2, sep = "_")]
    # print(names(pw2))
    if (boot_method == "standard") {
        pw2 <- pw2[, .(
            pgs1 = pgs1[1],
            pgs2 = pgs2[1],
            ci_lower = quantile(diff_full, probs = c(0.025, 0.975))[1],
            ci_upper = quantile(diff_full, probs = c(0.025, 0.975))[2]
        ), by = by_quantile]
        # NB possible to use before=
        setcolorder(pw2, c("pgs1", "pgs2", "pgs1_pgs2", "metric", "ci_lower", "ci_upper"))
    }
    if (boot_method == "blb") {
        pw2 <- pw2[, .(
            pgs1 = pgs1[1],
            pgs2 = pgs2[1],
            observed = mean(diff_full),
            ci_lower = quantile(diff_full, probs = c(0.025, 0.975))[1],
            ci_upper = quantile(diff_full, probs = c(0.025, 0.975))[2]
        ), by = by_quantile]
        pw2 <- pw2[, .(
            pgs1 = pgs1[1],
            pgs2 = pgs2[1],
            observed = mean(observed),
            ci_lower = mean(ci_lower),
            ci_upper = mean(ci_upper)
        ),
        by = c("pgs1_pgs2", "metric")
        ]
        # NB possible to use before=
        setcolorder(pw2, c("pgs1", "pgs2", "pgs1_pgs2", "metric", "observed", "ci_lower", "ci_upper"))
    }
    pw2
}
