boot_standard <- function(
    data_m,
    ind,
    boot_i,
    quant,
    K,
    pgs,
    covars,
    y,
    w,
    metrics_list) {
    n <- length(ind)
    metrics_type <- if (quant) "continuous" else "binary"
    n_pgs <- length(pgs)
    n_metrics <- length(metrics_list)
    ns_metrics <- names(metrics_list)

    y <- y[ind]
    liab_prep <- if (!quant) r2_liability_prep(y, K = K, w = w) else NULL

    if (is.null(covars)) {
        data_m_ind <- 1
    } else {
        data_m_ind <- 1:(1 + length(covars))
    }
    x_cov <- data_m[ind, data_m_ind, drop = FALSE]

    if (quant) {
        m_lm_cov <- NULL
        m_glm_cov <- fit_glm(x_cov, y, family = stats::gaussian(), weights = w)
    } else {
        # lm is for liability stuff
        m_lm_cov <- fit_glm(x_cov, y, family = stats::gaussian(), weights = w)
        m_glm_cov <- fit_glm(x_cov, y, family = binomial(), weights = w)
    }

    p_glm_cov <- stats::predict(m_glm_cov, newdata = x_cov, type = "response")
    metric_cov <- list()
    metric_cov <- lapply(metrics_list, function(metric) {
        metric$calculate(
            p = p_glm_cov,
            y = y,
            m = m_glm_cov,
            m_lm = m_lm_cov,
            liab_prep = liab_prep,
            n = n,
            w = w
        )
    })

    results <- list()
    for (i in seq_len(n_pgs)) {
        pgs_i <- pgs[i]
        results_tmp <- data.table(
            pgs = pgs_i,
            metric = ns_metrics
        )
        results_tmp$cov <- numeric(n_metrics)
        results_tmp$full <- numeric(n_metrics)
        results_tmp$partial <- numeric(n_metrics)
        pgs_i <- pgs[i]
        x_full <- cbind(x_cov, data_m[ind, pgs_i])
        colnames(x_full)[length(colnames(x_full))] <- pgs_i
        if (quant) {
            m_lm_full <- NULL
            m_glm_full <- fit_glm(x_full, y, family = stats::gaussian(), weights = w)
        } else {
            m_lm_full <- fit_glm(x_full, y, family = stats::gaussian(), weights = w)
            m_glm_full <- fit_glm(x_full, y, family = binomial(), weights = w)
        }
        p_glm_full <- stats::predict(m_glm_full, newdata = x_full, type = "response")
        for (j in seq_len(n_metrics)) {
            metric <- metrics_list[[j]]
            tmp_full <- metric$calculate(
                p = p_glm_full,
                y = y,
                m = m_glm_full,
                m_lm = m_lm_full,
                liab_prep = liab_prep,
                n = n,
                w = w
            )
            results_tmp$cov[j] <- metric_cov[[j]]
            results_tmp$full[j] <- tmp_full
            results_tmp$partial[j] <- tmp_full - metric_cov[[j]]
        }
        results <- append(results, list(results_tmp))
    }
    results <- rbindlist(results)
    results$boot_i <- boot_i
    results
}
