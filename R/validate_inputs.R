#' Validate inputs for pgsmetrics function
#'
#' @param data A data.table containing necessary data.
#' @param pgs Character vector. Names of PGS columns.
#' @param dep Character. Name of the dependent variable column.
#' @param covars Character vector. Names of covariate columns.
#' @param n_boot Integer. Number of bootstrap iterations.
#' @param n_cores Integer. Number of cores for parallel processing.
#' @return NULL. Throws an error if any input is invalid.
#' @keywords internal
#' @export
validate_inputs <- function(
    data,
    K,
    pgs,
    dep,
    covars,
    boot_method,
    blb_s,
    blb_r,
    blb_b,
    n_boot,
    n_cores,
    boot,
    missing) {
  checkmate::assert_data_frame(data)
  checkmate::assert_character(pgs, min.len = 1, any.missing = FALSE, unique = TRUE)
  checkmate::assert_string(dep)
  checkmate::assert_character(covars, any.missing = FALSE, unique = TRUE, null.ok = TRUE)
  checkmate::assert_choice(boot_method, c("standard", "blb"))
  checkmate::assert_number(K, lower = 0, upper = 1, null.ok = TRUE)
  checkmate::assert_number(blb_s)
  checkmate::assert_number(blb_r)
  checkmate::assert_number(blb_b, lower = 0, upper = 1)
  checkmate::assert_count(n_boot, positive = TRUE)
  checkmate::assert_count(n_cores, positive = TRUE)
  checkmate::assert_choice(missing, c("warn", "drop"))

  checkmate::assert_subset(c(pgs, dep, covars), colnames(data))
  checkmate::assert_true(all(sapply(data[, ..pgs], is.numeric)))

  checkmate::assert_numeric(data[[dep]])

  if (boot_method == "blb" && !boot) {
    stop("this combination is not possible, boot=F is only possible with boot_method='standard'")
  }

  if (missing == "warn") {
    check_missing(data, dep, pgs, covars)
  }
}


check_missing <- function(data, dep, pgs, covars) {
  cols_to_use <- c(dep, pgs, covars)
  data <- data[, ..cols_to_use]
  if (any(is.na(data))) {
    missing_info <- colSums(is.na(data))
    missing_cols <- missing_info[missing_info > 0]
    print(missing_cols)
    missing_msg <- paste0(
      "Warning: The data contains missing values in the following columns: ",
      paste(names(missing_cols), collapse = ", "),
      ".\nRecommendation: handle missingness with imputation.\nTo continue without imputation, use missing='drop' to use complete cases."
    )
    if (length(missing_cols) > 0) stop(missing_msg)
  }
}
