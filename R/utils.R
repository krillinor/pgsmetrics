#' Fit Generalized Linear Model
#'
#' @param x Matrix of predictors
#' @param y Vector of responses
#' @return Fitted GLM object
#' @importFrom fastglm fastglm
#' @importFrom stats binomial
#' @keywords internal
fit_glm <- function(x, y, family = binomial(), weights = NULL, fastglm_method = 2) {
  # NB fastglm_method=2 is LLT
  fastglm::fastglm(x = x, y = y, weights = weights, family = family, method = fastglm_method)
}
