#' Helper function, logit
#'
#' @description
#' Logit-transformation of an input variable.
#'
#'
#' @param p A random value between 0 and 1
#' @param crit A critical value corresponding to p = 0.5
#'
#' @returns A random value following the logit function
#' @export
#'
#' @examples
#' pred_logit(runif(1), crit = 0.5)
pred_logit <- function(p, crit){
  #
  # `p` - random value between 0 and 1
  # `crit` - critical value corresponding to p = 0.5
  #
  # Output:
  # random value following the logit function
  #
  if (max(p) > 1 | min(p) < 0){
    stop("in pred_logit(...) p is not 0 < p < 1")
  }else{
    (p*crit)/(1-p)
  }
}
