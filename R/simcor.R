#' Helper, generate a random variable with set correlation coefficient to a focal variable
#'
#' @description
#' For a given vector \code{x}, generate an independent random vector of the same length such that correlation between \code{x} and output vector equals to a set coefficient \code{correlation}.
#'
#'
#' @param x Numeric, input vector.
#' @param ymean Numeric, mean of the output vector.
#' @param ysd Positive numeric, SD of the output vector.
#' @param correlation Correlation coefficient.
#'
#' @returns Numeric of the same length as \code{x}.
#'
#' @importFrom stats rnorm
#' @importFrom stats lm
#' @importFrom stats resid
#'
#' @export
#'
#' @examples
#' x = rnorm(100)
#' simcor(x)
#'
simcor <- function (x, ymean = 0, ysd = 1, correlation = 0) {
  n <- length(x)
  y <- rnorm(n)
  z <- correlation * scale(x)[,1] + sqrt(1 - correlation^2) *
    scale(resid(lm(y ~ x)))[,1]
  yresult <- ymean + ysd * z
  yresult
}
