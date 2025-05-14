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


#' Initialize the local environment
#'
#' @description
#' Create a tibble (dataframe) representing the local habitat as a set of habitat patches with slightly different levels of the environmental factor.
#'
#' @param npatch Positive integer, number of patches in the environment.
#'
#' @param res Non-negative numeric, available resource level within each patch of the local habitat.
#'
#' @param gradient Character, the rule by which patches get their values of env, either:
#' \code{gradient = "random"} - default value, env is an independent random variable drawn from N(env_mean, env_sd),
#' \code{gradient = "linear"} - env changes linearly from patch number 1 to patch number \code{npatch} with min and max drawn from 95% bound of N(env_mean, env_sd),
#' \code{gradient = "correlated"} - environmental factor is correlated with patch number with correlation coefficient equal \code{rho}, or
#' \code{gradient = "clustered"} - for K clusters, there are linearly distributed local env_means and small env_sd.
#'
#' @param K Positive integer, number of clusters if \code{gradient = "clustered"}.
#'
#' @param env_mean_lh Numeric, mean value. of the environmental factor across all habitat patches.
#'
#' @param env_sd_lh Positive numeric, variation of the environmental factor across all habitat patches.
#'
#' @param rho Positive numeric between \code{0} and \code{1}, correlation coefficient between patch number and environmental factor when \code{gradient = "correlated"}.
#'
#' @returns A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lh}).
#'
#' @importFrom stats rnorm
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' init_envt()
#'
init_envt <- function(npatch = 10, res = 1000, gradient = "random", K = 3, env_mean_lh = 0, env_sd_lh = 25, rho = 0.75){

  env_mean <- env_mean_lh
  env_sd <- env_sd_lh

  envd = rnorm(npatch, mean = env_mean, sd = env_sd)
  env_min <- quantile(envd, 0.025) %>% unname()
  env_max <- quantile(envd, 0.975) %>% unname()
  if (gradient == "random"){
    lh <- tibble(
      patch = 1:npatch,
      env = envd
    )
  }else if (gradient == "linear"){
    lh <- tibble(
      patch = 1:npatch,
      env = seq(from = env_min, to = env_max, length.out = npatch)
    )
  }else if (gradient == "clustered"){
    env_sd_clust <- 0.1*env_sd
    env_means_clust <- seq(from = env_min, to = env_max, length.out = K)
    envc <- c(
      sapply(env_means_clust, function(x) rnorm(ceiling(npatch/K), mean = x, sd = env_sd_clust)) %>% unlist()
    )
    envc <- envc[1:npatch]
    lh <- tibble(
      patch = 1:npatch,
      env = envc
    )
  }else if(gradient == "correlated"){
    envc <- FilterABM::simcor(x = 1:npatch, ymean = env_mean, ysd = env_sd, correlation = rho)
    lh <- tibble(
      patch = 1:npatch,
      env = envc
    )
  }

  lh <- lh %>%
    mutate(res = res)

  FilterABM::FilterABM_lh(x = lh, gradient = gradient)
}
