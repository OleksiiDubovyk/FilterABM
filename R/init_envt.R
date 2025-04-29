#' Initialize the local environment
#'
#' @description
#' Create a tibble (dataframe) representing the local habitat as a set of habitat patches with slightly different levels of the environmental factor.
#'
#' @param npatch Positive integer, number of patches in the environment.
#'
#' @param gradient Character, the rule by which patches get their values of env, either:
#' \code{gradient = "random"} - default value, env is an independent random variable drawn from N(env_mean, env_sd),
#' \code{gradient = "linear"} - env changes linearly from patch number 1 to patch number \code{npatch} with min and max drawn from 95% bound of N(env_mean, env_sd),
#' \code{gradient = "correlated"} - environmental factor is correlated with patch number with correlation coefficient equal \code{rho}, or
#' \code{gradient = "clustered"} - for K clusters, there are linearly distributed local env_means and small env_sd.
#'
#' @param K Positive integer, number of clusters if \code{gradient = "clustered"}.
#'
#' @param env_mean Numeric, mean value. of the environmental factor across all habitat patches.
#'
#' @param env_sd Positive numeric, variation of the environmental factor across all habitat patches.
#'
#' @param rho Positive numeric between \code{0} and \code{1}, correlation coefficient between patch number and environmental factor when \code{gradient = "correlated"}.
#'
#' @returns A tibble (dataframe) representing habitat patches (column \code{patch}) and the patch-specific value of the environmental factor (column \code{env}).
#'
#' @importFrom stats rnorm
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' init_envt()
#'
init_envt <- function(npatch = 10, gradient = "random", K = 3, env_mean = 0, env_sd = 25, rho = 0.75){
  #
  # `npatch` - number of patches in the environment
  # `gradient` - the rule by which patches get their values of env, either:
  #   `gradient = "random"` - env is an independent random variable drawn from N(env_mean, env_sd)
  #   `gradient = "linear"` - env changes linearly from patch number 1 to patch number npatch with min and max drawn from 95% bound of N(env_mean, env_sd)
  #   `gradient = "correlated"` - environmental factor is correlated with patch number with correlation coefficient equal `rho`
  #   `gradient = "clustered"` - for K clusters, there are linearly distributed local env_means and small env_sd
  # `K` - number of clusters if `gradient = "clustered"`
  # `env_mean` - regional mean of the environmental factor
  # `env_sd` - regional variation of the environmental factor
  # `rho` - correlation coefficient between patch number and environmental factor
  #
  # Output:
  # tibble (dataframe) with the following columns:
  #   `$patch` - patch number
  #   `$env` - environmental factor
  #
  # Note: the model is one-dimensional, $patch represents relative patch position
  #
  envd = rnorm(npatch, mean = env_mean, sd = env_sd)
  env_min <- quantile(envd, 0.025) %>% unname()
  env_max <- quantile(envd, 0.975) %>% unname()
  if (gradient == "random"){
    tibble(
      patch = 1:npatch,
      env = envd
    )
  }else if (gradient == "linear"){
    tibble(
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
    tibble(
      patch = 1:npatch,
      env = envc
    )
  }else if(gradient == "correlated"){
    envc <- simcor(x = 1:npatch, ymean = env_mean, ysd = env_sd, correlation = rho)
    tibble(
      patch = 1:npatch,
      env = envc
    )
  }
}
