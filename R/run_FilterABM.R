#' Run the simulation with enclosed evaluation
#'
#' @description
#' A slightly overparametrized function.
#' The objective is to make a model run extremely user-friendly, so that only the simulation parameters are supplied, and all objects needed for the simulation are evaluated internally.
#'
#' Usage is not recommended, please use \code{run_sim()} instead.
#'
#'
#' @param nsteps Positive integer, number of simulation time steps.
#'
#' @param M Integer, species richness within metacommunity, positive integer. Feeds into \code{init_meta()}.
#' @param env_mean_mc Numeric, mean environmental factor value within the extent of a metacommunity. Feeds into \code{init_meta()}.
#' @param env_sd_mc Non-negative numeric, variation of environmental factor value within the extent of a metacommunity. Feeds into \code{init_meta()}.
#' @param cauchy Non-negative numeric, delta parameter in Cauchy function used to connect species' trait values and log-abundances. Feeds into \code{init_meta()}.
#' @param trait_sds Non-negative numeric, intraspecific trait variation; either a single value if same for all species, or a M-length vector. Feeds into \code{init_meta()}.
#' @param max_abun Numeric, the maximum abundance allowed for the most abundant species in the metacommunity. Feeds into \code{init_meta()}.
#'
#' @param npatch Positive integer, number of patches in the environment. Feeds into \code{init_envt()}.
#' @param res Non-negative numeric, available resource level within each patch of the local habitat. Feeds into \code{init_envt()}.
#' @param gradient Feeds into \code{init_envt()}. Character, the rule by which patches get their values of env, either:
#' \code{gradient = "random"} - default value, env is an independent random variable drawn from N(env_mean, env_sd),
#' \code{gradient = "linear"} - env changes linearly from patch number 1 to patch number \code{npatch} with min and max drawn from 95% bound of N(env_mean, env_sd),
#' \code{gradient = "correlated"} - environmental factor is correlated with patch number with correlation coefficient equal \code{rho}, or
#' \code{gradient = "clustered"} - for K clusters, there are linearly distributed local env_means and small env_sd.
#' @param K Positive integer, number of clusters if \code{gradient = "clustered"}. Feeds into \code{init_envt()}.
#' @param env_mean_lh Numeric, mean value. of the environmental factor across all habitat patches. Feeds into \code{init_envt()}.
#' @param env_sd_lh Positive numeric, variation of the environmental factor across all habitat patches. Feeds into \code{init_envt()}.
#' @param rho Positive numeric between \code{0} and \code{1}, correlation coefficient between patch number and environmental factor when \code{gradient = "correlated"}. Feeds into \code{init_envt()}.
#'
#' @param recr_init Positive integer, a number of individuals to draw from the metacommunity. Feeds into \code{draw_lcom()}.
#' @param age_crit Numeric; critical age at which half of the individuals die. Feeds into \code{draw_lcom()} and \code{dem()}.
#' @param mass_crit Numeric; critical mass at which half of the individuals reproduce. Feeds into \code{draw_lcom()} and \code{dem()}.
#'
#' @param recruitment Non-negative double, recruitment rate (i.e., expectation of number of individuals recruited into local habitat per patch per time step). Feeds into \code{recruit()}.
#'
#' @param dispersal Non-negative numeric, dispersal rate per habitat patch, i.e., expectation of the number of individuals per patch that disperse to a neighboring patch. Feeds into \code{dispersal()}.

#' @param res_input Numeric, increment of the resource level within a time step. Feeds into \code{lh_input_res()}.

#' @param R Numeric, resource level at which all individuals within a community successfully consume the resource (i.e., probability of resource consumption equals one). Feeds into \code{forage()}.
#' @param clustering Numeric, effect of niche clustering on probability of competition (between 0 and 1). Default to one. Feeds into \code{forage()}.
#' @param dispersion Numeric, effect of niche dispersion on probability of trait filtering by the environment (between 0 and 1). Default to one. Feeds into \code{forage()}.
#'
#' @returns A list.
#'
#' @export
#'
run_FilterABM <- function(nsteps,
                          M = 120, env_mean_mc = 0, env_sd_mc = 25, cauchy = 1.5, trait_sds = 1, max_abun = 1e6,
                          npatch = 100, res = 1000, gradient = "correlated", K = 3, env_mean_lh = 10, env_sd_lh = 1, rho = 0.75,
                          recr_init = 100, age_crit = 10, mass_crit = 5,
                          recruitment = 0.05,
                          dispersal = 0.05,
                          res_input = 10, R = 1000, clustering = 1, dispersion = 1
                            ){

  if (!is.integer(nsteps)){
    stop("`nsteps` in `run_FilterABM` must be integer.")
  }

  if (nsteps < 1){
    stop("`nsteps` in `run_FilterABM` must be positive.")
  }

  t0 <- Sys.time()

  mc <- init_meta(M = M, env_mean_mc = env_mean_mc, env_sd_mc = env_sd_mc, cauchy = cauchy, trait_sds = trait_sds, max_abun = max_abun)
  lh <- init_envt(npatch = npatch, res = res, gradient = gradient, K = K, env_mean_lh = env_mean_lh, env_sd_lh = env_sd_lh, rho = rho)
  lc <- draw_lcom(mc = mc, lh = lh, nind = recr_init, age_crit = age_crit, mass_crit = mass_crit)

  lc_diagn <- vector("list", nsteps)
  lh_diagn <- numeric(nsteps)

  t_recruit <- numeric(nsteps)
  t_advage <- numeric(nsteps)
  t_dem <- numeric(nsteps)
  t_disperse <- numeric(nsteps)
  t_forage <- numeric(nsteps)

  for (step in 1:nsteps){
    t1 <- Sys.time()

    lc <- recruit(lc = lc, mc = mc, lh = lh, recruitment = recruitment)

    t2 <- Sys.time()
    t_recruit[step] <- difftime(t2, t1, units = "s")

    lc <- adv_age(lc = lc)

    t3 <- Sys.time()
    t_advage[step] <- difftime(t3, t2, units = "s")

    lc <- dem(lc = lc, mc = mc, age_crit = age_crit, mass_crit = mass_crit)

    t4 <- Sys.time()
    t_dem[step] <- difftime(t4, t3, units = "s")

    lc <- disperse(lc = lc, lh = lh, dispersal = dispersal)

    t5 <- Sys.time()
    t_disperse[step] <- difftime(t5, t4, units = "s")

    res_prior <- sum(lh$res)
    frg_out <- forage(lc = lc, lh = lh, R = R, clustering = clustering, dispersion = dispersion)
    res_post <- sum(frg_out[["lh"]]$res)
    lh_diagn[step] <- res_prior - res_post

    t6 <- Sys.time()
    t_forage[step] <- difftime(t6, t5, units = "s")


    lc <- frg_out[["lc"]]
    lh <- lh_input_res(lh = frg_out[["lh"]], res_input = res_input)

    lc_diagn[[step]] <- summary(lc)
  }

  tx <- Sys.time()

  t_recruit <- sum(t_recruit)
  t_advage <- sum(t_advage)
  t_dem <- sum(t_dem)
  t_disperse <- sum(t_disperse)
  t_forage <- sum(t_forage)
  tt <- t_recruit + t_advage + t_dem + t_disperse + t_forage

  cat(paste0("A run_FilterABM with ", nsteps, " time steps took ", round(as.numeric(difftime(tx, t0, units = "m")), digits = 3), " minutes with the total time across functions:\n"))
  cat(paste0(" - recruit  --- ", round(t_recruit, digits = 3), " sec --- ", round(t_recruit*100/tt, digits = 2), "%\n"))
  cat(paste0(" - adv_age  --- ", round(t_advage, digits = 3), " sec --- ", round(t_advage*100/tt, digits = 2), "%\n"))
  cat(paste0(" - dem      --- ", round(t_dem, digits = 3), " sec --- ", round(t_dem*100/tt, digits = 2), "%\n"))
  cat(paste0(" - disperse --- ", round(t_disperse, digits = 3), " sec --- ", round(t_disperse*100/tt, digits = 2), "%\n"))
  cat(paste0(" - forage   --- ", round(t_forage, digits = 3), " sec --- ", round(t_forage*100/tt, digits = 2), "%\n"))

  list(lh_diagn, lc_diagn, lc)

}

# x <- run_FilterABM(nsteps = 500L, gradient = "clustered", dispersal = 0.1, recruitment = 0.1, clustering = 0, dispersion = 0)
#
# sapply(1:500, function(i) x[[1]][[i]]) %>% plot(type = "l")
# sapply(1:500, function(i) x[[2]][[i]]$nind) %>% plot(type = "l")
# sapply(1:500, function(i) x[[2]][[i]]$species_richness) %>% plot(type = "l")
# sapply(1:500, function(i) x[[2]][[i]]$mean_trait) %>% plot(type = "l")
# x[[3]]
