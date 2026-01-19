# simrun <- function(){
#   # ---- Simulation parameters ----
#
#   # Global
#
#   nsteps <- 500 # duration of the simulation
#
#   # Metacommunity
#
#   M <- 120 # metacommunity species richness
#   env_mean <- 0 # regional mean level of the environmental factor
#   env_sd <- 25 # regional variation of the environmental factor
#   cauchy <- 1.5 # shape of the regional trait abundance distribution
#   trait_sds <- 1 # intraspecific trait variation (same value for all or M-length vector)
#
#   # Local community
#
#   nind <- 100 # number of individuals in the initial community structure
#   age_crit <- 10 # age at which 1/2 of individuals die
#   mass_crit <- 5 # body mass at which 1/2 of individuals reproduce
#
#   # Local environment
#
#   env_local <- 10 # local environmental factor level
#   npatch <- 100
#   gradient <- "correlated"
#   K <- 3
#
#   # Time step
#
#   res0 <- tibble(patch = 1:npatch, res = 1000) # resource level at the beginning
#   res_input <- 10 # resource input per time step
#   R <- 1000 # sufficient resource abundance
#   recruitment <-  0.05
#   dispersal <- 0.05
#
#   # ---- Define metacommunity ----
#
#   mc <- init_meta(M = M, env_mean = env_mean, env_sd = env_sd, cauchy = cauchy, trait_sds = trait_sds)
#
#   # ---- Define local community ----
#
#   lcom <- draw_lcom(metacom = mc, nind = nind*npatch, age_crit = age_crit, mass_crit = mass_crit)
#   lcom$patch <- sample(1:npatch, nrow(lcom), replace = T)
#
#   # ---- Define local environment ----
#
#   lenv <- init_envt(npatch = npatch, gradient = gradient, K = K, env_mean = env_local, env_sd = 1)
#
#   # ---- Pre-define output list ----
#
#   sim_out <- vector(mode = "list", length = nsteps)
#
#   sim_out[[1]] <- time_step(
#     com = lcom,
#     metacom = mc,
#     envt = lenv,
#     age_crit = age_crit,
#     mass_crit = mass_crit,
#     res = res0,
#     res_input = res_input,
#     R = R,
#     dispersal = dispersal,
#     recruitment = recruitment
#   )
#
#   pb <- progress_bar$new(
#     format = "simulating [:bar] :percent elapsed: :elapsed eta: :eta tick :current",
#     clear = FALSE, total = nsteps-1, width = 60)
#
#   for (i in 2:nsteps){
#     com_clumped <- lapply(sim_out[[i-1]], function(i) i$com) %>% bind_rows()
#     res_clumped <- tibble(
#       patch = 1:length(sim_out[[i-1]]),
#       res = lapply(sim_out[[i-1]], function(i) i$res) %>% unlist()
#     )
#     sim_out[[i]] <- time_step(
#       com = com_clumped,
#       metacom = mc,
#       envt = lenv,
#       age_crit = age_crit,
#       mass_crit = mass_crit,
#       res = res_clumped,
#       res_input = res_input,
#       R = R,
#       dispersal = dispersal,
#       recruitment = recruitment
#     )
#     pb$tick()
#   }
#
#   return(sim_out)
#
# }
#
# ttime1 <- Sys.time()
# tst <- simrun()
# ttime2 <- Sys.time()
# difftime(ttime2, ttime1)

testthat::test_that("simulation runs", {
  mc = init_meta()
  lh = init_envt()
  lc = draw_lcom(mc = mc, lh = lh, nind = 100)
  lc = recruit(lc = lc, mc = mc, lh = lh, nind = 100)
  lc = dem(lc = lc, mc = mc)
  lc = disperse(lc = lc, lh = lh, dispersal = 10)
  lc = forage(lc = lc, lh = lh)

  testthat::expect_true(exists("lc"))
})

# mc = init_meta(env_mean_mc = 0, env_sd_mc = 1, cauchy = 1, trait_sds = 0.5)
# plot(mc)
# lh = init_envt(env_mean_lh = 2, env_sd_lh = 0.1, npatch = 100, gradient = "random")
# plot(lh)
# lc = draw_lcom(mc = mc, lh = lh, nind = 10000)
# plot(lc)
# # runsim1 <- run_sim(mc = mc, lh = lh, lc = lc, nsteps = 1000, progress_bar = T, recruitment = 0.05, dispersal = 10, res_input = 1, age_crit = 10, mass_crit = 2)
# # runsim1_ <- run_sim_(mc = mc, lh = lh, lc = lc, nsteps = 1000, progress_bar = T, recruitment = 0.05, dispersal = 10, res_input = 1, age_crit = 10, mass_crit = 2)
# runsim <- run_sim(mc = mc, lh = lh, lc = lc, nsteps = 1000, progress_bar = T, recruitment = 0, dispersal = 10, res_input = 10, age_crit = 10, mass_crit = 1.25)
# runsim_ <- run_sim_(mc = mc, lh = lh, lc = lc, nsteps = 1000, progress_bar = T, recruitment = 0, dispersal = 10, res_input = 10, age_crit = 10, mass_crit = 1.25)
# plot_run_sim(runsim1)
# runsim_$lcs %>% ggplot(aes(x = timestep, y = trait, color = factor(species))) + geom_point(size = 0.05, show.legend = F, alpha = 0.2)
# runsim_$lcs %>% group_by(timestep) %>% summarise(S = length(unique(species))) %>% plot()
