#' Regenerate resource in local habitat patches
#'
#' @param lh A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lh}).
#'
#' @param res_input Numeric, increment of the resource level within a time step.
#'
#' @returns A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lh}).
#'
#' @export
#'
#' @examples
#' lh1 = init_envt()
#' lh_input_res(lh = lh1, res_input = 10)
#'
lh_input_res <- function(lh, res_input = 0){
  lh %>%
    mutate(res = res + res_input,
           res = ifelse(res < 0, 0, res))
}

#' Simulate foraging within a habitat patch
#'
#' @description
#' The function takes in the description of the local community and surrounding environmental factors.
#' Based on these, each individual within a community takes a turn in random sequence consuming the available resource.
#' All else being equal, an individual with a trait value optimal for the local environmental factor \code{env} will consume more resource.
#' If there are many individuals in the community with a similar trait value, the focal individual is less likely to consume the resource.
#' Similarly, in an individuals-rich environment, the chances of consuming anything at all are reduced.
#'
#' @param lc A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc}).
#'
#' @param lh A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lh}).
#'
#' @param intake Positive numeric, single-meal resource intake by an individual in an event of a successful feeding.
#'
#' @param clustering Numeric, effect of niche clustering on probability of competition (between 0 and 1). Default to one.
#' If zero, local environmental conditions do not affect individual resource intake.
#'
#' @param dispersion Numeric, effect of niche dispersion on probability of trait filtering by the environment (between 0 and 1). Default to one.
#' If zero, trait differences among individuals do not affect resource intake.
#'
#' @returns A named list with:
#' \code{[["lc"]]} - A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc});
#' \code{[["lh"]]} - A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lh}).
#'
#' @importFrom stats weighted.mean
#' @importFrom purrr map_dbl
#' @export
#'
#' @examples
#' mc1 = init_meta()
#' lh1 = init_envt()
#' lc1 = draw_lcom(mc = mc1, lh = lh1, nind = 100)
#' lc1 = recruit(lc = lc1, mc = mc1, lh = lh1, nind = 100)
#' lc1 = dem(lc = lc1, mc = mc1)
#' lc1 = disperse(lc = lc1, lh = lh1, dispersal = 10)
#' forage(lc = lc1, lh = lh1)
#'
forage <- function(lc, lh, intake = 1, clustering = 1, dispersion = 1){

  # mc1 <- init_meta(env_mean_mc = 0, env_sd_mc = 1, cauchy = 1, trait_sds = 0.5)
  # lh1 = init_envt(env_mean_lh = 10, env_sd_lh = 0.1, npatch = 100, gradient = "random", res = 100)
  # lc1 = draw_lcom(mc = mc1, lh = lh1, nind = 10000)
  #
  # runsim <- run_sim(mc = mc1, lh = lh1, lc = lc1,
  #                    nsteps = 1000,
  #                    progress_bar = T, res_input = 10,
  #                    recruitment = 1, dispersal = 1, expenditure = 0.2, intake = 0.5, reproduction = 2,
  #                    clustering = 1, dispersion = 1)
  # plot(runsim)

  # formal checks
  if (any(c(clustering, dispersion) > 1)){
    stop("in `forage()`, clustering and dispersion parameters cannot be greater than 1")
  }
  if (any(c(clustering, dispersion) < 0)){
    stop("in `forage()`, clustering and dispersion parameters cannot be lower than 0")
  }
  if (intake < 0){
    stop("in `forage()`, intake parameter cannot be lower than 0")
  }

  # # # # # # # # # # # # #
  #                       #
  # V E R S I O N     3   #
  #                       #
  # # # # # # # # # # # # #

  # Individual resource consumption:
  # Null model - every individual consumes the same amount of resource (e.g., resource available / number of individuals)
  # Neutral model - a random subset of individuals consumes the resource
  # Niche model - individuals consume resource according to their trait value

  # get data about the environment
  lc <- left_join(
    lc,
    lh,
    by = "patch"
  )

  # determine meal size (cannot consume more than available resource)
  lc <- lc %>%
    group_by(patch) %>%
    mutate(maxmeal = min(mean(res)/n(), intake)) %>%
    ungroup()

  # #~~~~~~# NULL SCENARIO #~~~~~~#
  # # equilibrium number of individuals / patch = resource input * (1 / expenditure)
  # lc <- lc %>%
  #   mutate(meal = maxmeal) %>%
  #   mutate(mass = mass + meal)

  # #~~~~~~# NEAUTRAL SCENARIO #~~~~~~#
  # # individuals that feed are determined by sampling with replacement
  # # equilibrium nind is more tricky
  # # but given that in sampling with replacement, the probability of being selected at least once is 1-(1-n)^r
  # # and in our case 1-(1-n)^n,
  # # this is also the expectation of portion of individuals that feed
  # # somehow, equilibriium ind is approx res_input * 1 / (expenditure * 0.92)
  # nind <- nrow(lc)
  # lucky <- sample(1:nind, nind, replace = TRUE)
  # # dplyr used instead of %in% because it will be faster on big datasets, otherwise it would look like
  # # lc <- lc %>%
  # #   mutate(nmeals = sapply(1:nind, function(i) i %in% lucky)) %>%
  # #   mutate(meal = nmeals * maxmeal) %>%
  # #   mutate(mass = mass + meal)
  # lc <- lc %>%
  #   rowid_to_column(var = "id") %>%
  #   left_join(tibble(
  #     id = unique(lucky),
  #     nmeals = 1
  #   ), by = "id") %>%
  #   mutate(nmeals = ifelse(is.na(nmeals), 0, nmeals)) %>%
  #   mutate(meal = nmeals * maxmeal) %>%
  #   mutate(mass = mass + meal)

  #~~~~~~# NICHE SCENARIO #~~~~~~#
  # calculate facets affecting feeding
  lc <- lc %>%
    # mismatch between individual trait and environment
    mutate(
      t_env = abs(trait - env)
    ) %>%
    # trait difference from all other individuals
    group_by(patch) %>%
    mutate(t_ind = map_dbl(
      seq_along(trait),
      function(i) (trait[i] - trait[-i]) %>% abs() %>% mean()
    )) %>%
    ungroup() %>%
    mutate(t_ind = ifelse(is.nan(t_ind), Inf, t_ind)) %>% # fix NaNs for lone individuals
    # niche clustering (trait vs environment)
    # high prob if trait approximates environment
    mutate(p_clust = 1 - (1/(1 + exp(-log(t_env))))) %>%
    # niche dispersion (trait vs traits)
    # high prob if trait is unique compared to other individuals
    mutate(p_disp = (1/(1 + exp(-log(t_ind)))))
  pc <- mean(lc$p_clust)
  pd <- mean(lc$p_disp)
  lc <- lc %>%
    # recalculate for strength of clustering/dispersion
    # mutate(
    #   p_clust = map_dbl(p_clust,
    #                     function(p) weighted.mean(x = c(p, mean(p_clust)),
    #                                               w = c(clustering, 1 - clustering))),
    #   p_disp = map_dbl(p_disp,
    #                     function(p) weighted.mean(x = c(p, mean(p_disp)),
    #                                               w = c(dispersion, 1 - dispersion)))
    # ) %>%
    mutate(
      p_clust = clustering * p_clust + (1 - clustering) * pc,
      p_disp = dispersion * p_disp + (1 - dispersion) * pd
    ) %>%
    # total trait effect
    mutate(p_feed = p_clust * p_disp) %>%
    # get rid of intermediate columns
    select(species, trait, mass, patch, p_feed, maxmeal) %>%
    # recalculate for patch-specific
    group_by(patch) %>%
    mutate(p_feed = p_feed/sum(p_feed)) %>%
    # recalculate globally
    ungroup() %>%
    mutate(p_feed = p_feed/sum(p_feed))
  nind <- nrow(lc)
  lucky <- sample(1:nind, nind, replace = TRUE, prob = lc$p_feed)
  lc <- lc %>%
    rowid_to_column(var = "id") %>%
    left_join(tibble(
      id = unique(lucky),
      nmeals = 1
    ), by = "id") %>%
    mutate(nmeals = ifelse(is.na(nmeals), 0, nmeals)) %>%
    mutate(meal = nmeals * maxmeal) %>%
    mutate(mass = mass + meal)

  # update resource
  lh <- left_join(
    lh,
    lc %>%
      group_by(patch) %>%
      summarize(consumed = sum(meal)),
    by = "patch"
  ) %>%
    mutate(consumed = ifelse(is.na(consumed), 0, consumed)) %>%
    mutate(res = res - consumed) %>%
    select(-consumed) %>%
    FilterABM::FilterABM_lh()

  # update local community
  lc <- lc %>%
    select(species, trait, mass, patch) %>%
    FilterABM::FilterABM_lc()

  # # # # # # # # # # # # # #
  # #                       #
  # # V E R S I O N     2   #
  # #                       #
  # # # # # # # # # # # # # #
  #
  # # check resource levels
  # lh <- lh %>%
  #   mutate(res = ifelse(res < 0, 0, res))
  #
  # # initialize output local community
  # out_lc <- FilterABM::FilterABM_lc()
  #
  # # for each patch in lh
  # new_res <- map(
  #   1:nrow(lh),
  #   function(i){
  #
  #     # declare vars
  #     # p=patch, e=env, r=res
  #     p <- lh$patch[i] ; e <- lh$env[i] ; r <- lh$res[i]
  #
  #     # isolate individuals within the patch
  #     patch_lc <- lc %>%
  #       filter(patch == p)
  #
  #     if (nrow(patch_lc) >= 1){
  #
  #       # randomize order of foraging individuals
  #       patch_lc <- patch_lc[sample(x = 1:nrow(patch_lc), size = nrow(patch_lc)), ]
  #
  #       # for each individual in patch_lc
  #       new_mass <- map(
  #         1:nrow(patch_lc),
  #         function(j){
  #
  #           # declare vars
  #           # t = trait, m = mass
  #           t <- patch_lc$trait[j] ; m <- patch_lc$mass[j]
  #
  #           # account for resource availability
  #           p_resource <- r/R # need for each cycle
  #           p_resource <- ifelse(p_resource > 1, 1, p_resource)
  #
  #           # trait vs env (niche clustering)
  #           # logistic probability based on difference between trait and env
  #           p_filter <- 1 - (1/(1 + exp(-log(abs(t - e)))))
  #
  #           # trait vs traits (niche dispersion)
  #           d <- sapply(patch_lc$trait[-j], function(x){
  #             abs(x - t)
  #           })
  #           mwd <- ifelse(length(d) == 0, Inf, mean(d)) # if nobody to compete with, next step will be 1
  #           p_compete <- (1/(1 + exp(-log(mwd)))) # low probabilities for low MWD (= there are many similar individuals)
  #
  #           # tweak the effects of niche clustering / dispersion
  #           if (dispersion < 1){
  #             p_compete <- sample(x = c(p_compete, 1), size = 1, prob = c(dispersion, 1 - dispersion))
  #             # if the effect of niche dispersion is low, then it is more likely that the competition is low
  #             # (i.e., I don't care how different is my trait from others)
  #           }
  #           if (clustering < 1){
  #             p_filter <- sample(x = c(p_filter, 1), size = 1, prob = c(clustering, 1 - clustering))
  #             # if the effect of niche filtering is low, then it is more likely that resource consumption is equal
  #             # (i.e., I don't care how different my trait is from the local environment)
  #           }
  #
  #           # is individual going to feed?
  #           feed <- runif(1) <= p_resource*(p_resource + p_compete - p_resource*p_compete)
  #           mass_increase <- feed*m*p_filter # how much more the individual gets
  #           if (mass_increase < 0 | is.na(mass_increase)){
  #             mass_increase <- 0
  #           }
  #
  #           if (nrow(patch_lc) > 0){
  #             r <<- r - mass_increase # exclude from the resource pool
  #           }
  #
  #           return(m + mass_increase)
  #
  #         }
  #       ) %>% unlist() # end for each individual
  #
  #       # update local community
  #       out_lc <<- bind_rows(
  #         out_lc,
  #         patch_lc %>%
  #           mutate(mass = new_mass)
  #       )
  #
  #     }
  #
  #     return(r)
  #
  #   }
  # ) %>% unlist() # end for each patch
  #
  # # update res in lh
  # lh <- lh %>%
  #   mutate(res = new_res)

  # # # # # # # # # # # # # #
  # #                       #
  # # V E R S I O N     1   #
  # #                       #
  # # # # # # # # # # # # # #
  #
  # # for each patch
  # for (p in unique(tlh$patch)){
  #
  #   # isolate resource level
  #   res <- tlh[tlh$patch == p,]$res %>% unlist() %>% unname()
  #
  #   # isolate environmental factor level
  #   env <- tlh[tlh$patch == p,]$env %>% unlist() %>% unname()
  #
  #   # isolate individuals within the patch
  #   patch_lc <- lc %>%
  #     filter(patch == p)
  #
  #   # randomize order of foraging individuals
  #   idx <- sample(x = 1:nrow(patch_lc), size = nrow(patch_lc))
  #
  #   # trait values within the patch community
  #   com_traits <- patch_lc$trait %>% unlist()
  #
  #   #for each individual within the patch
  #   for (i in idx){
  #
  #     # body mass of an individual
  #     bmass <- patch_lc[i, "mass"] %>% unlist() %>% unname()
  #
  #     # account for resource availability
  #     p_resource <- res/R # need for each cycle
  #     p_resource <- ifelse(p_resource > 1, 1, p_resource)
  #
  #     # account for environmental filtering
  #     t <- patch_lc[i, "trait"] %>% unlist() %>% unname() # individual trait value
  #     # logistic probability based on difference between trait and env
  #     p_filter <- 1 - (1/(1 + exp(-log(abs(t - env)))))
  #
  #     # account for interactions
  #     d <- sapply(com_traits[-i], function(x){
  #       abs(x - t)
  #     })
  #     mwd <- ifelse(length(d) == 0, Inf, mean(d)) # if nobody to compete with, next step will be 1
  #     p_compete <- (1/(1 + exp(-log(mwd)))) # low probabilities for low MWD (= there are many similar individuals)
  #
  #     # tweak the effects of niche clustering / dispersion
  #     if (dispersion < 1){
  #       p_compete <- sample(x = c(p_compete, 1), size = 1, prob = c(dispersion, 1 - dispersion))
  #       # if the effect of niche dispersion is low, then it is more likely that the competition is low
  #       # (i.e., I don't care how different is my trait from others)
  #     }
  #     if (clustering < 1){
  #       p_filter <- sample(x = c(p_filter, 1), size = 1, prob = c(clustering, 1 - clustering))
  #       # if the effect of niche filtering is low, then it is more likely that resource consumption is equal
  #       # (i.e., I don't care how different my trait is from the local environment)
  #     }
  #
  #     # is individual going to feed?
  #     feed <- runif(1) <= p_resource*(p_resource + p_compete - p_resource*p_compete)
  #     mass_increase <- feed*bmass*p_filter # how much more the individual gets
  #
  #     res <- res - mass_increase # exclude from the resource pool
  #
  #     patch_lc[i, "mass"] <- bmass + mass_increase # write updated body mass
  #
  #   }
  #
  #   out_lc <- bind_rows(
  #     out_lc,
  #     patch_lc
  #   )
  #
  #   # tlh[tlh$patch == p, "res"] <- res
  #   new_res[p] <- res
  #
  # }

  list(
    lc = lc,
    lh = lh
  )

}
