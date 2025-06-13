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
#' lh = init_envt()
#' lh_input_res(lh = lh, res_input = 10)
#'
lh_input_res <- function(lh, res_input = 0){
  lh %>%
    mutate(res = .data$res + res_input,
           res = ifelse(.data$res < 0, 0, .data$res))
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
#' @param R Numeric, resource level at which all individuals within a community successfully consume the resource (i.e., probability of resource consumption equals one).
#'
#' @param clustering Numeric, effect of niche clustering on probability of competition (between 0 and 1). Default to one.
#'
#' @param dispersion Numeric, effect of niche dispersion on probability of trait filtering by the environment (between 0 and 1). Default to one.
#'
#' @returns A named list with:
#' \code{[["lc"]]} - A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc});
#' \code{[["lh"]]} - A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lh}).
#'
#' @export
#'
#' @examples
#' mc = init_meta()
#' lh = init_envt()
#' lc = draw_lcom(mc = mc, lh = lh, nind = 100)
#' lc = recruit(lc = lc, mc = mc, lh = lh, nind = 100)
#' lc = adv_age(lc = lc)
#' lc = dem(lc = lc, mc = mc)
#' lc = disperse(lc = lc, lh = lh, dispersal = 10)
#' forage(lc = lc, lh = lh, R = 1000)
#'
forage <- function(lc, lh, R, clustering = 1, dispersion = 1){

  # # it was but a sorry attempt to change lh in the parent environment from the function
  # if (!exists(deparse(substitute(lh)))){
  #   stop(
  #     paste0("`lh` must be defined in R's global environment for `forage` to function properly.\nPlease supply the object (i.e., define `lh <- init_envt()` prior to calling `forage(..., lh = lh, ...)`). Instead, `",
  #            deparse(substitute(lh)), "` supplied.")
  #   )
  # }

  # formal checks
  if (any(c(clustering, dispersion) > 1)){
    stop("in `forage()`, clustering and dispersion parameters cannot be greater than 1")
  }
  if (any(c(clustering, dispersion) < 0)){
    stop("in `forage()`, clustering and dispersion parameters cannot be lower than 0")
  }

  # temporary local habitat
  # add resource input and make sure the resource is not negative
  tlh <- lh

  # initialize output local community
  out_lc <- FilterABM::FilterABM_lc()

  # for each patch
  for (p in unique(tlh$patch)){

    # isolate resource level
    res <- tlh[tlh$patch == p,]$res %>% unlist() %>% unname()

    # isolate environmental factor level
    env <- tlh[tlh$patch == p,]$env %>% unlist() %>% unname()

    # isolate individuals within the patch
    patch_lc <- lc %>%
      filter(.data$patch == p)

    # randomize order of foraging individuals
    idx <- sample(x = 1:nrow(patch_lc), size = nrow(patch_lc))

    # trait values within the patch community
    com_traits <- patch_lc$trait %>% unlist()

    #for each individual within the patch
    for (i in idx){

      # body mass of an individual
      bmass <- patch_lc[i, "mass"] %>% unlist() %>% unname()

      # account for resource availability
      p_resource <- res/R # need for each cycle
      p_resource <- ifelse(p_resource > 1, 1, p_resource)

      # account for environmental filtering
      t <- patch_lc[i, "trait"] %>% unlist() %>% unname() # individual trait value
      # logistic probability based on difference between trait and env
      p_filter <- 1 - (1/(1 + exp(-log(abs(t - env)))))

      # account for interactions
      d <- sapply(com_traits[-i], function(x){
        abs(x - t)
      })
      mwd <- ifelse(length(d) == 0, Inf, mean(d)) # if nobody to compete with, next step will be 1
      p_compete <- (1/(1 + exp(-log(mwd)))) # low probabilities for low MWD (= there are many similar individuals)

      # tweak the effects of niche clustering / dispersion
      if (dispersion < 1){
        p_compete <- sample(x = c(p_compete, 1), size = 1, prob = c(dispersion, 1 - dispersion))
        # if the effect of niche dispersion is low, then it is more likely that the competition is low
        # (i.e., I don't care how different is my trait from others)
      }
      if (clustering < 1){
        p_filter <- sample(x = c(p_filter, 1), size = 1, prob = c(clustering, 1 - clustering))
        # if the effect of niche filtering is low, then it is more likely that resource consumption is equal
        # (i.e., I don't care how different my trait is from the local environment)
      }

      # is individual going to feed?
      feed <- runif(1) <= p_resource*(p_resource + p_compete - p_resource*p_compete)
      mass_increase <- feed*bmass*p_filter # how much more the individual gets

      res <- res - mass_increase # exclude from the resource pool

      patch_lc[i, "mass"] <- bmass + mass_increase # write updated body mass

    }

    out_lc <- bind_rows(
      out_lc,
      patch_lc
    )

    tlh[tlh$patch == p, "res"] <- res

  }

  # # it was but a sorry attempt to change lh in the parent environment from the function
  # # update lh in R global environment
  # unlockBinding(deparse(substitute(lh)), env = parent.frame())
  # eval.parent(bquote(.(substitute(lh)) <<- .(substitute(tlh))))

  list(
    lc = FilterABM::FilterABM_lc(x = out_lc, val_in = FALSE),
    lh = tlh
  )

}
