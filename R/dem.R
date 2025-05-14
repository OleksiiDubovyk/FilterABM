#' Advance age of all individuals in the local community
#'
#' @param lc An object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc}).
#'
#' @returns A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc}).
#'
#' @export
#'
#' @examples
#' mc = init_meta()
#' lh = init_envt()
#' lc = draw_lcom(mc = mc, lh = lh)
#' lc = recruit(lc = lc, mc = mc, lh = lh)
#' adv_age(lc)
#'
adv_age <- function(lc){
  lc %>%
    mutate(age = .data$age + 1)
}

#' Simulate demographic processes within the local community
#'
#' @description
#' Take in the tibble representing the local metacommunity and return what it would look like after two demographic processes are applied:
#' (1) mortality, i.e., elimination of all individuals that exceed their maximum lifespan, and
#' (2) reproduction, at which all individuals whose body mass exceeds their reproductive body mass asexually reproduce into two new individuals
#' with body masses equal to half of that in parent individual and trait values equal to parent's trait value plus random error,
#' as dictated by the metacommunity description.
#'
#' Note that age of individuals is not increased by this function, this is a responsibility of the \code{adv_age()}.
#'
#' @param lc A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc}).
#'
#' @param mc A metacommunity object of class "FilterABM_mc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_mc}).
#'
#' @param age_crit Numeric; critical age at which half of the individuals die.
#'
#' @param mass_crit Numeric; critical mass at which half of the individuals reproduce.
#'
#' @returns A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc}).
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' mc = init_meta()
#' lh = init_envt()
#' lc = draw_lcom(mc = mc, lh = lh)
#' lc = recruit(lc = lc, mc = mc, lh = lh)
#' lc = adv_age(lc = lc)
#' dem(lc = lc, mc = mc)
#'
dem <- function(lc, mc, age_crit = 10, mass_crit = 5){

  lc <- lc %>%
    filter(.data$age <= .data$lifespan) # mortality
  dues <- lc %>%
    filter(.data$mass > .data$repmass) # individuals that reproduce
  lc <- lc %>%
    filter(.data$mass <= .data$repmass) # individuals that do not reproduce

  if (nrow(dues) > 0){

    p <- dues$patch
    w <- dues$mass
    sp <- dues$species
    t <- dues$trait
    tsd <- mc[sapply(sp, function(i) which(mc$species == i)), ]$trait_sd

    p <- rep(x = p, each = 2)
    w <- rep(x = w/2, each = 2)
    sp <- rep(x = sp, each = 2)
    t <- lapply(1:nrow(dues), function(i) t[i] + rnorm(n = 2, mean = 0, sd = tsd[i])) %>% unlist()

    newborn <- tibble(
      species = sp,
      trait = t,
      age = 1,
      mass = 1,
      lifespan = rexp(n = length(t), rate = 1/age_crit) %>% round(),
      repmass = FilterABM::pred_logit(p = runif(n = length(t), 0.1, 0.9), crit = mass_crit),
      patch = p
    )

    lc <- bind_rows(
      lc,
      FilterABM::FilterABM_lc(newborn, val_in = FALSE, val_out = TRUE)
    )

  }

  lc

}
