#' Simulate demographic processes within the local community
#'
#' @description
#' Take in the tibble representing the local metacommunity and return what it would look like after two demographic processes are applied:
#' (1) mortality, i.e., elimination of all individuals whose body mass is insufficient to survive, and
#' (2) reproduction, at which all individuals whose body mass exceeds their reproductive body mass asexually reproduce into two new individuals
#' with body masses equal to half of that in parent individual and trait values equal to parent's trait value plus random error,
#' as dictated by the metacommunity description.
#'
#' @param lc A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc}).
#'
#' @param mc A metacommunity object of class "FilterABM_mc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_mc}).
#'
#' @param reproduction Non-negative numeric; mass at which individuals reproduce.
#'
#' @param expenditure Positive numeric; amount of body mass each individual needs to spend in order to survive one time step.
#'
#' @returns A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame".
#'
#' @import dplyr
#' @importFrom purrr map
#' @importFrom purrr pmap_dbl
#'
#' @export
#'
#' @examples
#' mc1 = init_meta()
#' lh1 = init_envt()
#' lc1 = draw_lcom(mc = mc1, lh = lh1)
#' lc1 = recruit(lc = lc1, mc = mc1, lh = lh1)
#' dem(lc = lc1, mc = mc1)
#'
dem <- function(lc, mc, reproduction = 10, expenditure = 0.1){

  if (reproduction < 0){
    stop("`reproduction` in `dem()` must be non-negative.")
  }

  if (expenditure < 0){
    stop("`expenditure` in `dem()` must be positive.")
  }

  lc <- lc %>%
    mutate(mass = mass - expenditure) %>% # expend body mass
    filter(mass > 0) # eliminate starving individuals

  dues <- lc %>%
    filter(mass >= reproduction) # individuals that reproduce

  if (nrow(dues) > 0){

    lc <- lc %>%
      filter(mass < reproduction) # individuals that do not reproduce

    newborn_masses <- purrr::map(
      dues$mass, function(x) runif(n = 1, min = 0, max = x) %>% round(digits = 5)
    ) %>% unlist()

    lcmc <- left_join(
      dues,
      mc %>%
        mutate(trait_mean = trait) %>%
        select(species, trait_mean, trait_sd),
      by = "species"
    ) %>%
      select(trait, trait_mean, trait_sd)

    lc <- bind_rows(
      # non-reproducing individuals
      lc,
      # old individuals after loosing mass to their offspring
      dues %>%
        mutate(mass = mass - newborn_masses),
      # newborn individuals with new traits
      dues %>%
        mutate(mass = newborn_masses,
               trait = purrr::pmap_dbl(
                 lcmc,
                 function(trait, trait_mean, trait_sd){
                   (trait + trait_mean)/2 + rnorm(n = 1, mean = 0, sd = trait_sd)
                 }
               )
        )
    )


    # tsds <- left_join(
    #   dues, mc,
    #   by = "species"
    # )$trait_sd
    #
    # lc <- bind_rows(
    #   # non-reproducing individuals
    #   lc,
    #   # old individuals after loosing mass to their offspring
    #   dues %>%
    #     mutate(mass = mass - newborn_masses),
    #   # newborn individuals with new traits
    #   dues %>%
    #     mutate(mass = newborn_masses,
    #            trait = purrr::map2_dbl(trait, tsds,
    #                             function(t, tv) t + rnorm(n = 1, mean = 0, sd = tv)
    #            )
    #     )
    # )

  }

  # double-check for sneaky rounding errors
  lc <- lc %>%
    filter(mass > 0)

  return(FilterABM::FilterABM_lc(lc))

}
