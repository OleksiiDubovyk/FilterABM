#' Simulate demographic processes within the local community
#'
#' @description
#' Take in the tibble representing the local metacommunity and return what it would look like after two demographic processes are applied:
#' (1) mortality, i.e., elimination of all individuals that exceed their maximum lifespan, and
#' (2) reproduction, at which all individuals whose body mass exceeds their reproductive body mass asexually reproduce into two new individuals
#' with body masses equal to half of that in parent individual and trait values equal to parent's trait value plus random error,
#' as dictated by the metacommunity description.
#'
#' Note that age of individuals is not increased by this function, this is a responsibility of the \code{time_step()}.
#'
#'
#' @param com A tibble (dataframe) representing the \strong{local} community with the following columns:
#' \code{patch} - identifier of habitat patch;
#' \code{species} - species ID;
#' \code{trait} - individual trait value;
#' \code{age} - current age of the individual;
#' \code{mass} - current body mass of the individual;
#' \code{lifespan} - maximum lifespan after reaching which the individual will perish;
#' \code{repmass} - body mass after reaching which the individual will asexually reproduce into two children individuals.
#'
#' @param metacom A tibble (dataframe) representing the \strong{metacommunity} with the following columns:
#' \code{species} - species ID, in ascending order relative to the trait value,
#' \code{trait} - species mean trait value,
#' \code{abundance} - expected species abundance,
#' \code{trait_sd} - intraspecific trait variation.
#'
#' @param age_crit Numeric; critical age at which half of the individuals die.
#'
#' @param mass_crit Numeric; critical mass at which half of the individuals reproduce.
#'
#' @returns A tibble (dataframe) representing the **local community** with the following columns:
#' \code{patch} - identifier of habitat patch;
#' \code{species} - species ID;
#' \code{trait} - individual trait value;
#' \code{age} - current age of the individual;
#' \code{mass} - current body mass of the individual;
#' \code{lifespan} - maximum lifespan after reaching which the individual will perish;
#' \code{repmass} - body mass after reaching which the individual will asexually reproduce into two children individuals.
#'
#' @import dplyr
#'
#' @export
#'
#' @examples
#' x = init_meta()
#' y = dplyr::mutate(draw_lcom(x, 100), patch = 1)
#' dem(com = y, metacom = x)
#'
dem <- function(com, metacom, age_crit = 10, mass_crit = 5){
  #
  # `com` - local community object with the following columns:
  #   `$patch` - patch number
  #   `$species` - species ID
  #   `$trait` - individual trait value
  #   `$age` - age of the individual
  #   `$mass` - body mass
  #   `$lifespan` - maximum lifespan
  #   `$repmass` - mass when reproduction occurs
  # `metacom` - metacommunity object with the following columns:
  #   `$species` - species ID, in ascending order relative to the trait value
  #   `$trait` - species mean trait value
  #   `$abundance` - expected species abundance
  #   `$trait_sd` - intraspecific trait variation
  # `age_crit` - critical age at which half of the individuals die
  # `mass_crit` - critical mass at which half of the individuals reproduce
  #
  com <- com %>%
    filter(.data$age <= .data$lifespan) # mortality
  dues <- com %>%
    filter(.data$mass > .data$repmass) # individuals that reproduce
  com <- com %>%
    filter(.data$mass <= .data$repmass) # individuals that do not reproduce
  if (nrow(dues) > 0){
    for (i in 1:nrow(dues)){
      p <- dues[i, "patch"] %>% unlist()
      w <- dues[i, "mass"] %>% unlist()
      sp <- dues[i, "species"] %>% unlist()
      t <- dues[i, "trait"] %>% unlist()
      tsd <- metacom %>%
        filter(.data$species == sp)
      tsd <- tsd$trait_sd %>% unlist()
      # tm <- metacom %>%
      #   filter(species == sp) %>%
      #   .$trait %>% unlist()
      # new_traits <- tm + rnorm(2, 0, tsd)
      new_traits <- t + rnorm(2, 0, tsd)
      com <- bind_rows(com,
                       tibble(
                         species = sp,
                         trait = new_traits,
                         age = 1,
                         mass = w/2,
                         # lifespan = pred_logit(p = runif(2, 0.1, 0.9), crit = age_crit) %>% round(),
                         lifespan = rexp(2, 1/age_crit) %>% round(),
                         repmass = pred_logit(p = runif(2, 0.1, 0.9), crit = mass_crit),
                         patch = p
                       ))
    }
  }
  com
}
