#' Simulate a discrete time step
#'
#' @description
#' Simulate changes in the local community through a single time step.
#'
#' There are multiple processes involved in simulated changes in local community:
#' (1) agents are recruited from the metacommunity with \code{recruit()},
#' (2) age of all agents increases by one step and all agents whose age is greater than their maximum lifespan are eliminated with \code{dem()},
#' (3) all agents whose body mass is greater than individual reproductive body mass reproduce with \code{dem()},
#' (4) agents disperse to neighboring habitat patches at a set \code{dispersal} rate,
#' (5) agents consume resources with \code{forage()}.
#'
#' An individual’s ability to consume the resource within a time step will depend on
#' (1) whether an individual’s trait value matches a cell’s environmental level as a proxy of how adapted to the local environment the individual is, and
#' (2) whether other individuals in the cell have similar trait values, thus inducing competition for the resource.
#'
#' This function summarizes all actions commited by individuals into shaping the local community at a time step.
#'
#' @param com A tibble (dataframe) representing the \strong{local} community with the following columns:
#' \code{species} - species ID;
#' \code{trait} - individual trait value;
#' \code{age} - current age of the individual;
#' \code{mass} - current body mass of the individual;
#' \code{lifespan} - maximum lifespan after reaching which the individual will perish;
#' \code{repmass} - body mass after reaching which the individual will asexually reproduce into two children individuals.
#'
#' @param metacom A tibble (dataframe) with the following columns:
#' \code{species} - species ID, in ascending order relative to the trait value,
#' \code{trait} - species mean trait value,
#' \code{abundance} - expected species abundance.
#' Output of \code{init_meta()}.
#'
#' @param envt A tibble (dataframe) representing habitat patches (column \code{patch}) and the patch-specific value of the environmental factor (column \code{env}).
#' Output of \code{init_envt()}.
#'
#' @param age_crit Numeric; critical age at which half of the individuals die.
#'
#' @param mass_crit Numeric; critical mass at which half of the individuals reproduce.
#'
#' @param res available resource level, a dataframe with two columns:
#   `$patch` - patch number
#   `$res` - resource level
#'
#' @param res_input Numeric, increment of the resource level within a time step.
#'
#' @param R Numeric, resource level at which all individuals within a community successfully consume the resource (i.e., probability of resource consumption equals one).
#'
#' @param recruitment Numeric, expected number of individuals to be recruited from the metacommunity into one patch with \code{recruit()}.
#' Default to one.
#'
#' @param dispersal Numeric, expected number of individuals to disperse into a different patch from their patch. Default to one.
#'
#' @returns A list with the following elements:
#' \code{[[i]]} - habitat patch number with a list of the following:
#' \code{[[i]][["com"]]} - updated community object,
#' \code{[[i]][["res"]]} - updated resource value,
#' \code{[[i]][["res_consumed"]]} - sum of consumed resource,
#' \code{[[i]][["p_compete"]]} - sum of foraging probabilities given competition,
#' \code{[[i]][["p_consume"]]} - sum of foraging probabilities given resource availability.
#'
#' @import dplyr
#' @importFrom stats rpois
#'
#' @export
#'
#' @examples
#' x = init_meta()
#' y = draw_lcom(x, 100)
#' y$patch = sample(1:10, 100, replace = TRUE)
#' z = init_envt()
#' res0 <- dplyr::tibble(patch = 1:10, res = 1000)
#' time_step(com = y, metacom = x, envt = z, res = res0)
#'
time_step <- function(com, metacom, envt, age_crit = 10, mass_crit = 5, res = 1000, res_input = 10, R = 1000, recruitment = 1, dispersal = 1){
  #
  # `com` - local community object with the following columns:
  #   `$patch` - patch number
  #   `$species` - species ID
  #   `$trait` - individual trait value
  #   `$age` - age of the individual
  #   `$mass` - body mass
  #   `$lifespan` - maximum lifespan
  #   `$repmass` - mass when reproduction occurs
  # `metacom` - tibble (dataframe) with the following columns:
  #   `$species` - species ID, in ascending order relative to the trait value
  #   `$trait` - species mean trait value
  #   `$abundance` - expected species abundance
  #   `$trait_sd` - intraspecific trait variation
  # `envt` - environment object, a tibble with the following columns:
  #   `$patch` - patch number
  #   `$env` - environmental factor
  # `age_crit` - critical age at which half of the individuals die
  # `mass_crit` - critical mass at which half of the individuals reproduce
  # `res` - available resource level, a dataframe with two columns:
  #   `$patch` - patch number
  #   `$res` - resource level
  # `res_input` - increase in resource level per time step
  # `R` - resource level at which (or greater than which) all individuals consume resource (probability = 1)
  # `recruitment` - expected number of individuals to disperse from the metacommunity into one patch
  # `dispersal` - expected number of individuals to disperse into a different patch from their patch
  #
  # Output: a list of
    # [[i]] - patch number with a list of
    #   [["com"]] - updated community object
    #   [["res"]] - updated resource value
    #   [["res_consumed"]] - sum of consumed resource
    #   [["p_compete"]] - sum of foraging probabilities given competition
    #   [["p_consume"]] - sum of foraging probabilities given resource availability
  #

  npatch <- nrow(envt)

  out <- vector(mode = "list", length = npatch)

  # check if the community exists

  if (nrow(com) == 0){
    for (i in sample(1:npatch, npatch, replace = F)){
      com_p <- com[0,]
      env <- envt %>%
        filter(.data$patch == i)
      env <- env$env %>% unlist() %>% unname()
      resrc <- res %>%
        filter(.data$patch == i)
      resrc <- resrc$res %>% unlist() %>% unname()
      out[[i]] <- list(
        com = com_p,
        res = resrc,
        res_consumed = 0,
        p_compete = 0,
        p_consume = 0
      )
    }
  } else {

    # recruitment

    npatch <- envt$patch %>% unique() %>% length()

    new_inds <- draw_lcom(
      metacom = metacom,
      nind = rpois(n = 1, lambda = recruitment*npatch), # number of individuals drawn is a Poisson process
      age_crit = age_crit,
      mass_crit = mass_crit)
    new_inds$patch <- sample(1:npatch, nrow(new_inds), replace = T) # random distribution of newly recruited inds into patches

    com <- bind_rows(
      com,
      new_inds
    )

    # advance age
    com <- com %>%
      mutate(age = .data$age + 1)

    # demographic processes
    com <- dem(
      com = com,
      metacom = metacom,
      age_crit = age_crit,
      mass_crit = mass_crit
    )

    # between-patches dispersal
    ndisperse <- rpois(1, lambda = dispersal*npatch)
    ndisperse <- min(nrow(com), ndisperse)
    if (ndisperse <= 0){
      com <- com
    }else{
      dispersed_idx <- sample(1:nrow(com), ndisperse, replace = F)
      dispersed <- com[dispersed_idx,]
      stayed <- com[-dispersed_idx,]
      for (i in 1:nrow(dispersed)){
        p <- dispersed[i, "patch"] %>% unlist()
        if (p == 1){
          dispersed[i, "patch"] <- p + 1
        }else if (p == npatch){
          dispersed[i, "patch"] <- p - 1
        }else{
          dispersed[i, "patch"] <- p + sample(c(-1, 1), 1)
        }
      }
      com <- bind_rows(dispersed, stayed)
    }

    # feeding
    for (i in sample(1:npatch, npatch, replace = F)){
      com_p <- com %>%
        filter(.data$patch == i)
      env <- envt %>%
        filter(.data$patch == i)
      env <- env$env %>% unlist() %>% unname()
      resrc <- res %>%
        filter(.data$patch == i)
      resrc <- resrc$res %>% unlist() %>% unname()
      if (nrow(com_p) == 0){
        out[[i]] <- list(
          com = com_p,
          res = resrc + res_input,
          res_consumed = 0,
          p_compete = 0,
          p_consume = 0
        )
      }else{
        out[[i]] <- forage(
          com = com_p,
          env = env,
          res = resrc,
          res_input = res_input,
          R = R
        )
      }
    }

  }

  return(out)

}
