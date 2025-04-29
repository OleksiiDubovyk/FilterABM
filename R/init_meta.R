#' Initialize a metacommunity
#'
#' @description
#' Create a metacommunity "object", a tibble with rows describing different abstract hypothetical species in the species pool,
#' and columns representing species identifier, mean trait value across individuals within the species, and abundance of the species.
#' Intraspecific trait variation is set manually and passed to the output as a function parameter.
#'
#' @param M Species richness within metacommunity, positive integer
#' @param env_mean Mean environmental factor value within the extent of a metacommunity
#' @param env_sd Variation of environmental factor value within the extent of a metacommunity
#' @param cauchy Delta parameter in Cauchy function used to connect species' trait values and abundances
#' @param trait_sds Intraspecific trait variation; either a single value if same for all species, or a M-length vector
#'
#' @returns A tibble (dataframe) with the following columns:
#' \code{species} - species ID, in ascending order relative to the trait value,
#' \code{trait} - species mean trait value,
#' \code{abundance} - expected species abundance.
#'
#' @import dplyr
#' @import tibble
#' @importFrom stats rnorm
#' @importFrom stats dcauchy
#'
#' @export
#'
#' @examples init_meta()
init_meta <- function(M = 120, env_mean = 0, env_sd = 25, cauchy = 1.25, trait_sds = 0){
  #
  # `M` - species richness within metacommunity
  # `env_mean` - mean environmental factor value within extent of a metacommunity
  # `env_sd` - variation of environmental factor value within extent of a metacommunity
  # `cauchy` - delta parameter in Cauchy function used to connect species' trait values and abundance
  # `trait_sds` - intraspecific trait variation; either one value if same for all species or M-length vector
  #
  # Output: tibble (dataframe) with the following columns:
  # `species` - species ID, in ascending order relative to the trait value
  # `trait` - species mean trait value
  # `abundance` - expected species abundance
  #
  spp_trait_means <- rnorm(n = M, mean = env_mean, sd = env_sd)
  regpool_spp <- tibble(trait = spp_trait_means)
  regpool_spp <- regpool_spp %>%
    mutate(abundance = dcauchy(.data$trait, 0, cauchy))
  regpool_spp <- regpool_spp %>%
    mutate(abundance = round(.data$abundance/min(.data$abundance)))
  regpool_spp <- regpool_spp %>%
    arrange(.data$trait) %>%
    rowid_to_column(var = "species") %>%
    mutate(trait_sd = trait_sds)
  return(regpool_spp)
}
