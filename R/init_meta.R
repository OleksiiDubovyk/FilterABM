#' Initialize a metacommunity
#'
#' @description
#' Create a metacommunity (\code{"FilterABM_mc"}) object, a tibble with rows describing different abstract hypothetical species in the species pool,
#' and columns representing species identifier, mean trait value across individuals within the species, and abundance of the species.
#' Intraspecific trait variation is set manually and passed to the output as a function parameter.
#'
#' @param M Integer, species richness within metacommunity, positive integer
#' @param env_mean Numeric, mean environmental factor value within the extent of a metacommunity
#' @param env_sd Non-negative numeric, variation of environmental factor value within the extent of a metacommunity
#' @param cauchy Non-negative numeric, delta parameter in Cauchy function used to connect species' trait values and log-abundances
#' @param trait_sds Non-negative numeric, intraspecific trait variation; either a single value if same for all species, or a M-length vector
#' @param max Numeric, the maximum abundance allowed for the most abundant species in the metacommunity.
#'
#' @returns A metacommunity \code{"FilterABM_mc"}-class tibble with the following columns:
#' \code{species} - integer, species ID, in ascending order relative to the trait value,
#' \code{trait} - double, species mean trait value,
#' \code{abundance} - integer, expected species abundance,
#' \code{trait_sd} - non-negative double, intraspecific trait variation.
#'
#' @import dplyr
#' @import tibble
#' @importFrom stats rnorm
#' @importFrom stats dcauchy
#'
#' @export
#'
#' @examples
#' x = init_meta()
#' class(x)
#'
init_meta <- function(M = 120, env_mean = 0, env_sd = 1, cauchy = 1, trait_sds = 0, max = 1e6){
  # logarithms of expected max abundance
  lmax <- log(max, 10)
  # generate mean trait values for species
  spp_trait_means <- rnorm(n = M, mean = env_mean, sd = env_sd)
  # map abundance logarithms through Cauchy function
  map_cauchy <- dcauchy(x = spp_trait_means, location = 0, scale = cauchy)
  # normalize abundance logarithms
  map_cauchy <- map_cauchy * (lmax / max(map_cauchy))
  # calculate abundances
  abundances <- round(10^map_cauchy)
  # initialize metacommunity tibble
  regpool_spp <- tibble(trait = spp_trait_means,
                        abundance = abundances)
  regpool_spp <- regpool_spp %>%
    arrange(.data$trait) %>%
    rowid_to_column(var = "species") %>%
    mutate(trait_sd = trait_sds) %>%
    mutate(species = as.integer(.data$species),
           abundance = as.integer(.data$abundance))
  FilterABM_mc(regpool_spp)
}
