#' Initialize a metacommunity
#'
#' @description
#' Create a metacommunity (\code{"FilterABM_mc"}) object, a tibble with rows describing different abstract hypothetical species in the species pool,
#' and columns representing species identifier, mean trait value across individuals within the species, and abundance of the species.
#' Intraspecific trait variation is set manually and passed to the output as a function parameter.
#'
#' The regional pool (in this context, the terms "metacommunity" and "regional pool" are used interchangeably) is initialized with the following steps:
#'
#' (1) species-specific mean trait values are drawn from a normal distribution with a mean \code{env_mean_mc} and variance \code{env_sd_mc};
#'
#' (2) the relationship between species' trait value and abundance is mapped through a Cauchy function following Koffel et al. 2022, where the function has the location parameter equal to zero and
#' the shape parameter equal to the set value of \code{cauchy};
#'
#' (3) generated species-abundance distribution is rounded and standardized by \code{max_abun} so that the generated species-abundance distribution does not exceed the maximum set abundance.
#'
#' @param M Integer, species richness within metacommunity, positive integer.
#' @param env_mean_mc Numeric, mean environmental factor value within the extent of a metacommunity.
#' @param env_sd_mc Non-negative numeric, variation of environmental factor value within the extent of a metacommunity.
#' @param cauchy Non-negative numeric, delta parameter in Cauchy function used to connect species' trait values and log-abundances.
#' @param trait_sds Non-negative numeric, intraspecific trait variation; either a single value if same for all species, or a M-length vector.
#' @param max_abun Numeric, the maximum abundance allowed for the most abundant species in the metacommunity.
#'
#' @returns A metacommunity \code{"FilterABM_mc"}-class tibble (see \code{?FilterABM::FilterABM_mc}).
#'
#' @import dplyr
#' @import tibble
#' @importFrom stats rnorm
#' @importFrom stats dcauchy
#'
#' @export
#'
#' @references Koffel, T., K. Umemura, E. Litchman, and C. A. Klausmeier. 2022. A general framework for species‐abundance distributions: linking traits and dispersal to explain commonness and rarity. Ecology Letters 25:2359–2371.
#'
#' @examples
#' x = init_meta()
#' class(x)
#'
init_meta <- function(M = 120, env_mean_mc = 0, env_sd_mc = 1, cauchy = 1, trait_sds = 0, max_abun = 1e6){
  env_mean <- env_mean_mc
  env_sd <- env_sd_mc
  # logarithms of expected max abundance
  lmax <- log(max_abun, 10)
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
  FilterABM::FilterABM_mc(regpool_spp)
}
