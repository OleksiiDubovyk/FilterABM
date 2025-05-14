#' Helper function, logit
#'
#' @description
#' Logit-transformation of an input variable.
#'
#' @param p A random value between 0 and 1
#' @param crit A critical value corresponding to p = 0.5
#'
#' @returns A random value following the logit function
#' @export
#'
#' @examples
#' pred_logit(runif(1), crit = 0.5)
pred_logit <- function(p, crit){
  #
  # `p` - random value between 0 and 1
  # `crit` - critical value corresponding to p = 0.5
  #
  # Output:
  # random value following the logit function
  #
  if (max(p) > 1 | min(p) < 0){
    stop("in pred_logit(...) p is not 0 < p < 1")
  }else{
    (p*crit)/(1-p)
  }
}

#' Draw individual(s) from the metacommunity
#'
#' @description
#' Provided a metacommunity object (class \code{"FilterABM_mc")}) and the local habitat (\code{"FilterABM_lh"}),
#' returns a tibble with the number of rows representing recruited individuals.
#' Rows in the output tibble represent individuals recruited from the metacommunity (nrows equal to \code{nind}).
#' Columns represent the species ID (arbitrary identifier), the trait value, age of the individual, its body mass, maximum lifespan, and reproductive body mass,
#' as well as the local habitat patch occupied by an individual.
#'
#' The major difference from \code{recruit()} is that \code{draw_meta()} returns a set of individuals drawn from the metacommunity,
#' while \code{recruit()} mutates the local community adding new individuals.
#'
#' @param mc A metacommunity object of class "FilterABM_mc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_mc}).
#'
#' @param lh A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lh}).
#'
#' @param nind Positive integer, a number of individuals to draw from the metacommunity.
#'
#' @param age_crit Numeric; critical age at which half of the individuals die.
#'
#' @param mass_crit Numeric; critical mass at which half of the individuals reproduce.
#'
#' @returns An object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc}).
#'
#' @importFrom stats rexp
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' mc = init_meta()
#' lh = init_envt()
#' draw_lcom(mc = mc, lh = lh)
#'
draw_lcom <- function(mc, lh, nind = 1, age_crit = 10, mass_crit = 5){

  if (nind < 1){
    stop(
      paste0("`draw_lcom` must draw a positive number of individuals, however, `nind` was set to ", nind),
      call. = FALSE
    )
  }

  if (!is.integer(nind)){
    nind <- as.integer(nind)
  }

  if (is.na(nind)){
    stop("Make sure that `nind` in `draw_lcom` is an integer.")
  }

  lucky <- mc[sample(1:nrow(mc), size = nind, replace = T, prob = mc$abundance), ]

  new_trait <- sapply(1:nrow(lucky), function(i){
    lucky$trait[i] + rnorm(1, 0, lucky$trait_sd[i])
  })

  lucky <- lucky %>%
    mutate(
      trait = new_trait,
      age = 1,
      mass = 1,
      lifespan = rexp(nind, 1/age_crit) %>% round(),
      repmass = FilterABM::pred_logit(p = runif(nind, 0.1, 0.9), crit = mass_crit),
      patch = sample(x = unique(lh$patch), size = nind, replace = TRUE)
    )

  lc <- FilterABM::FilterABM_lc(x = lucky, val_in = FALSE, val_out = TRUE)

  lc

}

#' Add new individuals from the metacommunity into the local community
#'
#' @description
#' Provided a metacommunity object (class \code{"FilterABM_mc")}) and the local habitat (\code{"FilterABM_lh"}),
#' as well as a prior local community (\code{"FilterABM_lc"}),
#' add the newly drawn individuals into the metacommunity.
#' Rows in the output tibble represent individuals recruited from the metacommunity (nrows equal to \code{nind}).
#' If the species is described as one having no intraspecific trait variation (e.g., \code{trait_sd = 0}),
#' then the trait of a recruited individual will be equal to the species-specific trait value (e.g., \code{trait}), otherwise
#' a small random error will be added drawn from a normal distribution with SD equal to \code{trait_sd}.
#'
#'
#' @param lc A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc})
#'
#' @param mc A metacommunity object of class "FilterABM_mc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_mc}).
#'
#' @param lh A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lh}).
#'
#' @param recruitment Non-negative double, recruitment rate (i.e., expectation of number of individuals recruited into local habitat per patch per time step).
#'
#' @param ... Other parameters applicable to \code{draw_lcom()}.
#'
#' @returns An object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame".
#'
#' @importFrom stats rpois
#'
#' @export
#'
#' @examples
#' mc = init_meta()
#' lh = init_envt()
#' lc = draw_lcom(mc = mc, lh = lh)
#' recruit(lc = lc, mc = mc, lh = lh)
#'
recruit <- function(lc, mc, lh, recruitment = 0, ...){

  nind <- rpois(n = 1, lambda = recruitment*nrow(lh))

  if (nind == 0){

    return(lc)

  }else{

    return(
      bind_rows(
        lc,
        FilterABM::draw_lcom(mc = mc, lh = lh, nind = nind, ...)
      )
    )

  }
}
