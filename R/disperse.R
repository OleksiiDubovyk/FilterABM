#' Helper function, randomly jump values in a vector
#'
#' @description
#' For every value in a vector, randomly add or subtract one.
#' The min value in the vector can only be added one, and the max value can only be subtracted from.
#'
#' @param x Integer vector
#'
#' @returns Vector of the same length as input vector
#'
#' @export
#'
jump <- function(x){
  add <- sapply(x, function(y){
    if (y == min(x)){
      1
    }else if (y == max(x)){
      -1
    }else{
      sample(x = c(-1, 1), size = 1)
    }
  })
  x + add
}

#' Simulate patch-to-patch dispersal
#'
#' @description
#' As the local habitat consists of a finite set of discrete patches, one might allow individuals to disperse across patches.
#' This function shuffles patch IDs in the input local community \code{"FilterABM_lc"} object in such a way that it simulates expected
#' \code{ndisperse} individuals per patch to move into a random neighboring patch.
#' Note that the exact number of individuals that "choose" to disperse is drawn from a Poisson distribution, where \code{ndisperse} serves as a parameter.
#'
#' @param lc A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc}).
#'
#' @param lh A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lh}).
#'
#' @param dispersal Non-negative numeric, dispersal rate per habitat patch, i.e., expectation of the number of individuals per patch that disperse to a neighboring patch.
#'
#' @returns A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame" (see \code{?FilterABM::FilterABM_lc}).
#'
#' @importFrom stats rpois
#'
#' @export
#'
#' @examples
#' mc = init_meta()
#' lh = init_envt()
#' lc = draw_lcom(mc = mc, lh = lh)
#' lc = recruit(lc = lc, mc = mc, lh = lh)
#' lc = adv_age(lc = lc)
#' lc = dem(lc = lc, mc = mc)
#' disperse(lc = lc, lh = lh, dispersal = 10)
disperse <- function(lc, lh, dispersal = 1){

  if (!is.numeric(dispersal)){
    stop(
      paste0("The `dispersal` parameter in `disperse` must be numeric, however, ", typeof(dispersal), " found."),
      call. = FALSE
    )
  }

  if (dispersal < 0){
    warning(
      "The `dispersal` parameter in `disperse` is negative, set to zero."
    )
    dispersal <- 0
  }

  npatch <- length(unique(lh$patch))
  ndisperse <- rpois(1, lambda = dispersal*npatch)
  ndisperse <- min(nrow(lc), ndisperse)

  if (ndisperse == 0){

    lc

  }else{

    dispersed_idx <- sample(x = 1:nrow(lc), size = ndisperse, replace = FALSE)
    dispersed <- lc[dispersed_idx,]
    stayed <- lc[-dispersed_idx,]

    dispersed <- dispersed %>%
      mutate(patch = as.integer(FilterABM::jump(.data$patch)))

    bind_rows(
      stayed,
      dispersed
    )

  }

}
