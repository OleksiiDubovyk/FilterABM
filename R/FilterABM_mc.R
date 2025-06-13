#' Metacommunity validator
#'
#' @description
#' Validate a metacommunity (\code{"FilterABM_mc"}) object.
#'
#' The \code{"FilterABM_mc"} class is based on \code{dplyr}'s tibbles, thus \code{dplyr} functions are applicable.
#'
#' The metacommunity object must have the following columns:
#' \code{species} - integer, species ID, in ascending order relative to the trait value,
#' \code{trait} - double, species mean trait value,
#' \code{abundance} - integer, expected species abundance,
#' \code{trait_sd} - non-negative double, intraspecific trait variation.
#'
#' @param x A valid metacommunity object of class "FilterABM_mc"/"tbl_df"/"tbl"/"data.frame"
#'
#' @returns A metacommunity object of class "FilterABM_mc"/"tbl_df"/"tbl"/"data.frame".
#'
#' @import dplyr
#' @importFrom stats rnorm
#' @importFrom stats rpois
#'
#' @export
#'
#' @examples
#' x = dplyr::tibble(species = 1:10, trait = rnorm(10), abundance = rpois(10, 10), trait_sd = 0)
#' validate_FilterABM_mc(x)
#'
validate_FilterABM_mc <- function(x){

  if (any(is.na(x))){
    stop("NA values are not allowed in any column in `FilterABM_mc` tibble. Check your input for NAs.\nDid you pass data with wrong columns to `FilterABM_mc`?",
         call. = FALSE)
  }

  if (ncol(x) != 4){
    stop(
      paste0("Exactly 4 columns expected in `FilterABM_mc` tibble, but ", ncol(x), " found"),
      call. = FALSE
    )
  }

  if (!all(c("species", "trait", "abundance", "trait_sd") %in% colnames(x))){
    stop(
      paste0("Column names in `FilterABM_mc` tibble must be: `species`, `trait`, `abundance`, `trait_sd`. ",
             paste(colnames(x), sep = " "), " found."),
      call. = FALSE
    )
  }

  vals <- unclass(x)

  if (!all(sapply(vals, is.numeric))){
    stop(
      "All columns in `FilterABM_mc` tibble must be numeric. Check variable types.",
      call. = FALSE
    )
  }

  if (!is.integer(vals$species)){
    warning(
      paste0("`species` in `FilterABM_mc` tibble must be integer, not ", typeof(vals$species), ". Rounding up values."),
      call. = FALSE
    )
    x$species <- as.integer(x$species)
    vals <- unclass(x)
  }

  if (length(vals$species) != length(unique(vals$species))){
    reps <- unique(vals$species[duplicated(vals$species)])
    stop(
      paste0("Non-unique `species` found in `FilterABM_mc` tibble with value(s) ", paste(reps))
    )
  }

  if (!is.integer(vals$abundance)){
    warning(
      paste0("`abundance` in `FilterABM_mc` tibble is recommended to be an integer, not ", typeof(vals$abundance)),
      call. = FALSE
    )
    x$abundance <- as.integer(x$abundance)
  }

  if (any(vals$abundance < 1)){
    warning("At least one value of `abundance` in `FilterABM_mc` tibble is not a positive integer. Such values changed to 1.")
    x$abundance[x$abundance < 1] <- 1
  }

  if (any(vals$trait_sd < 0)){
    stop("At least one value of `trait_sd` in `FilterABM_mc` tibble is negative. Trait variation cannot be negative.")
  }

  x

}

#' Build a metacommunity object
#'
#' @description
#' Build a metacommunity (\code{"FilterABM_mc"}) object.
#'
#' The \code{"FilterABM_mc"} class is based on \code{dplyr}'s tibbles, thus \code{dplyr} functions are applicable.
#'
#' The metacommunity object must have the following columns:
#' \code{species} - integer, species ID, in ascending order relative to the trait value,
#' \code{trait} - double, species mean trait value,
#' \code{abundance} - integer, expected species abundance,
#' \code{trait_sd} - non-negative double, intraspecific trait variation.
#'
#' @param x Optional (i.e., if not supplied, will return an empty valid object) tibble or dataframe with the columns specified above
#' (unique integer \code{species}, double \code{trait}, integer positive \code{abundance}, and non-negative double \code{trait_sd}).
#' Each row represents a species in the regional species pool.
#' @param val_in Logical, default to TRUE. Is formal validation of \strong{input} necessary to check if consistent with the \code{"FilterABM_mc"} class?
#' @param val_out Logical, default to TRUE. Is formal validation of \strong{output} necessary to check if consistent with the \code{"FilterABM_mc"} class?
#'
#' @returns A metacommunity object of class "FilterABM_mc"/"tbl_df"/"tbl"/"data.frame".
#'
#' @importFrom stats rnorm
#' @importFrom stats rpois
#' @import dplyr
#'
#' @export
#'
#' @examples
#' x = dplyr::tibble(species = 1:10, trait = rnorm(10), abundance = rpois(10, 10), trait_sd = 0)
#' FilterABM_mc(x)
#'
FilterABM_mc <- function(x, val_in = TRUE, val_out = TRUE){

  y <- tibble(
    species = integer(0),
    trait = double(0),
    abundance = integer(0),
    trait_sd = double(0)
  )

  if (!missing(x)){
    if (val_in){
      x <- validate_FilterABM_mc(x)
    }
    y <- bind_rows(y, x)
  }

  y <- y %>% select(.data$species, .data$trait, .data$abundance, .data$trait_sd)

  y <- structure(y, class = c("FilterABM_mc", class(y)))

  if (val_out){
    y <- FilterABM::validate_FilterABM_mc(y)
  }

  y
}

#' Summarize a metacommunity object
#'
#' @param object A valid metacommunity (\code{"FilterABM_mc"}) object.
#' @param ... Arguments passed to or from other methods.
#'
#' @exportS3Method base::summary
#'
summary.FilterABM_mc <- function(object, ...){
  m <- nrow(object)
  cat(paste0("A metacommunity of ", m, " species."))
}

#' Plot a metacommunity object
#'
#' @param x A valid metacommunity (\code{"FilterABM_mc"}) object.
#' @param y Parameter two.
#' @param ... Arguments passed to or from other methods.
#'
#' @import graphics
#' @importFrom stats density
#'
#' @exportS3Method base::plot
#'
plot.FilterABM_mc <- function(x, y, ...){
  graphics.off()
  object <- x
  par(mfrow = c(2, 2))
  # Rank-abundance  (Whittaker) plot
  plot(x = sort(object$abundance, decreasing = TRUE), log = "y", pch = 16, xlab = "Species", ylab = "log-Abundance", main = "Rank-abundance SAD plot")
  # log Preston plot
  log(object$abundance, 10) %>% density() %>% plot(xaxt = "n", xlab = "log-Species abundance", main = "Preston SAD plot")
  max_labun <- max(log(object$abundance, 10)) %>% round()
  min_labun <- min(log(object$abundance, 10)) %>% round()
  axis(
    side = 1,
    at = seq(from = min_labun, to = max_labun, by = 1),
    labels = lapply(
      seq(from = min_labun, to = max_labun, by = 1),
      function(x) str2lang(paste0("10^", x))
    ) %>% as.expression()
  )
  polygon(log(object$abundance, 10) %>% density(), col = "blue")
  # TAD plot
  trait_density <- (
    rep(x = object$trait, times = object$abundance) +
      mapply(
        FUN = function(a, b) rnorm(n = a, mean = 0, sd = b),
        object$abundance, object$trait_sd
      ) %>% unlist()
  )%>%
    density()
  # trait_density <- density(x = object$trait, weights = object$abundance) # won't work, no variation for weights
  plot(trait_density, main = "Trait abundance distribution", xlab = "Trait value", ylab = "Dens. of ind.")
  polygon(trait_density, col = "purple")
  # Traits by species
  trait_bysp <- density(object$trait)
  plot(trait_bysp, main = "Traits across species", xlab = "Trait value", ylab = "Dens. of species")
  polygon(trait_bysp, col = "lightblue")
}
