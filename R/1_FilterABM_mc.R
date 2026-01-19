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

  y <- y %>% select(species, trait, abundance, trait_sd)

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
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom stats density
#'
#' @exportS3Method base::plot
#'
plot.FilterABM_mc <- function(x, y, ...){

  p1 <- tibble(
    rank = 1:nrow(x),
    abundance = sort(x$abundance, decreasing = TRUE)
  ) %>%
    ggplot(aes(x = rank, y = abundance)) +
    geom_line(lwd = 2) +
    scale_y_log10() +
    theme_minimal() +
    xlab("Rank") +
    ylab("log-Abundance") +
    labs(title = "Rank-abundance SAD plot")

  p2 <- tibble(
    logabun = x$abundance
  ) %>%
    ggplot(aes(x = logabun)) +
    geom_density(fill = "blue") +
    scale_x_log10() +
    theme_minimal() +
    xlab("log-Species abundance") +
    ylab("Density") +
    labs(title = "Preston SAD plot")

  p3 <- x %>%
    ggplot(aes(x = trait)) +
    geom_density(fill = "purple", aes(weight = abundance), bw = 1/nrow(x)) +
    theme_minimal() +
    xlab("Trait value") +
    ylab("Individuals") +
    labs(title = "Trait abundance distribution")

  p4 <- x %>%
    ggplot(aes(x = trait)) +
    geom_density(fill = "lightblue") +
    theme_minimal() +
    xlab("Trait value") +
    ylab("Species") +
    labs(title = "Trait across species")

  gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)
}
