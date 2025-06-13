#' Local community validator
#'
#' @description
#' Validate a local community (\code{"FilterABM_lc"}) object.
#'
#' The \code{"FilterABM_lc"} class is based on \code{dplyr}'s tibbles, thus \code{dplyr} functions are applicable.
#'
#' The local community object must have the following columns:
#' \code{species} - integer for species ID,
#' \code{trait} - double for individual trait value,
#' \code{age} - integer for the current individual age in time steps,
#' \code{mass} - double for the current individual body mass,
#' \code{lifespan} - double for maximum individual age,
#' \code{repmass} - double for a critical body mass at which the individual reproduces,
#' \code{patch} - integer for the patch ID in which individual currently resides.
#'
#' @param x A valid local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame"
#'
#' @returns A valid local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame"
#'
#' @export
#'
validate_FilterABM_lc <- function(x){

  if (any(is.na(x))){
    stop("NA values are not allowed in any column in `FilterABM_lc` tibble. Check your input for NAs.\nDid you pass data with wrong columns to `FilterABM_lc`?",
         call. = FALSE)
  }

  if (ncol(x) != 7){
    stop(
      paste0("Exactly 7 columns expected in `FilterABM_lc` tibble, but ", ncol(x), " found"),
      call. = FALSE
    )
  }

  if (!all(c("species", "trait", "age", "mass", "lifespan", "repmass", "patch") %in% colnames(x))){
    stop(
      paste0("Column names in `FilterABM_lc` tibble must be: `species`, `trait`, `age`, `mass`, `lifespan`, `repmass`, `patch`. Instead, ",
             paste(colnames(x), sep = " "), " found."),
      call. = FALSE
    )
  }

  vals <- unclass(x)

  if (!all(sapply(vals, is.numeric))){
    stop(
      "All columns in `FilterABM_lc` tibble must be numeric. Check variable types.",
      call. = FALSE
    )
  }

  if (!is.integer(vals$species)){
    warning(
      paste0("`species` in `FilterABM_lc` tibble must be integer, not ", typeof(vals$species), ". Rounding up values."),
      call. = FALSE
    )
    x$species <- as.integer(x$species)
    vals <- unclass(x)
  }

  if (!is.integer(vals$patch)){
    warning(
      paste0("`patch` in `FilterABM_lc` tibble must be integer, not ", typeof(vals$species), ". Rounding up values."),
      call. = FALSE
    )
    x$patch <- as.integer(x$patch)
    vals <- unclass(x)
  }

  if (any(vals$age < 0)){
    stop(
      "At least one value of `age` in `FilterABM_lc` is negative.",
      call. = FALSE
    )
  }

  if (any(vals$mass < 0)){
    stop(
      "At least one value of `mass` in `FilterABM_lc` is negative.",
      call. = FALSE
    )
  }

  if (any(vals$lifespan < 0)){
    stop(
      "At least one value of `lifespan` in `FilterABM_lc` is negative.",
      call. = FALSE
    )
  }

  if (any(vals$repmass < 0)){
    stop(
      "At least one value of `repmass` in `FilterABM_lc` is negative.",
      call. = FALSE
    )
  }

  x

}


#' Build a local community object
#'
#' Validate a local community (\code{"FilterABM_lc"}) object.
#'
#' The \code{"FilterABM_lc"} class is based on \code{dplyr}'s tibbles, thus \code{dplyr} functions are applicable.
#'
#' The local community object must have the following columns:
#' \code{species} - integer for species ID,
#' \code{trait} - double for individual trait value,
#' \code{age} - integer for the current individual age in time steps,
#' \code{mass} - double for the current individual body mass,
#' \code{lifespan} - double for maximum individual age,
#' \code{repmass} - double for a critical body mass at which the individual reproduces,
#' \code{patch} - integer for the patch ID in which individual currently resides.
#'
#' @param x Optional (i.e., if not supplied, will return an empty valid object) tibble or dataframe with the columns specified above
#' (integer \code{species}, double \code{trait}, integer \code{age}, double \code{mass}, double \code{lifespan}, double \code{repmass}, integer \code{patch}).
#' Each row represents an individual in the local community.
#' @param val_in Logical, default to TRUE. Is formal validation of \strong{input} necessary to check if consistent with the \code{"FilterABM_lc"} class?
#' @param val_out Logical, default to TRUE. Is formal validation of \strong{output} necessary to check if consistent with the \code{"FilterABM_lc"} class?
#'
#' @returns A local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame".
#'
#' @export
#'
#' @examples
#' FilterABM_lc()
#'
FilterABM_lc <- function(x, val_in = TRUE, val_out = TRUE){

  y <- tibble(
    species = integer(0),
    trait = double(0),
    age = integer(0),
    mass = double(0),
    lifespan = double(0),
    repmass = double(0),
    patch = integer(0)
  )

  if (!missing(x)){
    if (val_in){
      x <- FilterABM::validate_FilterABM_lc(x)
    }
    y <- bind_rows(y, x)
  }

  y <- y %>% select(.data$species, .data$trait, .data$age, .data$mass, .data$lifespan, .data$repmass, .data$patch)

  y <- structure(y, class = c("FilterABM_lc", class(y)))

  if (val_out){
    y <- FilterABM::validate_FilterABM_lc(y)
  }

  y

}

#' Summarize a local community object
#'
#' @param object A valid local community (\code{"FilterABM_lc"}) object.
#' @param ... Arguments passed to or from other methods.
#'
#' @exportS3Method base::summary
#'
summary.FilterABM_lc <- function(object, ...){
  if (nrow(object) > 1){
    kde <- density(x = object$trait)
  }else{
    kde <- density(x = object$trait, bw = 1)
  }
  list(
    nind = nrow(object),
    species_richness = length(unique(object$species)),
    mean_trait = mean(object$trait),
    trait_distribution = kde
  )
}

#' Plot a local community object
#'
#' @param x A valid local habitat (\code{"FilterABM_lc"}) object.
#' @param y Parameter two.
#' @param ... Arguments passed to or from other methods.
#'
#' @import ggplot2
#'
#' @exportS3Method base::plot
#'
plot.FilterABM_lc <- function(x, y, ...){
  graphics.off()

  S <- unique(x$species)
  n <- nrow(x)

  # trait density vs species
    ggplot2::ggplot() +
    ggplot2::geom_density(data = x %>%
                   group_by(x$species) %>%
                   filter(n() > 1) %>%
                   ungroup(),
                 mapping = aes(x = .data$trait, ggplot2::after_stat(count), fill = factor(.data$species)),
                 position = "stack", show.legend = F, na.rm = F) +
    ggplot2::geom_point(
      data = x %>%
        group_by(.data$species) %>%
        filter(n() == 1) %>%
        ungroup(),
      mapping = aes(x = .data$trait, y = -1, color = factor(.data$species)),
      show.legend = F
    ) +
    ggplot2::xlab("Trait value") + ggplot2::ylab("Individuals") +
    ggplot2::labs(title = paste0(S, " species, ", n, " individuals"))

}
