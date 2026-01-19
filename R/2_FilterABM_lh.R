#' Local habitat validator
#'
#' @description
#' Validate a local habitat (\code{"FilterABM_lh"}) object.
#'
#' The \code{"FilterABM_lh"} class is based on \code{dplyr}'s tibbles, thus \code{dplyr} functions are applicable.
#'
#' The local habitat object must have the following columns:
#' \code{patch} - unique integer, patch ID;
#' \code{env} - double, patch-specific level of the environmental factor;
#' \code{res} - non-negative double, level of the resource available in the patch.
#'
#' @param x A valid local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame"
#'
#' @returns A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame".
#'
#' @import dplyr
#' @importFrom stats rnorm
#'
#' @export
#'
#' @examples
#' x = dplyr::tibble(patch = 1:10, env = rnorm(10), res = 1000)
#' validate_FilterABM_lh(x)
#'
validate_FilterABM_lh <- function(x){

  if (any(is.na(x))){
    stop("NA values are not allowed in any column in `FilterABM_lh` tibble. Check your input for NAs.\nDid you pass data with wrong columns to `FilterABM_lh`?",
         call. = FALSE)
  }

  if (ncol(x) != 3){
    stop(
      paste0("Exactly 3 columns expected in `FilterABM_lh` tibble, but ", ncol(x), " found"),
      call. = FALSE
    )
  }

  if (!all(c("patch", "env", "res") %in% colnames(x))){
    stop(
      paste0("Column names in `FilterABM_lh` tibble must be: `patch`, `env`, `res`. ",
             paste(colnames(x), sep = " "), " found."),
      call. = FALSE
    )
  }

  vals <- unclass(x)

  if (!all(sapply(vals, is.numeric))){
    stop(
      "All columns in `FilterABM_lh` tibble must be numeric. Check variable types.",
      call. = FALSE
    )
  }

  if (!is.integer(vals$patch)){
    warning(
      paste0("`patch` in `FilterABM_lh` tibble must be integer, not ", typeof(vals$species), ". Rounding up values."),
      call. = FALSE
    )
    x$patch <- as.integer(x$patch)
    vals <- unclass(x)
  }

  if (length(vals$patch) != length(unique(vals$patch))){
    reps <- unique(vals$patch[duplicated(vals$patch)])
    stop(
      paste0("Non-unique `patch` found in `FilterABM_lh` tibble with value(s) ", paste(reps))
    )
  }

  if (!is.numeric(vals$env)){
    stop(
      "Well, somehow, `env` in `FilterABM_lh` is not numeric.",
      call. = FALSE
    )
  }

  x

}

#' Build a local habitat object
#'
#' @description
#'
#' Build a local habitat (\code{"FilterABM_lh"}) object.
#'
#' The \code{"FilterABM_lh"} class is based on \code{dplyr}'s tibbles, thus \code{dplyr} functions are applicable.
#'
#' The local habitat object must have the following columns:
#' \code{patch} - unique integer, patch ID;
#' \code{env} - double, patch-specific level of the environmental factor;
#' \code{res} - non-negative double, level of the resource available in the patch.
#' Resource is rather an ephemeral variable because it is the only variable outside of the local community that is mutable.
#' The most efficient way to implement this fact seemed to force changes in this variable directly from functions while the \code{"FilterABM_lh"} is
#' in the global environment (see the source code for \code{forage}).
#'
#' @param x Optional (i.e., if not supplied, will return an empty valid object) tibble or dataframe with the columns specified above
#' (unique integer \code{patch}, double \code{env}).
#' Each row represents a discrete habitat patch within the local habitat with its own level of the environmental factor.
#' @param val_in Logical, default to TRUE. Is formal validation of \strong{input} necessary to check if consistent with the \code{"FilterABM_lh"} class?
#' @param val_out Logical, default to TRUE. Is formal validation of \strong{output} necessary to check if consistent with the \code{"FilterABM_lh"} class?
#' @param gradient Can be NULL. Character, the rule by which patches get their values of env, either:
#' \code{gradient = "random"} - default value, env is an independent random variable drawn from N(env_mean, env_sd),
#' \code{gradient = "linear"} - env changes linearly from patch number 1 to patch number \code{npatch} with min and max drawn from 95% bound of N(env_mean, env_sd),
#' \code{gradient = "correlated"} - environmental factor is correlated with patch number with correlation coefficient equal \code{rho}, or
#' \code{gradient = "clustered"} - for K clusters, there are linearly distributed local env_means and small env_sd.
#'
#' @returns A local habitat object of class "FilterABM_lh"/"tbl_df"/"tbl"/"data.frame".
#'
#' @export
#'
#' @examples
#' x = dplyr::tibble(patch = 1:10, env = rnorm(10), res = 1000)
#' FilterABM_lh(x)
#'
FilterABM_lh <- function(x, val_in = TRUE, val_out = TRUE, gradient = NULL){

  y <- tibble(
    patch = integer(0),
    env = double(0),
    res = double(0)
  )

  if (!missing(x)){
    if (val_in){
      x <- validate_FilterABM_lh(x)
    }
    y <- bind_rows(y, x)
  }

  y <- y %>% select(patch, env, res)

  y <- structure(y, class = c("FilterABM_lh", class(y)), gradient = gradient)

  if (val_out){
    y <- FilterABM::validate_FilterABM_lh(y)
  }

  y

}

#' Summarize a local habitat object
#'
#' @param object A valid local habitat (\code{"FilterABM_lh"}) object.
#' @param ... Arguments passed to or from other methods.
#'
#' @exportS3Method base::summary
#'
summary.FilterABM_lh <- function(object, ...){
  m <- nrow(object)
  g <- attributes(object)$gradient
  if (is.null(g)){
    cat(paste0("A local habitat with ", m, " patches, gradient not defined."))
  }else{
    cat(paste0("A local habitat with ", m, " patches, ", g, " gradient."))
  }
}

#' Plot a local habitat object
#'
#' @param x A valid local habitat (\code{"FilterABM_lh"}) object.
#' @param y Parameter two.
#' @param ... Arguments passed to or from other methods.
#'
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom stats density
#'
#' @exportS3Method base::plot
#'
plot.FilterABM_lh <- function(x, y, ...){

  p1 <- x %>%
    ggplot(aes(x = patch, y = env, color = env)) +
    geom_point() +
    scale_color_gradient(low = "lightgreen", high = "#003a00") +
    theme_minimal() +
    xlab("Patch") +
    ylab("Environmental factor") +
    theme(legend.position = "none")

  p2 <- x %>%
    ggplot() +
    geom_vline(aes(xintercept = patch, color = env), linewidth = 200/nrow(x)) +
    scale_color_gradient(low = "lightgreen", high = "#003a00") +
    theme_minimal() +
    xlab("Patch") +
    theme(legend.position = "none")

  gridExtra::grid.arrange(p1, p2, nrow = 2, heights = c(3, 1))

}
