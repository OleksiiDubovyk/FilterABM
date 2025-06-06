% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FilterABM_lc.R
\name{validate_FilterABM_lc}
\alias{validate_FilterABM_lc}
\title{Local community validator}
\usage{
validate_FilterABM_lc(x)
}
\arguments{
\item{x}{A valid local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame"}
}
\value{
A valid local community object of class "FilterABM_lc"/"tbl_df"/"tbl"/"data.frame"
}
\description{
Validate a local community (\code{"FilterABM_lc"}) object.

The \code{"FilterABM_lc"} class is based on \code{dplyr}'s tibbles, thus \code{dplyr} functions are applicable.

The local community object must have the following columns:
\code{species} - integer for species ID,
\code{trait} - double for individual trait value,
\code{age} - integer for the current individual age in time steps,
\code{mass} - double for the current individual body mass,
\code{lifespan} - double for maximum individual age,
\code{repmass} - double for a critical body mass at which the individual reproduces,
\code{patch} - integer for the patch ID in which individual currently resides.
}
