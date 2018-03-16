#' This script generates a collection of common applied filter packs.
#'
#'
#'

# General gene filter pack
intensity_filter      <- function(x) { gene_filter_intensity(x, cutoff = 50) }
variance_filter       <- function(x) { gene_filter_quantile(x, cutoff = 200)}
prevalence_filter     <- function(x) { gene_filter_prevalence(x)}

filter_pack1          <- list(intensity_filter,variance_filter,prevalence_filter)
