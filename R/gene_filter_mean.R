#' Filter genes by mean expression
#'
#' To be used as an argument to \code{\link{genefilter}} from the \pkg{genefilter} package.
#' 
#' @param x 
#' @param cutoff 
#'
#' @return
#' @export
gene_filter_mean <- function(x, cutoff) 
{
  mean(x) > cutoff
}