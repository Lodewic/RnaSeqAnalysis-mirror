#' gene_filter_prevalence
#'
#' Function for filtering genes. Given a gene array function returns TRUE if gene counts
#' occur less than the cutoff values for at least as the threshold percentage of samples.
#'
#' @return
#' @export
#' 
gene_filter_prevalence <- function(x, cutoff=5, threshold=0.05)
{
  sum(x > cutoff) > threshold*length(x)
}
