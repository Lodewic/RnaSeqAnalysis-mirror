gene_filter_intensity <- function(x, cutoff)
{
  sum(x) > cutoff
}
