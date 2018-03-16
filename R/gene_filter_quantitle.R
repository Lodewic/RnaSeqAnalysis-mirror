gene_filter_quantile <- function(x, cutoff)
{
  genrange <- range(quantile(x))

  max(genrange)-min(genrange) > cutoff
}
