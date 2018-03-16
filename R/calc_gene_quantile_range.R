calc_gene_quantite_range <- function(x,gene_are_rows=T)
{
  apply(x, ifelse(gene_are_rows,1,2),FUN=function(x){
    genQuantRange <- range(quantile(x))

    max(genQuantRange)-min(genQuantRange)
  })

}
