#' deseq_gene_filter
#'
#' A helper function for removing genes according to various criteria.
#'
#' @param dds A DESeqDataset object, easily made using \code{\link{DESeqDataSetFromMatrix()}}
#'
#' @param flist        A list of filter functions to be applied to the array.
#'                     Each function should return either TRUE or FALSE for every gene. Genes for which
#'                     the function returns FALSE are removed.
#' @value returns a DESeqDataSet with all the same fields as the input data, except some rows may be filtered.
#' @export
#'
#' 
deseq_gene_filter <- function(dds, flist, normalized=FALSE)
{
  require(genefilter)

  # Read data files
  countTable <- counts(dds, normalized)
  
  # Apply filters
  filters        <- filterfun(flist)
  gfilter        <- genefilter(countTable, filters)
  
  # Filter the DESeqDataSet (dds)
  #   There is also metadata in the mcols(dds)$annotation and mcols(dds)$vst elements.
  #   These are row data and as such they get filtered by row as well as soon as we filter the dds object
  dds <- dds[gfilter,]

  return(dds)

}
