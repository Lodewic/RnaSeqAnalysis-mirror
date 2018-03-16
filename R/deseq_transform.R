#' deseq_transform
#'
#' Perform deseq2 transformations such as rlog and vst. Returns a data.frame contaned the transformed counts.
#'
#' @param dds A DESeqDataSet object.
#' @return A data.frame containing the transformed counts.
#'
deseq_transform <- function(dds,
                            blind=TRUE,
                            method='scale'){
 require(DESeq2)

  dds                   <- estimateSizeFactors(dds)
  dds                   <- estimateDispersions(dds)

  # transform data
  countTable <- switch(method,
                      'vst'   = assay(varianceStabilizingTransformation(dds,blind)),
                      'rlog'  = assay(rlog(dds,blind)),
                      'scale' = counts(dds,normalized=TRUE),
                       stop('Unknown method'))



  return(countTable)
}
