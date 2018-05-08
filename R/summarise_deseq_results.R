#' Summarise deseq2 results
#'
#' This function makes the same type of summary output as the \code{\link{summary}} function from \pkg{DESeq2}.
#' The difference being that this function will also work on data.frame objects, especially handy when we combine
#' DESeqResults objects together in a long-format data.frame. The code was largely copied from \code{\link{summary}}
#' with some minor adjustments.
#'
#' The \code{\link{summary}} function from DESeq2 will only work with DESeqResults objects in R
#'
#' @param dds.analysis Data.frame of DESeq2 results, with the same kind of format as the output of \code{\link{results}}.
#' @param model.index If not \code{NULL} will only print combined summary of 1 model
#'
#' @return The output is a printed summary of the results
#' @export
#'
#'
SummariseModelResults <- function(dds.analysis, model.index = NULL) {
  if (!is.null(model.index)) {
    model.contrasts <- dds.analysis$Results[[model.index]]
    model.comparisons <- dds.analysis$Comparisons$contrasts[[model.index]]
    meta <- dds.analysis$meta[[model.index]]
  } else stop("Not implemented yet for all models at once")

  df.results <- CombineModelContrasts(model.contrasts, model.comparisons)

  cat("\n")
  # Determine values for summary
  notallzero <- sum(df.results$baseMean > 0)
  up <- sum(df.results$padj < meta$alpha & df.results$log2FoldChange >
              0, na.rm = TRUE)
  down <- sum(df.results$padj < meta$alpha & df.results$log2FoldChange <
                0, na.rm = TRUE)
  filt <- sum(!is.na(df.results$pvalue) & is.na(df.results$padj))
  outlier <- sum(df.results$baseMean > 0 & is.na(df.results$pvalue))
  ft <- floor(meta$filterThreshold)

  # False until we allow IHW
  ihw <- FALSE

  # Print summary of results
  printsig <- function(x) format(x, digits = 2)
  cat("out of", notallzero, "with nonzero total read count\n")
  cat(paste0("adjusted p-value < ", meta$alpha, "\n"))
  cat(paste0("LFC > 0 (up)     : ", up, ", ", printsig(up/notallzero *
                                                         100), "% \n"))
  cat(paste0("LFC < 0 (down)   : ", down, ", ", printsig(down/notallzero *
                                                           100), "% \n"))
  cat(paste0("outliers [1]     : ", outlier, ", ", printsig(outlier/notallzero *
                                                              100), "% \n"))
  if (!ihw)
    cat(paste0("low counts [2]   : ", filt, ", ", printsig(filt/notallzero *
                                                             100), "% \n"))
  if (!ihw)
    cat(paste0("(mean count < ", ft, ")\n"))
  cat("[1] see 'cooksCutoff' argument of ?results\n")
  if (!ihw)
    cat("[2] see 'independentFiltering' argument of ?results\n")
  if (ihw)
    cat("[2] see metadata(res)$ihwResult on hypothesis weighting\n")
  cat("\n")
}

#' Title
#'
#' @param model.contrasts
#' @param model.comparisons
#'
#' @return
#' @export
#'
#' @examples
CombineModelContrasts <- function(model.contrasts, model.comparisons) {
  require(dplyr)
  results.nrows <- sapply(model.contrasts, nrow)
  df.results <- do.call(rbind, lapply(model.contrasts, as.data.frame)) %>%
    mutate(Contrast = rep(names(model.contrasts), times = results.nrows)) %>%
    mutate(Name = row.names(.)) %>%
    select(Contrast, everything())
  return(df.results)
}
