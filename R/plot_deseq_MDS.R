#' Glimma MDS plot from DESeq2 variance-stabilized counts
#'
#' @param vsd Variance stabilized counts. Recommended to put in mcols(dds)$vst
#' @param groups.vec Group names to color by. should be column names of colData(dds)
#' @param folder Output folder of .html output, either relative or absolute
#' @param main Title of MDS plot
#' @param html Output filename of .html file
#' @param launch Whether to launch a browser to view the Glimma MDS plot
#'
#' @details
#' The \code{vsd} is expected to be DESeqTransform object, output from \code{\link{VarianceStabilizingTransformation}}.
#' 
#' @return
#' 
#' @export
#' 

plot_deseq_mds <- function(vsd, groups.vec,
                            folder="Glimma",
                            main=NULL,
                            html = 'MDS-Plot',
                            launch=TRUE) {
  if (is.null(main)) main=paste("MDS-Plot")
  require(Glimma)
  require(DESeq2)
  
  mds.groups <- colData(vsd)[,groups.vec]
  # TODO: Outlier logical to colData?
  glMDSPlot(assay(vsd), labels=colnames(vsd), groups=mds.groups,
            folder=folder, main=main, html=html, launch=launch)
  
  return(file.path(folder, paste0(html, ".html")))
}

