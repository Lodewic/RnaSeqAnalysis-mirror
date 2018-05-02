#' Determine WGCNA soft threshold with DESeqDataSet
#'
#' Attempt to determine soft-threshold for WGCNA similarity measures. Effectively we raise the absolute correlations between gene expressions to a power,
#' and we aim to determine the minimal power required to achieve an approximately scale-free network topology.
#'
#' @param datExpr A matrix or data.frame with samples in rows and gene expressions in columns. Recommended to use variance-stabilized counts
#' @param subsets A list of integer vectors to determine different subsets. 
#'     Preferably a named list where names indicate a short name to differentiate subsets. Default is NULL, in which case we use all data.
#' @param powers Numeric vector of powers (soft-thresholds) to test
#' @param ... Additional arguments passed to \code{\link{pickSoftThreshold}} (TODO)
#'
#' @details 
#' Calls the function \code{\link{pickSoftThreshold}} from the \pkg{WGCNA} package using biweight midcorrelation (bicor) and a signed network by default.
#' 
#' You may use this function especially for differential network analysis, where one needs to compute weighted networks based on subsets of the data.
#' This function may help to determine the best soft-threshold, which needs to be determined for both subsets individually.
#'
#' @return A two-element list. First the \code{powerEstimates}, the determined powers for each subset. 
#'      Second the \code{fitIndices}, the combined individual data.frame outputs from \code{\link{pickSoftThreshold}}.
#' @export
#'
WGCNA_ThresholdSubsets <- function(datExpr, subsets = NULL, powers = c(1:10, seq(12, 30, 2)), ...) {
  require(WGCNA)
  # Currently hard-coded arguments to pickSoftThreshold
  # TODO:
  #   Use dots argument to make the call to pickSoftThreshold
  
  # Use all samples if subsets is left NULL (defaults to simply applying pickSoftThreshold() to all samples).
  if (is.null(subsets)) subsets = list(AllSamples = 1:nrow(datExpr))
  
  # Make sure we use a list to loop over - not a single vector
  #   Assuming that if the input is NOT a list (or NULL) then the input must be a vector. Otherwise we'll run into an error anyway.
  if (!class(subsets) == "list") subsets = list(subsets)
  
  # EXPECT A LIST OF VECTORS FOR SUBSETS
  # A named list preferably, otherwise just name the list elements numerically (redundant but may be improved later)
  if (is.null(names(subsets))) names(subsets) <- 1:length(subsets)
  
  # Initialize list
  results.list <- list()
  # Loop over subsets
  for (i in 1:length(subsets)) {
    # Determine soft threshold using subset of the data
    subsetExpr <- datExpr[subsets[[i]],]
    results.list[[names(subsets)[i]]] <- pickSoftThreshold(subsetExpr, 
                                                           RsquaredCut = 0.85,
                                                           powerVector = powers,
                                                           verbose = 5,
                                                           corFnc = "bicor",
                                                           corOptions = list(use = "p"),
                                                           blockSize = ncol(subsetExpr),
                                                           networkType = "signed hybrid")
    # Save results to a list and compute the SignedR2 for plotting.
    #   Each list element is a data.frame - we add the subset name to this data.frame 
    results.list[[names(subsets)[i]]]$fitIndices <- results.list[[names(subsets)[i]]]$fitIndices %>%
      mutate(SignedR2 = -sign(slope)*SFT.R.sq,
             Subset = names(subsets)[i])
  }

  # Return a two-element list
  #   powerEstimates: determined minimal powers to reach a certain RSquaredCut
  #   fitIndices: the fitIndices from the invidual procedures, combined by rbind()
  return(list(powerEstimates = lapply(results.list, `[[`, "powerEstimate"),
              fitIndices = do.call(rbind, lapply(results.list, `[[`, "fitIndices"))))
}


#' Plot soft-threshold estimations from subsets
#'
#' Uses the output from \code{\link{WGCNA_ThresholdSubsets}} and plots the results in an interactive plot of the signed R-squared and the mean network connectivity.
#'
#' @param fitIndices A data.frame of fitIndices including a column \code{SignedR2} and \code{Subset}, as output from \code{\link{WGCNA_ThresholdSubsets}}
#' @param powerEstimates A named list of powerEstimates for all subsets.
#' @param RsquaredCut Cutoff of the minimal r-squared needed to accept soft-threshold power.
#'
#' @return A plotly subplot of the signed R^2 and mean network connectivity versus the soft-threshold.
#' @export
#'
#' @examples
WGCNA_PlotThresholds <- function(fitIndices, powerEstimates, RsquaredCut = 0.85) {
  require(plotly)
  require(ggplot2)
  gg.R2 <- ggplot(fitIndices, aes(x = Power, y = SignedR2)) + 
    geom_text(aes(label = Power, color = Subset)) + ylab("Signed R^2") +
    geom_hline(yintercept = RsquaredCut, color = "red", alpha = 0.8)
  
  gg.Conn <- ggplot(fitIndices, aes(x = Power, y = mean.k.)) + 
    geom_text(aes(label = Power, color = Subset)) + ylab("Mean connectivity")
  
  plotly.subplot <- subplot(gg.R2, gg.Conn) %>% layout(title = "Signed R^2 and Mean connectivity")
  return(plotly.subplot)
}



#' BlockWise Modules of subsets
#'
#' @param datExpr 
#' @param subsets 
#' @param power 
#' @param include.total 
#' @param ... passed to blockwiseModules()
#'
#' @return
#' @export
#'
#' @examples
WGCNA_SubsetModules <- function(datExpr, subsets, power, include.total = TRUE, ...) {
  # CAREFUL NOT TO FEED A datExpr matrix THAT IS TOO LARGE
  #   Some heavy computation is done in this function that scales badly with the number of genes
  
  # Use all samples if subsets is left NULL (defaults to simply applying pickSoftThreshold() to all samples).
  # if (is.null(subsets)) subsets = list(AllSamples = 1:nrow(datExpr))
  if (include.total & is.null(subsets$AllSamples)) subsets = c(subsets, list(AllSamples = 1:nrow(datExpr)))
  
  # Make sure we use a list to loop over - not a single vector
  #   Assuming that if the input is NOT a list (or NULL) then the input must be a vector. Otherwise we'll run into an error anyway.
  if (!class(subsets) == "list") subsets = list(subsets)
  
  # EXPECT A LIST OF VECTORS FOR SUBSETS
  # A named list preferably, otherwise just name the list elements numerically (redundant but may be improved later)
  if (is.null(names(subsets))) names(subsets) <- 1:length(subsets)
  # Compute blockWiseModules for every subsets
  # Initialize list
  NetworkList <- list()
  # Loop over subsets
  for (i in 1:length(subsets)) {
    # Determine soft threshold using subset of the data
    subsetExpr <- datExpr[subsets[[i]], ]
    NetworkList.tmp <- blockwiseModules(subsetExpr,
                                        power = power,
                                        minModuleSize = 50,
                                        networkType = "signed",
                                        corType = "bicor",
                                        nThreads = 6,
                                        pamStage = FALSE,
                                        loadTOM = TRUE,
                                        numericLabels = TRUE,
                                        pamRespectsDendro = FALSE,
                                        saveTOMFileBase = names(subsets)[i],
                                        saveTOMs = TRUE,
                                        maxBlockSize = ncol(subsetExpr),
                                        verbose = 5, robustY = FALSE)
    
    # Extend network list elements for plotting
    NetworkList.tmp$moduleLabels = NetworkList.tmp$colors
    NetworkList.tmp$moduleColors = labels2colors(NetworkList.tmp$colors)
    NetworkList.tmp$colorOrder = c("grey", standardColors(50))
    NetworkList.tmp$moduleLabels = match(NetworkList.tmp$moduleColors, NetworkList.tmp$colorOrder)-1
    NetworkList.tmp$MEs = NetworkList.tmp$MEs
    NetworkList.tmp$geneTree = NetworkList.tmp$dendrograms[[1]]
    NetworkList.tmp$mergedColors = labels2colors(NetworkList.tmp$colors)
    
  NetworkList[[names(subsets)[i]]] <- NetworkList.tmp
  }
  
  return(NetworkList )
}
