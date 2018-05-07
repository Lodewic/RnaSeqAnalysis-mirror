#' Title
#'
#' @param dds 
#' @param comparisonFile 
#' @param annotationData 
#'
#' @return
#' @export
#'
#' @examples
DESeqAnalysis <- function(dds, comparisonFile, annotationData = NULL) {
  # Read and Parse comparisonfile
  comparisonData <- ReadComparisons(comparisonfile)
  comparisons <- ParseComparisons(comparisonData)
  designs <- comparisons$designs         # List of designs
  contrasts <- comparisons$contrasts     # List of comparisons per design
  n.designs <- length(designs)
  
  # Estimate the DESeq model for every design
  #   List of deseq models
  dds.models <- lapply(designs, EstimateDESeqModel, dds = dds)
  
  # Get individual contrasts per model in a list
  #   The p-value adjustment is done per contrast
  dds.contrasts <- lapply(1:n.designs, function(model.ind) {
    GetDESeqContrasts(dds.models[[model.ind]],
                      contrasts = contrasts[[model.ind]],
                      model.index = model.ind)
  })
  
  ########################
  ### RE-ADJUST P-VALUES WITH COMBINED CONTRASTS
  ########################
  # Apply pvalue adjustment to every top-level list element of dds.contrasts
  #   named as model.contrasts internally
  #   for each model individually.
  adjusted.contrasts <- lapply(dds.contrasts, AdjustDESeqContrasts)
  dds.contrasts <- lapply(adjusted.contrasts, function(x) x$model.contrasts)
  # Metadata of pvalueAdjustment to plot and review results from independent filtering
  # of the pvalues 
  adjusted.meta <- lapply(adjusted.contrasts, function(x) x$meta)
  
  # Return a list of models and a list of contrasts separately
  #   TODO: Optionally make named lists if models/contrasts have names
  #   These names may be given in the comparisonfile in an additional column
  return(list(Models = dds.models, Results = dds.contrasts, meta = adjusted.meta))
}

#' Title
#'
#' @param dds 
#' @param dsgn 
#'
#' @return
#' @export
#'
#' @examples
EstimateDESeqModel <- function(dds, dsgn) {
  # Note: Can't use "design" as variable as it is a DESeq2 function.
  
  # Loop over all designs and 
  design(dds) <- dsgn
  design.vars <- all.vars(design(dds))
  # Check if design is valid
  if (any(!design.vars %in% colnames(colData(dds)))) {
    stop("Model design includes invalid sample feature names!")
  }
  # In some cases the design variables may be NA for some samples
  # We automatically remove these samples
  if (length(design.vars) == 1) {
    invalid.samples <- sapply(colData(dds)[, design.vars], anyNA)
  } else invalid.samples <- apply(colData(dds)[, design.vars], 1, anyNA)
  
  # Warning if we have invalid samples
  if (any(invalid.samples)) {
    warning(sprintf("Removing %i samples with missing design feature!",
                    sum(invalid.samples)))
    dds <- dds[, !invalid.samples]
  }
  
  # Estimate Negative Binomial model of expression data with DESeq2
  #   Use BetaPrior=TRUE by default - unless there are interaction effects in the model
  #   TODO: grepl() for both ":" and "*" in the design formula.
  interaction.effects <- grepl(":", dsgn)[2]
  dds <- DESeq(dds, parallel = FALSE, 
               betaPrior = !interaction.effects)
  return(dds)
}

GetDESeqContrasts <- function(dds.model, contrasts, 
                              model.index = 1,
                              padj.alpha = 0.1) {
#' Title
#'
#' @param contrast 
#'
#' @return
#' @export
#'
#' @examples
  model.results <- lapply(contrasts, function(contrast) {
    # Get results for given model and corresponding contrasts
    contrast <- unlist(strsplit(contrast, split = " "))
    
    # If contrast is a single element then it is meant for a continuous variable
    # and should be fed the results() function as a single list element
    #   Can also be a comparison of an interaction effect- same idea.
    if ((length(contrast)) == 1) contrast <- list(contrast)
    
    # Get results for current contrast with current model
    contrast.results <- results(dds.model,
                                contrast = contrast,
                                alpha = padj.alpha)
    # Apply logfoldchange shrinkage to results if there are
    # interaction effects - because in that case betaPrior = False and
    # therefore the lfc shrinkage was not done when estimating the model.
    # if (interaction.effects) {
    #   contrast.results <- lfcShrink(deseq.models[[i]]$model,
    #                                 contrast = contrast,
    #                                 contrast.results)}
    return(contrast.results)
  })
  # Name results as 'Comp'design.contrast  (i.e. Comp1.1, Comp1.2)
  names(model.results) <- paste0("Comp", model.index,
                                 ".", 1:length(contrasts))
  return(model.results)
}

#' Title
#'
#' @param dds.contrasts 
#' @param padj.alpha 
#' @param theta 
#' @param pAdjustMethod 
#' @param returnMeta
#'
#' @return
#' @export
#'
#' @examples
AdjustDESeqContrasts <- function(model.contrasts, padj.alpha = 0.1,
                                 theta = seq(from=0, to=0.5, by=0.01),
                                 pAdjustMethod = "BH",
                                 returnMeta = TRUE) {
  require(dplyr)
  # Apply independent filtering to all contrasts at once for statistical
  # robustness
  #   Independent filtering finds the minimal expression cutoff that maximizes
  #   the number of DEG's
  # Quantiles to test for independent filtering of p-adjusted values
  # theta <- seq(from=0, to=0.5, by=0.01)
  
  # Count number of rows/genes in every comparison
  results.nrows <- unlist(lapply(model.contrasts, nrow))
  
  # Combine results by row-binding them as data.frames
  # Add column indicating name of contrast 
  results.combined <- do.call(rbind, lapply(model.contrasts, as.data.frame)) %>%
    mutate(Contrast = rep(names(model.contrasts), times = results.nrows)) %>%
    mutate(Name = row.names(.)) %>%
    select(Contrast, everything())
  
  
  # Function to do p-value adjustment with Independent filtering the way DESeq2
  # does it
  adjusted.pvalues <- pvalueAdjustment(results.combined, alpha = padj.alpha, 
                                       theta = theta, pAdjustMethod = pAdjustMethod)
  
  # Adjust the p-values accordingly
  results.combined$padj <- adjusted.pvalues$res$padj
  # Update the metadata from the independent filtering for summary printing
  
  # Now replace the adjusted p-values in the DESeqResults objects
  #   Otherwise we're left with simple data.frames which include less
  #   information!
  for (i in seq_along(model.contrasts)) {
    ContrastName = names(model.contrasts)[i]
    model.contrasts[[i]]$padj <- (results.combined %>% 
      filter(Contrast == ContrastName))$padj
    # Add metadata to DESeqResults object from the pvalueAdjustment()
    metadata(model.contrasts[[i]])$filterThreshold <- adjusted.pvalues$meta$filterThreshold
    metadata(model.contrasts[[i]])$alpha <- adjusted.pvalues$meta$alpha
  }
  
  # Return model.contrasts and pvalue adjustment metadata for plotting
  if (returnMeta) {
    return(list(model.contrasts = model.contrasts, meta = adjusted.pvalues$meta))
  # Return same model.contrasts excpet with new p-adjusted values and metadata
  } else return(model.contrasts)
}

#' Adjust p-values of DESeq2 results in data.frames
#' 
#' Adjust p-values using independentFiltering the way DESeq2 does it when using the \code{\link[DESeq2]{results}} function.
#' The only difference is that this function works with data.frames as well, and therefore does not update the metadata.
#' Only DESeqResults objects have a metadata method.
#' 
#' 
#' Copied from
#' \url{https://github.com/mikelove/DESeq2/blob/master/R/results.R}
#' 
#' Copied because it is not exported and we can use it to do independentFiltering of
#' combined results in data.frames instead of DESeqResults objects
#' 
#' Does not adjust metadata of results, because there isn't any.
#' 
#' @param res A data.frame or DESeqResults object
#' @param filter defaults to the baseMean column of the results. Otherwise takes a vector of the same lengt has the number of rows of \code{res}
#' @param independentFiltering Logical whether to apply independent filtering. Defaults to TRUE 
#' @theta Can be missing - a sequence of quantiles to filter by for independent filtering
#' @pAdjustMethod A method passed to \code{\link[genefilter]{filtered_p}} from the \pkg{genefilter} package.
#'  
#' @export
#'
pvalueAdjustment <- function(res, filter, theta, alpha = 0.1,
                             pAdjustMethod = "BH", independentFiltering = TRUE) {
  
  # perform independent filtering
  library(genefilter)
  if (independentFiltering) {
    if (missing(filter)) {
      filter <- res$baseMean
    }
    
    if (missing(theta)) {
      lowerQuantile <- mean(filter == 0)
      if (lowerQuantile < .95) upperQuantile <- .95 else upperQuantile <- 1
      theta <- seq(lowerQuantile, upperQuantile, length=50)
    }

    # do filtering using genefilter
    stopifnot(length(theta) > 1)
    stopifnot(length(filter) == nrow(res))
    filtPadj <- filtered_p(filter=filter, test=res$pvalue,
                           theta=theta, method=pAdjustMethod) 
    numRej  <- colSums(filtPadj < alpha, na.rm = TRUE)
    # prevent over-aggressive filtering when all genes are null,
    # by requiring the max number of rejections is above a fitted curve.
    # If the max number of rejection is not greater than 10, then don't
    # perform independent filtering at all.
    
    lo.fit <- lowess(numRej ~ theta, f=1/5)
    if (max(numRej) <= 10) {
      j <- 1
    } else { 
      residual <- if (all(numRej==0)) {
        0
      } else {
        numRej[numRej > 0] - lo.fit$y[numRej > 0]
      }
     
      thresh <- max(lo.fit$y) - sqrt(mean(residual^2))
      j <- if (any(numRej > thresh)) {
        which(numRej > thresh)[1]
      } else {
        1  
      }
    }
    
    # j <- which.max(numRej) # old method
    
    padj <- filtPadj[, j, drop=TRUE]
    cutoffs <- quantile(filter, theta)
    filterThreshold <- cutoffs[j]
    filterNumRej <- data.frame(theta=theta, numRej=numRej)
    filterTheta <- theta[j]
    
    meta <- list()
    meta[["filterThreshold"]] <- filterThreshold
    meta[["filterTheta"]] <- filterTheta
    meta[["filterNumRej"]] <- filterNumRej
    meta[["lo.fit"]] <- lo.fit
    meta[["alpha"]] <- alpha
  } else {
    
    # regular p-value adjustment
    # does not include those rows which were removed
    # by maximum Cook's distance
    padj <- p.adjust(res$pvalue,method=pAdjustMethod)  
  }
  
  res$padj <- padj
  list(results = res, meta = meta)
}

