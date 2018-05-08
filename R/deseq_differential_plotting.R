#' Title
#'
#' @param dds.analysis 
#' @param annotationData 
#' @param groups 
#' @param color.var 
#' @param alpha 
#' @param normalized 
#' @param glimma.alpha 
#' @param folder 
#' @param main 
#' @param html 
#' @param verbose 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
PlotDESeqAnalysis <- function(dds.analysis, annotationData = NULL,
                              groups = NULL,
                              color.var = NULL,
                              alpha=0.05,
                              normalized = TRUE,
                              glimma.alpha = 1,
                              folder='./Glimma', 
                              main=NULL,
                              html='MD-Plot',
                              verbose = 1, # Set to 0 for silent
                              ...) {
  
  annotationInput <- annotationData
  names.given <- !is.null(names(dds.analysis$Models))
  md.plot.files <- list()
  
  for (model.ind in seq_along(dds.analysis$Models)) {
    dds.model <- dds.analysis$Models[[1]]
    if (nrow(annotationInput) != nrow(dds.model)) {
      annotationData <- annotationInput[match(row.names(dds.model),
                                              row.names(annotationInput)),]
    } else annotationData <- annotationInput
    # Name the model for plots
    if (names.given) {
      design.fname <- names(dds.analysis$Models)[1]
    } else {
      # Make a character variable of the design for printing
      design.fname <- gsub("[~+]", "", design(dds.model))[[2]]
      design.fname <- gsub("  ", "_", design.fname)
    }
    
    # 
    model.contrasts <- dds.analysis$Results[[model.ind]]
    model.comparisons <- dds.analysis$Comparisons$contrasts[[model.ind]]
    
    # Add list element to output list of filenames
    md.plot.files[[model.ind]] <- list()
    
    # Loop over contrasts for given  model
    for (contrast.ind in seq_along(model.contrasts)) {
      contrast.string <- model.comparisons[contrast.ind]
      contrast.vec <- unlist(strsplit(contrast.string, split = " "))
      
      # Get DESeqResults from the deseq2 analysis for current contrast
      dds.res <- model.contrasts[[contrast.ind]]
      
      # Same annotation you'd see for the results in the R console above the
      # results data.frame
      contrast.description <- paste0(mcols(dds.res)$description, 
                                     collapse = "\n")
      
      # Strip text off of 'log2 fold change MAP: "contrast.char"' 
      contrast.char <- strsplit(
        mcols(dds.res)$description[2], 
        split = ": ")[[1]][2]
      
      # Character names for output file and plot title
      html.name <- paste0('MD-Plot ', 'Comp', paste0(model.ind, ".", contrast.ind))
      main.name <- paste0('MD-Plot ', design.fname, ": ",
                          contrast.char)
      
      # Verbose message
      if (verbose > 0) print(sprintf("Plotting %s: %s",
                                       names(model.contrasts)[contrast.ind],
                                       contrast.char))
      
      
      # Plot Glimma MD plot for given contrast results
      # returns file location of md plot .html file
      md.plot <- PlotGlimmaContrast(dds.model, 
                                dds.res,
                                color.var,
                                annotationData = annotationData,
                                contrast = contrast.vec,
                                groups = groups,
                                alpha=alpha,
                                normalized = TRUE, 
                                glimma.alpha = glimma.alpha,
                                folder = "Glimma",
                                main = main.name,
                                html = html.name,
                                launch = FALSE)
      # Add file location to output list
      md.plot.files[[model.ind]][[names(model.contrasts)[contrast.ind]]] <- md.plot
    }
  }
  
  dds.analysis$MD.Plots = md.plot.files
  return(dds.analysis)
}

#' Plot Glimma MD plot for DESeq2 results
#'
#' Helper function for DESeq2 analysis reports
#' Plot an MD plot of DESeq2 results with a specific contrast
#' - Requires the Glimma package
#' Outputs a Glimma MD plot .html file into a specified subfolder of the report

#' @param dds DESeq dataset with computed sizeFactors and dispersions
#' @param dds.res  DESeqResults object - or alternatively only the results in a data.frame
#' @param annotationData Annotation data as data.frame
#' @param groups A single value or vector of character values of features in the colData of dds to separate groups by in the plot
#' @param color.var Sample variable/feature to color expression plot by
#' @param contrast Contrast vector used in DESeq's \code{\link{results}} function. 
#'     Used for determining groups in the expression plot only if neither \code{groups} or \code{color.var} is not given
#' @param alpha P-adjusted significance level for coloring DEG's
#' @param normalized Logical whether to use normalized expression values. Default TRUE
#' @param glimma.alpha P-value cutoff to eliminate genes from glimma plot. May save diskspace by removing uninteresting genes. Default is 1 (does not remove anything)
#' @param folder Output folder. Default to /Glimma subdirectory
#' @param main Title of the glimma plot
#' @param html HTML file output title
#' @param launch Boolean to open Glimma plot output in browser. Default FALSE (use False in reports!)
#'
#' @return
#' Saves a Glimma plot (.html file) to the specified directory. Returns relative file location as a string.
#'
#' @export
#'
#'
PlotGlimmaContrast <- function(dds, dds.res, annotationData,
                           groups = NULL,
                           color.var = NULL,
                           contrast = NULL,
                           alpha=0.05,
                           normalized = TRUE, glimma.alpha = 1,
                           folder='./Glimma', main=NULL,
                           html='MD-Plot',
                           launch = FALSE, ...) {
  require(Glimma)
  require(viridisLite)
  
  # Backwards compatibility
  # glimma.alpha used to be p.alpha
  #   Name change because of confusing variable input name
  # Added ... dots argument so p.alpha can be passed. If it is, set glimma.alpha=p.alpha
  # Also give a warning that p.alpha will be deprecated!
  if (hasArg(p.alpha)) {
    warning("p.alpha is deprecated because of confusing variable naming. Use glimma.alpha instead!")
    glimma.alpha <- list(...)$p.alpha
  }
  
  # Fix contrast argument in case it is a single list element (for continuous variables mostly)
  if (length(contrast) == 1) contrast <- c(contrast[[1]])
  
  # Only plot points up to a sigfnicance level based on the pvalue
  #   Default is plot everything!
  select.genes <- na.omit(dds.res$pvalue < glimma.alpha)
  dds.res <- dds.res[select.genes,]
  
  # Get coldata from DeseqDataSet for readability
  sample.data <- as.data.frame(colData(dds))
  
  # Indicator for up/down regulated significantly different genes - to color points
  dt.res <- as.numeric((dds.res$padj<alpha & dds.res$log2FoldChange>0) -
                         (dds.res$padj<alpha & dds.res$log2FoldChange<0))
  # Change NA, FALSE or TRUE values to -1, 0, 1
  # dt.res <- sapply(dt.res, function(x) ifelse(is.na(x), -1, x))
  
  # If groups is not given then try to get it from the contrast value
  # groups is supposed to be a vector of colData feature names or a single name
  if (is.null(groups)) {
    # Retrieve factor of relevant group for expression plot groups
    if (contrast[1] %in% names(sample.data)) {
      dds.group <- sample.data[,contrast[1]]
      # Sometimes we give the contrast as numeric vector
    } else if (is.numeric(contrast) &
               length(contrast) == length(resultsNames(dds))) {
      resnames <- resultsNames(dds)
      contrast.names <- paste0(resnames[which(contrast != 0)], collapse = " ")
      contrast.features <- sapply(names(sample.data), grepl,
                                  x = contrast.names)
      
      # Split resultsNames in contrast and create new group from included groups
      if (sum(contrast.features) == 1) dds.group <- sample.data[, contrast.features]
      else {
        coldata.df <- as.data.frame(sample.data[, contrast.features])
        dds.group <- factor(apply(coldata.df, 1,
                                  function(x) paste0(as.character(x), collapse = "_")))
      }
      
      # If interaction effect in model design - get both groups of interaction
    } else if (grepl(":", design(dds))[2]) {
      # Parse the interaction term from the model design
      dsgn.char <- unlist(strsplit(as.character(design(dds))[2], " \\+ "))
      dds.group <- dsgn.char[which(grepl(":", dsgn.char))]
      dds.group <- as.data.frame(sample.data[, unlist(strsplit(dds.group, ":"))])
      # Split interaction term groups and create new group from included groups
      dds.group <- factor(apply(dds.group, 1, paste0, collapse = "_"))
    } else {
      # If group name not in ColData - give warning and use last variable in model design
      warning("MD Plot contrast group not found in ColData for expression plot!")
      dsgn.char <- unlist(strsplit(as.character(design(dds))[2], " \\+ "))
      dds.group <- sample.data[, last(dsgn.char)]
    }
  } else {
    if (length(groups) == 1) dds.group <- sample.data[, groups]
    else {
      coldata.df <- as.data.frame(sample.data[, groups])
      dds.group <- factor(apply(coldata.df, 1,
                                function(x) paste0(as.character(x), collapse = "_")))
    }
  }
  
  # Add sample colors
  col.vec <- c("red", "blue", "green", "black", "cyan",
               "darkgoldenrod3", "darkorchid4", "hotpink")
  
 
  # Determine color vector
  if (!is.null(color.var)) {
    sample.cols <- col.vec[as.numeric(sample.data[,color.var])]
  } else {
    n.colors <- length(unique(dds.group))
    if (is.numeric(dds.group)) {
      sample.cols <- viridis(n.colors, option = "B")[sort(dds.group)]
    } else {
      dds.group <- as.factor(dds.group)
      sample.cols <- viridis(n.colors)[as.numeric(as.factor(dds.group))]
    }
  }
  
  
  
  # If class of dds.res is data.frame and not DESeqResults
  # Then add the logMean column to it and specify the columns for glMDPlot()!
  if (class(dds.res) == "data.frame") {
    dds.res <- dds.res %>% mutate(logMean = log(baseMean)) %>%
      rename("log2FC" = log2FoldChange,
             "Adj.Pvalue" = padj)
    
    # Add names to result
    #   Gene names given in the Name column (NOT ROWNAMES)
    match.inds <- match(dds.res$Name, rownames(annotationData))
    gene.data <- data.frame(list(GeneID=dds.res$Name,
                                 BaseMean=dds.res$baseMean,
                                 pvalue=dds.res$pvalue))
    
    if (!is.null(annotationData)) {
      gene.data <- cbind(annotationData[match.inds,], gene.data)
    }
    
    # MD plot with Glimma
    glMDPlot(cbind(gene.data, dds.res), xval = "logMean", yval = "log2FC", status=dt.res,
             counts=counts(dds, normalized = normalized)[select.genes,],
             groups=dds.group,
             transform=F, samples=colnames(dds), main=main,
             folder=folder, html=html, launch=launch,
             sample.cols=sample.cols,
             display.columns = c(names(gene.data),
                                 "Adj.Pvalue", "baseMean"))
  } else {
    gene.data <- annotationData
    gene.data$GeneID <- row.names(annotationData)
    # For a DESeqDataset object (dds.res) Glimma will figure out some stuff itself
    # MD plot with Glimma
    glMDPlot(dds.res, status=dt.res,
             counts=counts(dds, normalized = normalized)[select.genes,],
             groups=dds.group,
             transform=F, samples=colnames(dds), anno=gene.data, main=main,
             folder=folder, html=html, launch=launch,
             sample.cols=sample.cols)
  }
  #
  glimma.md.loc <- file.path(folder, paste0(html, ".html"))
  # When we're using relative links, add a "./" in front of the filename
  #   Absolute paths on Windows contain ":" so we check for this.
  if (!grepl(":", glimma.md.loc)  & !startsWith(glimma.md.loc, "./")) {
    glimma.md.loc <- file.path(".", glimma.md.loc)
  }
  return(glimma.md.loc)
}
