# Function to read gene expression data into a DESeq dataset
#' Title
#'
#' @param countdata Filename of count / expression data in a format where rownames are genes/features and columns are samples.
#' @param coldata Filename of column / sample data. Sample data has samples in the rows and features in the columns. The rows of colData should preferably equal the column names of the count data in the same order.
#' @param annotationdata (OPTIONAL) Filename of annotation data with gene names in the rownames, equal to the count data, and different annotations in the columns. E.g. ENSEMBL gene id's in the rownames, and SYMBOL and Entrez Id's in other columns.
#' @param species The species of the gene annotations, e.g. Mouse, Human, Rat, etc. 
#' @param key.type The gene id key, e.g. ENSEMBL, ENTREZ or SYMBOL etc.
#'
#' @return
#' @export
#'
#' @examples
getData <- function(countdata, coldata, species = NULL, keytype = NULL, annotationdata = NULL,
                    calc.sizeFactors = TRUE, calc.disps = TRUE, calc.vst = FALSE) {
  require(DESeq2)
  require(AnnotationDbi)
  
  warning("calc.vst is nog longer used")
  
  if (!any(c(is.null(species), is.null(keytype), is.null(annotationData)))) {
    warning("Annotation is no longer done with getData(). Use AnnotateCounts() instead")
  }
  # Read count and sample data - should always be given
  counts.out <- ReadCounts(countdata)
  coldata.out <- ReadColData(coldata)
  
  # Make into DESeq dataset
  #   Always calculate dispersions and variance-stabilized counts
  dds <- DESeqDataSetFromMatrix(counts.out, coldata.out, ~1)
  if (calc.sizeFactors) dds <- estimateSizeFactors(dds)
  
  #     # Estimating dispersions before the vst sometimes given an error... not sure why.
  if (calc.disps) dds <- estimateDispersions(dds)
  
  # Add data.frame as annotation data to the metadata of the dds object
  # !!!! ADDING NEW DATA as a data.frame TO MCOLS(DDS) BREAKS THE REFITTING STEP OF DESEQ() !!!!
  # SO setting mcols(dds) or rowData(dds) (same thing) seems like the wrong way to go
  # mcols(dds)$annotation <- annotation.out
  
  
  # Return a DESeqDataset with annotation data in the metadata (mcols())
  return(dds)
}



#' Make an educated guess on the separator character
#'
#' This function tries to guess which separator was used in a text delimited file
#' 
#' 
#' from https://github.com/federicomarini/ideal/blob/master/R/helpers.R
#' From the author of the BioConductor package 'ideal' - Federico Marini :)
#'
#' @param file The name of the file which the data are to be read from
#' @param sep_list A vector containing the candidates for being identified as
#' separators. Defaults to \code{c(",", "\t", ";"," ")}
#'
#' @return A character value, corresponding to the guessed separator. One of ","
#' (comma), "\\t" (tab), ";" (semicolon)," " (whitespace)
#' @export
#
#' @examples
#' # Examples rely on the ideal package for now
#' sepguesser(system.file("extdata/design_commas.txt",package = "ideal"))
#' sepguesser(system.file("extdata/design_semicolons.txt",package = "ideal"))
#' sepguesser(system.file("extdata/design_spaces.txt",package = "ideal"))
#' mysep <- sepguesser(system.file("extdata/design_tabs.txt",package = "ideal"))
#'
#' # to be used for reading in the same file, without having to specify the sep
#'
sepguesser <- function(file, sep_list = c(",", "\t", ";"," ")) {
  separators_list = sep_list
  rl = readLines(file, warn = FALSE)
  rl = rl[rl != ""] # allow last line to be empty
  sephits_min = sapply(separators_list, function(x) min(stringr::str_count(rl, x))) #minimal number of separators on all lines
  sep = separators_list[which.max(sephits_min)]
  sep
}


#' Read rna-seq count data
#' 
#' @param COUNTFILE 
#' @param sep 
#' @param sort.samples 
#'
#' @export

ReadCounts <- function(COUNTFILE, sep = NULL, sort.samples = TRUE) {
  # Read unnormalized count data from .csv file
  if (is.null(sep)) sep <- sepguesser(COUNTFILE)
  
  #   Try() so that duplicate row.names may be caught if needed
  countTable <- try(read.csv(COUNTFILE, header = T,
                             stringsAsFactors = T,row.names = 1, sep = sep, 
                             check.names = F),
                    silent = TRUE)
  
  # Try to handle duplicate rownames
  if (class(countTable) == "try-error") {
    if (grepl("duplicate", countTable[1])) {
      # Read counts without rownames
      countTable <- read.csv(COUNTFILE, header = T,
                             stringsAsFactors = F, sep = sep)
      
      # Remove all zeros before we set rownames
      #   Duplicate rownames aren't an issue if they are gone after
      #   removing zero-count genes
      countTable <- countTable[which(rowMeans(countTable[,-1])>1),]
      
      # If error not resolved - line below still raises error
      row.names(countTable) <- countTable[,1]
      
      # Remove first column 
      countTable <- countTable[, -1]
      # If isssue is not duplicate row names - raise error anyway
    } else stop(cat(countTable))
  }
  
  # Make sure we still remove all rows with zero counts if there is no error
  countTable <- countTable[which(rowMeans(countTable)>1),]
  
  # Sort sample names in column names to align with coldata
  if (sort.samples) {
    countTable <- countTable[, sort(colnames(countTable),
                                    index.return = TRUE)$ix]
  }
  #
  return(countTable)
}



#' Read rna-seq annotation data
#'
#' @param ANNOTATIONFILE Path to annotation file. File should have gene ID's in the rows, same gene ID's as the expression data.
#' @param sep (Optional) Separator of file, determined automatically if not given
#' @param quote parameter passed to \code{\link{read.table}}. Default disables quoting
#' @return data frame of annotation data
#' @export
ReadAnnotation <- function(ANNOTATIONFILE, sep = NULL, quote = "\"'") {
  # Determine separator if not given
  if (is.null(sep)) sep <- sepguesser(ANNOTATIONFILE)
  
  # Read column / sample data from file
  annotationData <- read.table(ANNOTATIONFILE, header = T,
                               stringsAsFactors = T,row.names = 1, sep = sep, quote = quote)
  
  # Return column / sample data
  return(annotationData)
}


#' Read rna-seq coldata file (sample data)
#' 
#' This function is used to read coldata files, or sample data files. Every row is a sample and every column a feature.
#' The rownames should be the same as the column names in the expression data. Or, at least the expression columnnames and sample data rownames should overlap.
#' By default the sample and expression data are sorted by their rownames so they automatically align if all sample names overlap.
#'
#' @param COLDATAFILE Path to annotation file. File should have gene ID's in the rows, same gene ID's as the expression data.
#' @param sep (Optional) Separator of file, determined automatically if not given
#' @param sort.samples Logical whether to sort samples by rownames. Default TRUE. This will make is easier to align expression data columns and sample data rownames.
#'
#' @return A data.frame of the sample data
#' @export
#'
#' @examples
ReadColData <- function(COLDATAFILE, sep = NULL, sort.samples = TRUE) {
  # Determine separator if not given
  if (is.null(sep)) sep <- sepguesser(COLDATAFILE)
  
  # Read column / sample data from file
  colData <- read.csv(COLDATAFILE, header = T,
                      stringsAsFactors = T,row.names = 1, sep = sep)
  
  # Sort sample names in row names to align with counts
  if (sort.samples) {
    colData <- colData[sort(rownames(colData),
                            index.return = TRUE)$ix,]}
  # Return column / sample data
  return(colData)
}

#' Read comparison file
#'
#' @param COMPARISONFILE Path to comparison file in .csv format(!)
#' @param sep (Optional) Separator of file. Determined automatically if not given.
#'
#' @return
#' @export
#'
ReadComparisons <- function(COMPARISONFILE, sep = NULL) {
  #######################################################
  # NOTE:
  #   The way we define the comparisons and designs should not be
  #   given in a .csv file. So this function is definitely temporary until
  #   a better input format comes along to replace it.
  #   
  #   More importantly - the comparisons are just a 3 characters right now
  #   but should also extend to numeric contrasts vectors (used in DESeq2)
  #######################################################
  
  # Determine separator if not given
  if (is.null(sep)) sep <- sepguesser(COMPARISONFILE)
  
  # Comparison data for differential analysis
  #  Rownames not currently used but can serve as
  #  shortnames for specific contrasts.
  #  
  #  Comparison file has a Description and a Value column
  #  where Description is either 'Design' or 'Comparison'.
  compData <- read.csv(COMPARISONFILE,
                       header = T,
                       row.names = 1, sep = sep,
                       # Force column classes
                       colClasses = c('character', 'factor', 'character'))
  
  # Output comparisons as a data.frame
  return(compData)
}

#' Load the correct annotation database given the species
#'
#' @param gene.species 
#'
#' @return
#' @export
#'
#' @examples
LoadAnnotationDb <- function(gene.species) {
  # Annotation libraries
  require(pathview)
  require(AnnotationDbi)
  
  # Load data so we can determine the annotation database to load
  data(bods)                # From pathview package
  bods <- data.frame(bods)
  
  # List of db names named by their speciesdata(b)
  database.packages <- setNames(bods$package, nm = bods$species)
  
  # Name the annotation package to use - to be used in annotation chunk(s)
  if (gene.species %in% bods$species) {
    annotation.package <- with(bods, package[which(species == gene.species)])
    # Return name of annotation package for given species
    return(as.character(annotation.package))
  } else {
    warning("Given species not implemented or known")
    # Return NULL if not found
    return(NULL)
  }
}

#' Parse rna-seq comparison file
#'
#' @param compData Comparison data in data.frame from \code{\link{ReadComparisons}}
#'
#' @return list of designs and contrasts
#' @export
#'
ParseComparisons <- function(compData) {
  # Parse a data.frame of comparisons (from ReadComparisons())
  # Returns a list of designs and contrasts
  # 
  # Note: This will be replaced fairly soon if the input of 
  # comparisons is updated - which definitely needs to happen
  ############################################################
  # Initialize
  designs <- c()
  contrasts <- list()
  cntrst.tmp <- c()
  n.designs <- 0
  # Loop over comparison data
  for (i in 1:nrow(compData)) {
    # If the row is a design parameter
    if (tolower(compData$Description[i]) == 'design') {
      # Then add it to the vector of design formulae
      designs <- c(designs, as.formula(compData$Value[i]))
      
      # If it not the first design - add an element to the contrasts list
      if (n.designs > 0) contrasts <- c(contrasts, list(cntrst.tmp))
      n.designs <- n.designs + 1
      # Reinitialize the contrasts for the next design
      cntrst.tmp <- c()
      # If the row is a comparison / contrast
    } else if (tolower(compData$Description[i]) == 'comparison') {
      # Add contrast to temp contrast vec - for the current design only
      cntrst.tmp <- c(cntrst.tmp, compData$Value[i])
    }
  }
  # Multiple contrast vectors in a list
  #   As many elements as there are designs
  contrasts <- c(contrasts, list(cntrst.tmp))
  
  # Return named list of designs and contrasts
  return(list(designs = designs, contrasts = contrasts))
}

#' Annotate count data with AnnotationDbi
#'
#' @param counts The count data as read with \code{\link{ReadCounts()}}.
#' @param species The species used for the gene annotation
#' @param key.type Keytype used for gene annotation, e.g. ENSEMBL, Entrez, etc.
#' @param column.keys A named vector of other keytypes to add to the annotation using the \pkg{AnnotationDbi} package
#' @param multiVals What to do with multiple matching annotations , passed to \code{\link{mapIds}}
#'
#' @return A data.frame with all chosen annotations according to \code{column.keys}
#' @export
#'
#' @examples
AnnotateCounts <- function(counts, species, key.type, 
                           column.keys = c('SYMBOL', 'ENTREZID', 'ENSEMBL', 'GENENAME'),
                           multiVals = 'first') {
  # Needs annotationDbi
  require(AnnotationDbi)
  
  # Determine which annotation package to use
  annotation.package <- LoadAnnotationDb(species)
  if (is.null(annotation.package)) {
    return(NULL)
  } else {
    # Install annotation package if not yet available
    if (!require(annotation.package, character.only = TRUE)) {
      source("https://bioconductor.org/biocLite.R")
      # Avoid updating dependencies - it's just an annotation package.. :)
      biocLite(annotation.package, suppressUpdates = T, ask = FALSE)
      require(annotation.package, character.only = TRUE)
    }
  }
  # Annotation mapping
  #   Use get({{ANNOTATIONPACKAGE}}) where ANNOTATIONPACKAGE is something like
  #   org.Hs.eg.db for humans
  #   org.Mm.eg.db for Mice
  #   etc.
  #   
  #   To see all available names run,
  #     library(pathview)
  #     data(bods)
  #     bods
  
  # Create a named list
  annotationList <- list()
  # Loop over currently possible annotation keys
  for (curr.key in column.keys) {
    # If the keytype of the countdata is not already the given key
    if (key.type != curr.key) {
      # Then add annotation for this keytype to a named list
      #  Uses mapIds from the annotationDbi package
      annotationList[[tolower(curr.key)]] <- mapIds(get(annotation.package),
                                                    keys = row.names(counts),
                                                    column = curr.key,
                                                    keytype = key.type,
                                                    multiVals = multiVals)}}
  
  # Return annotation as a data.frame
  return(data.frame(annotationList))
}



