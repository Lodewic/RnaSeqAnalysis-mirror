#' Subset deseq dataset by samples
#'
#' @param dds A DESeqDataSet object with the sample data in colData(dds)
#' @param subset.list A list of two-element vectors. The first element of every vector is the column / feature name, all further elements are feature levels to keep.
#' @param verbose Logical. Whether to output a message listing the removed samples (default FALSE)
#' 
#' @return
#' @export
#'
#' @examples
#' dds <- deseq_sample_subset(dds, list(c("Diet.x", chow"), c("TimeNum", 12, 18, 24)))
#' 
deseq_sample_subset <- function(dds, subset.list, verbose = FALSE) {
  # Subset.list should be a list of two-element vectors
  #   First element is the name of the column to subset by
  #   Second element can be a value or a vector of values to keep
  
  # We lose rownames if we start using %>% so make a new column with the rownames
  sample.subsets <- sapply(subset.list, function(x) which(colData(dds)[[x[1]]] %in% x[-1]))
  
  # If only 1 subset is given then the result is a matrix with 1 column
  #   Otherwise it's a list and dim(sample.subsets) returns NULL
  if (!is.null(dim(sample.subsets))) {
    unique.sample.inds <- as.vector(sample.subsets)
  } else unique.sample.inds <- Reduce(intersect, sample.subsets)
  
  if (verbose) {
    # Check if we actually remove any samples (if the argument is given that is)
    eval.outliers <- setdiff(1:ncol(dds), unique.sample.inds)
    
    # Only evaluate chunks based on samples indices to keep if it exists
    if (length(eval.outliers) > 0) {
      # Start of text if outliers were removed at all
      outlier.text <- sprintf(
        "These are the %i samples we remove of a total of %i samples,\n\n",
        length(unique.sample.inds), ncol(dds))
      
      # Determine text to show for samples names
      #   Format as list with elements "'outlier index' - 'sample name'"
      samples.list.text <- paste0(unique.sample.inds, " - ",
                                  colnames(dds)[unique.sample.inds],
                                  collapse= "\n\t* ")
      samples.list.text <- paste0("\t* ", samples.list.text)
      
      # Combine text with text list
      outlier.text <- paste0(outlier.text, samples.list.text)
    } else {
      outlier.text <- "No samples were removed"
    }
    message(cat(outlier.text))
  }
  
  
  # Filter deseq dataset
  dds[, as.vector(unique.sample.inds)]
}
