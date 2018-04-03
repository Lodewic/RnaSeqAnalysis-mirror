#' Pairplot of genes and selected features
#' 
#'
#' @param dds A heavily filtered DESeqDataSet, i.e. a maximum of 10 genes
#' @param features Names of features to include in the pairplot so they can be correlated with gene expression. Need to correspond to column names in colData(dds).
#' @param colour Single feature name to colour plots by.
#'
#' @return
#' @export
#'
#' @examples
plot_gene_pairs <- function(dds, features = NULL, colour = NULL) {
  require(tidyverse)
  require(GGally)
  # dds is expected to be a filtered DESeqDataSet object
  #   Notably, we only expect a maximum of 10 genes! Recommended to filter by the highest variance for example.
  
  # Only plot the first 10 genes
  #   ggpairs with more than 10 becomes messy and slow. There are other methods to plot more than 10 genes.
  if (dim(dds)[1] > 10) {
    warning("More than 10 genes to plot! Only plotting first 10 genes") 
    dds <- dds[1:10,]
  }
  
  #
  gene.names <- rownames(dds)
  
  # Bind coldata and normalized gene expression
  colData_HighestGenes <- cbind(
    as.data.frame(colData(dds)),
    t(as.data.frame(counts(dds, normalized = TRUE))))

  # Use ggpairs() to plot gene pairs and features
  colData_HighestGenes %>% 
    select(unlist(list(colour, features, gene.names))) %>%
    GGally::ggpairs(columns = length(colour):ncol(.),
                    mapping = aes_string(colour = colour))
}

