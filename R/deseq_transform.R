transformDds <- function(dds, transformation) {
  switch(transformation,
         "raw", counts(dds),
         "normalized", counts(dds, normalized = TRUE),
         # Assumes you set the variance stabilized transformed counts to mcols(dds)$vst
         "vst", mcols(dds)$vst,
         stop("Unkown transformation"))
}



#' deseq_transform
#'
#' Perform deseq2 transformations such as rlog and vst.
#'
#'
deseq_transform <- function(countTable,sampData=NULL,
                            deseq.design=~1,
                            blind=TRUE,
                            method='scale',
                            out.path='../data/out/'){

  suppressWarnings(suppressMessages(library(DESeq2)))

  deseq.design          <- as.formula(deseq.design)

  # read data files
  countTable.filename   <- sub("\\.[[:alnum:]]+$", "", basename(as.character(countTable)))
  countTable            <- read.table(countTable,sep='\t',header=T,row.names=1,check.names=F,stringsAsFactors=F)

  # add pseudocount
  # Ideally, this function would perform a check before normalization and add pseucounts only when needed.
  #countTable <- countTable+1

  if(!is.null(sampData) | !blind){
   sampData              <- read.table(sampData,sep='\t',header=T,row.names=1,check.names=F,stringsAsFactors=F)
   sampData.rnames     <- row.names(sampData)

   #sampData            <- data.frame(sampData[order(row.names(sampData)),])
   #row.names(sampData) <- sampData.rnames


   #countTable <- countTable[,order(names(countTable))]

  } else {
    sampData <- data.frame(data.frame(1:ncol(countTable)))
    row.names(sampData) <- names(countTable)
  }



  # create DESeq2 obj
  dds                   <- DESeqDataSetFromMatrix(countData = countTable,colData=sampData,design=deseq.design)
  dds                   <- estimateSizeFactors(dds)
  dds                   <- estimateDispersions(dds)

  # transform data
  countTable <- switch(method,
                      'vst'   = assay(varianceStabilizingTransformation(dds,blind)),
                      'rlog'  = assay(rlog(dds,blind)),
                      'scale' = counts(dds,normalized=TRUE),
                       stop('Unknown method'))


  # write output
  write.table(file = paste0(out.path,countTable.filename,'-','DESeq2_',gsub(toupper(method),pattern=' ',replacement='_'),ifelse(blind,'_BLIND',''),'.txt'),
              x = countTable,
              col.names=T,
              row.names=T,sep='\t')
}

