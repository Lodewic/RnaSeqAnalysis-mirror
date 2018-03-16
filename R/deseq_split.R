deseq_split <- function(countTable, sampData, split, out.path='../data/out/')
{
  # read input files
  sampData.filename   <- sub("\\.[[:alnum:]]+$", "", basename(as.character(sampData)))
  sampData              <- read.table(sampData,sep='\t',header=T,row.names=1,check.names=F,stringsAsFactors=F)


  countTable.filename   <- sub("\\.[[:alnum:]]+$", "", basename(as.character(countTable)))
  countTable            <- read.table(countTable,sep='\t',header=T,row.names=1,check.names=F,stringsAsFactors=F)

  split.groups <- unique(sampData[[split]])

  for(group in split.groups)
  {
    countTable.split <- countTable[,split.groups == sampData[[split]]]
    sampData.split   <- sampData[ split.groups == sampData[[split]],]

    # write output
    write.table(file = paste0(out.path,countTable.filename,'-',gsub(group,pattern=' ',replacement='_'),'.txt'),
                x = countTable.split,
                col.names=T,
                row.names=T,sep='\t')

    write.table(file = paste0(out.path,sampData.filename,'-',gsub(group,pattern=' ',replacement='_'),'.txt'),
                x = sampData.split,
                col.names=T,
                row.names=T,sep='\t')

  }

}
