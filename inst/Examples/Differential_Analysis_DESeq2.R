library(DESeq2)
library(RnaSeqAnalysis)

########################
### SET THE DATA
########################
counts <- ReadCounts(system.file("ExampleData/MKMD/Liver_Counts.csv", package = "RnaSeqAnalysis"))
coldata <- ReadColData(system.file("ExampleData/MKMD/Liver_colData_Endpoints.csv", package = "RnaSeqAnalysis"))
annotationData <- AnnotateCounts(counts, species = "Mouse", key.type = "ENSEMBL",
                                 multiVals = "first")

dds <- DESeqDataSetFromMatrix(counts, coldata, design = ~1)
dds <- estimateSizeFactors(dds)
# Save some computation time for this example
#   Although you won't see a difference for pvalue adjustment per contrast or combined.
# dds <- dds[1:1000,]

########################
### SET THE COMPARISONS 
########################
comparisonfile <- system.file("ExampleData/MKMD/LiverAorta_MultiModel_Comparisons.csv", package = "RnaSeqAnalysis")
comparisonData <- ReadComparisons(comparisonfile)
comparisons <- ParseComparisons(comparisonData)
designs <- comparisons$designs
contrasts <- comparisons$contrasts
n.designs <- length(designs)


########################
### GET COMPARISON RESULTS
########################
# Estimate the DESeq model for every design
dds.models <- lapply(designs, EstimateDESeqModel, dds = dds)

# Get individual contrasts per model
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


# Check results with
# lapply(dds.contrasts[[1]], summary)
# lapply(dds.contrasts.new[[1]], summary)

########################
### EVERYTHING IS ALREADY WRAPPED IN ONE FUNCTION
########################
# Or .. just do:
dds.analysis <- DESeqAnalysis(dds[1:100,], comparisonFile = comparisonfile)
# Note: a lot of default values that you can't set in this function(YET)
#     like p-adjustment method or padj.alpha etc.