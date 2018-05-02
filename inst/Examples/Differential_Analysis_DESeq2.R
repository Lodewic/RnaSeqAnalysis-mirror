library(DESeq2)
library(RnaSeqAnalysis)
counts <- ReadCounts(system.file("ExampleData/MKMD/Liver_Counts.csv", package = "RnaSeqAnalysis"))
coldata <- ReadColData(system.file("ExampleData/MKMD/Liver_colData_Endpoints.csv", package = "RnaSeqAnalysis"))
annotationData <- AnnotateCounts(counts, species = "Mouse", key.type = "ENSEMBL",
                                 multiVals = "first")

dds <- DESeqDataSetFromMatrix(counts, coldata, design = ~1)
# Save some computation time for this example
#   Although you won't see a difference for pvalue adjustment per contrast or combined.
# dds <- dds[1:1000,]

comparisonfile <- system.file("ExampleData/MKMD/LiverAorta_MultiModel_Comparisons.csv", package = "RnaSeqAnalysis")
comparisonData <- ReadComparisons(comparisonfile)
comparisons <- ParseComparisons(comparisonData)
designs <- comparisons$designs
contrasts <- comparisons$contrasts
n.designs <- length(designs)

# Estimate the DESeq model for every design
dds.models <- lapply(designs, EstimateDESeqModel, dds = dds)

# Get individual contrasts per model
#   The p-value adjustment is done per contrast
dds.contrasts <- lapply(1:n.designs, function(model.ind) {
  GetDESeqContrasts(dds.models[[model.ind]],
                    contrasts = contrasts[[model.ind]],
                    model.index = model.ind)
})

# Apply pvalue adjustment to every top-level list element of dds.contrasts
#   A.k.a. for each model individually.
dds.contrasts.new <- lapply(dds.contrasts, AdjustDESeqContrasts)


# Or .. just do:
dds.analysis <- DESeqAnalysis(dds, comparisonFile = comparisonfile)
# Note: a lot of default values that you can't set in this function(YET)
#     like p-adjustment method or padj.alpha etc.