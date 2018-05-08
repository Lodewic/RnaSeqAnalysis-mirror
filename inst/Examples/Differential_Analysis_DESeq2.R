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
dds <- getData(system.file("ExampleData/MKMD/Liver_Counts.csv", package = "RnaSeqAnalysis"),
               system.file("ExampleData/MKMD/Liver_colData_Endpoints.csv", package = "RnaSeqAnalysis"),
               species = "Mouse", keytype = "ENSEMBL",
               calc.disps = FALSE, calc.vst = FALSE)

dds <- estimateSizeFactors(dds)
dds <- estimateDispersions(dds)

# Save some computation time for this example
#   Although you won't see a difference for pvalue adjustment per contrast or combined.
dds <- dds[1:1000,]


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
dds.input <- dds
mcols(dds.input)$annotation <- NULL
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

# Combine relevant results of the differential analysis into a list
dds.analysis <- list(Models = dds.models, 
                     Results = dds.contrasts, 
                     Comparisons = comparisons,
                     meta = adjusted.meta)

########################
### EVERYTHING IS ALREADY WRAPPED IN ONE FUNCTION
########################
# Or .. just do:
dds.analysis <- DESeqAnalysis(dds, comparisonFile = comparisonfile)
# Note: a lot of default values that you can't set in this function(YET)
#     like p-adjustment method or padj.alpha etc.
# One of the outputs is 'dds.analysis$meta' which holds the results on the
# independent filtering of the p-values that can be used for plotting.

##
##### PLOT RESULTS ####
## using Glimma MD plots
## Output is dds.analysis with new list dds.analysis$MD.Plots with md-plot file locations
dds.analysis <- PlotDESeqAnalysis(dds.analysis, annotationData = annotationData)
dds.analysis$MD.Plot <- NULL

# Save output as example data
#   Assuming the package folder is your working directory
save(dds.analysis, file = "inst/ExampleData/AnalysisOutput/Liver_DDS_Analysis.RData")
save(annotationData, file = "inst/ExampleData/AnalysisOutput/Liver_DDS_Annotation.RData")
