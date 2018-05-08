#### Lodewic van Twillert
# This script performs differential analysis
# 
# New folders and files will be created  in your working directory
# 
# Saves the intermediate output as .RData files
#   - dds
#   - annotationData
#   - dds.analysis
#   
# Then report sections are copied from the package 
# and saved to your working directory.
# 
# Reports are rendered with parameters, using analysis output file locations
#   - For this purpose we'd prefer that the output data lies in a 
#     subdirectory of the local report files used to render
#   
# You should be left with 3 .html reports 
#   - input data report
#   - array quality metrics (including AQM output folder)
#   - Differential expression report
#     - including Glimma output plots
#     
# 
# Once we're sure that all reports render individually we can render them as
# a  website. I used a modified version of rmarkdown::render_site() and 
# added it to the RnaSeqAnalysis package. With this we can use output_files in
# the _site.yml that defines the report website. Essentially it allows us to
# render a site with report parameters! :D
# 
# The _site.yml may be easily created from a snakefile, the format looks very
# similar.
#####



# Load libraries
# TODO: Add DESeq2 as an RnaSeqAnalysis dependency
library(RnaSeqAnalysis)
library(DESeq2)

# Set output directories
# Preferably you'll want the .Rmd section files to be one folder
# above the data analysis output. This makes is much easier to use render_site()
# later correctly.
output.dir <- "" # Just set your working directory 
setwd(output.dir)
output.data.dir <- "Data"

# Set AQM project title and plots to color
int.groups <- "Diet.x TimeNum" # feature names separated by spaces
project.name <- "Test Pipeline Analysis" # Title for aqm report

if (!dir.exists(output.data.dir)) {
  dir.create(output.data.dir, recursive = T)
}

# Bibliography
# A bibtex .bib bibliography file for references in all report sections
# This needs to be present before rendering the first report (That uses it)
file.copy(system.file("ReportSections/bibliography.bib", package = "RnaSeqAnalysis"),
          "bibliography.bib")


##############################################
### GET THE DATA
### using example data in the package

# Set the input files
countfile <- system.file("ExampleData/MKMD/Liver_Counts.csv", package = "RnaSeqAnalysis")
coldatafile <- system.file("ExampleData/MKMD/Liver_colData_Endpoints.csv", package = "RnaSeqAnalysis")
species <- "Mouse"
key.type <- "ENSEMBL"

# Get the data into a dds
#   DO NOT USE getData() 
#   Setting mcols(ddS) or rowData(dds) breaks the refitting step of DESeq()
#   Only if you set mcols(dds) = "some data.frame" though.
#   I suppose you could use cbind(mcols(dds), annotationData) instead.
#   Still it seems cleaner to do annotation separately so we can save the output
#   separately and logically as well.
counts <- ReadCounts(countfile)
coldata <- ReadColData(coldatafile)
annotationData <- AnnotateCounts(counts, species = species, key.type = key.type,
                                 multiVals = "first")

dds <- DESeqDataSetFromMatrix(counts, coldata, design = ~1)
dds <- dds[1:1000,] # Save computation time

# No need to re-estimate factors for every model
# But always recalculate after removing genes or samples
dds <- estimateSizeFactors(dds)
dds <- estimateDispersions(dds)

# Save output as example data
save(dds, file = file.path(output.data.dir, "Liver_DDS.RData"))
# Also save annotation data separately!
save(annotationData, file = file.path(output.data.dir, "Liver_DDS_Annotation.RData"))

# Make report section with dds output
# Data input section
file.copy(system.file("ReportSections/RnaSeq-Data-Input.Rmd", package = "RnaSeqAnalysis"),
          "RnaSeq-Data-Input.Rmd")
# # Render reports with parameters
input.report <- rmarkdown::render("RnaSeq-Data-Input.Rmd",
                                  params = list(
                                    echo = FALSE,
                                    dds = file.path(output.data.dir, "Liver_DDS.RData"),
                                    annotationdata = file.path(output.data.dir, "Liver_DDS_Annotation.RData")),
                                  envir = new.env())



############################################################
## Array Quality Metrics Report
##  Currently computes AQM within the report
##  Preferably we create the AQM report from a function and just pass the 
##  output folder directory as a variable to the report.
file.copy(system.file("ReportSections/ArrayQualityMetrics.Rmd", package = "RnaSeqAnalysis"),
          "ArrayQualityMetrics.Rmd")
# Array quality metrics report
aqm.report <- rmarkdown::render(file.path(output.dir, "ArrayQualityMetrics.Rmd"),
                                params = list(
                                  echo = FALSE,
                                  project.name = project.name,
                                  dds = file.path(data.subdir, "Liver_DDS.RData"),
                                  int.groups = int.groups,
                                  aqm.dir = "AQM"),
                                envir = new.env())

############################################################
### Perform differential analysis
###
comparisonfile <- system.file("ExampleData/MKMD/LiverAorta_MultiModel_Comparisons.csv", package = "RnaSeqAnalysis")
## EVERYTHING IS ALREADY WRAPPED IN ONE FUNCTION
## output is a list with Models, Results, Comparisons, metadata, etc.
dds.analysis <- DESeqAnalysis(dds, comparisonFile = comparisonfile)
# Note: a lot of default values that you can't set in this function(YET)
#     like p-adjustment method or padj.alpha etc.
# One of the outputs is 'dds.analysis$meta' which holds the results on the
# independent filtering of the p-values that can be used for plotting.

## PLOT RESULTS
## using Glimma MD plots
## Output is dds.analysis with new list dds.analysis$MD.Plots with md-plot file locations
dds.analysis <- PlotDESeqAnalysis(dds.analysis, annotationData = annotationData, 
                                  output.dir = output.dir,
                                  folder = "Glimma")
# adds dds.analysis$MD.Plots - you can skip this step and then the plots will be
# generated when making the report.

# Save analysis output
save(dds.analysis, file = file.path(output.data.dir, "Liver_DDS_Analysis.RData"))

# Copy report file
file.copy(system.file("ReportSections/RnaSeq-DifferentialExpression.Rmd", package = "RnaSeqAnalysis"),
          "RnaSeq-DifferentialExpression.Rmd")

# Differential analysis report
diff.report <- rmarkdown::render("RnaSeq-DifferentialExpression.Rmd",
                                 params = list(
                                   echo = FALSE,
                                   dds.analysis = file.path(output.data.dir, "Liver_DDS_Analysis.RData"),
                                   annotationdata = file.path(output.data.dir, "Liver_DDS_Annotation.RData")),
                                 envir = new.env())

# So is saving the MD-Plot links along with the dds.analysis a good idea?
# It might save space to save them separately but then you'll always have to
# match the results with the plots again. Would you ever save dds.analysis without
# MD plots, and then create the plots separately later?
# The output files are really the .html files for the plots anyway...

## 
## Creating reports from the output
## 
## Copy the .Rmd report sections to project output folder separately?
##  These files often change between versions and this makes sure we use the
##  same version between runs until we delete these files from the project.


#### ONLY RUN IF EVERYTHING ABOVE ALREADY WORKS CORRECTLY ####
# Once you add _site.yml to the report folder , everything you render will be
# affected

# Index homepage for render_site()
#   Need something to test with so don't mind the contents for now
file.copy(system.file("ReportSections/index.Rmd", package = "RnaSeqAnalysis"),
          "index.Rmd")
# Example _site.yml file
file.copy(system.file("ReportSections/_site.yml", package = "RnaSeqAnalysis"),
          "_site.yml")

## 
## Render a website with parameters
##  Found an unmerged Pull Request on RMarkdown and copied the code
##  needed for rendering site with parameters.
##  The parameter and website are defined in _site.yml
# All inputs and outputs are defined in _site.yml
library(rmarkdown)
RnaSeqAnalysis::render_site(envir = new.env())

