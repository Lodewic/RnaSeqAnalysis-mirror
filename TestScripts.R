library(DESeq2)

source.functions <- sapply(dir("R"), function(x) ifelse(grepl("*.r", tolower(x)), source(file.path("R", x)), ""))

counts.file <- "inst/ExampleData/MKMD/Aorta_Counts.csv"
coldata.file <- "inst/ExampleData/MKMD/Aorta_colData_Endpoints.csv"




dds <- getData(counts.file, coldata.file, species = "Mouse", keytype = "ENSEMBL",
               calc.disps = FALSE, calc.vst = FALSE)

# Setup gene filter
filter.list <- list(
  function(x) gene_filter_mean(x, cutoff = 30),
  function(x) gene_filter_prevalence(x, cutoff = 5, threshold = 0.2))
)

# Filter genes by mean and prevalence
dds <- deseq_gene_filter(dds, flist = filter.list, normalized = TRUE)

# Subset the data
dds_chow <- deseq_sample_subset(dds, subset.list = list(c("Diet.x", "chow")))
dds_hfd <- deseq_sample_subset(dds, subset.list = list(c("Diet.x", "HFD")))


