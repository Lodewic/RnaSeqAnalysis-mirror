library(DESeq2)
library(tidyverse)

# source.functions <- sapply(dir("R"), function(x) ifelse(grepl("*.r", tolower(x)), source(file.path("R", x)), ""))

library(RnaSeqAnalysis)
counts.file <- "inst/ExampleData/MKMD/Liver_Counts.csv"
coldata.file <- "inst/ExampleData/MKMD/Liver_colData_Endpoints.csv"


dds <- getData(counts.file, coldata.file, species = "Mouse", keytype = "ENSEMBL",
               calc.disps = FALSE, calc.vst = FALSE)

# Setup gene filter
filter.list <- list(
  function(x) gene_filter_mean(x, cutoff = 30),
  function(x) gene_filter_prevalence(x, cutoff = 5, threshold = 0.4))

# Filter genes by mean and prevalence
dds <- deseq_gene_filter(dds, flist = filter.list, normalized = TRUE)

# Subset only relevant timepoints (those with measured endpoints)
dds <- deseq_sample_subset(dds, subset.list = list(c("TimeNum", 12, 18, 24)))

# Plot MDS with all currently filtered genes
# Calculate VST after filtering genes
mcols(dds)$vst <- varianceStabilizingTransformation(dds)

# Plot MDS
dds_mds <- plot_deseq_mds(mcols(dds)$vst, c("Diet.x", "TimeNum"), html = "MDS Without highest variance genes")

# Subset the data
dds_chow <- deseq_sample_subset(dds, subset.list = list(c("Diet.x", "chow")), verbose = TRUE)
dds_hfd <- deseq_sample_subset(dds, subset.list = list(c("Diet.x", "HFD")))

# Plot highest variance genes against lesion area for HFD
#   Highest variance only based on the HFD subset
hfd_gene_variance <- rowVars(counts(dds, normalized = TRUE))
hfd_gene_mean <- rowMeans(counts(dds, normalized = TRUE))
topN_var <- 5
genes_high_Variance <- rownames(dds_hfd)[order(hfd_gene_variance)[1:topN_var]]

#
colData_HighestGenes <- cbind(
  as.data.frame(colData(dds)),
  t(as.data.frame(counts(dds))[genes_high_Variance,]))

GGally::ggpairs(colData_HighestGenes %>% filter(Diet.x == "HFD") %>% 
                  select(Diet.x, Time.x, Lesion.area, genes_high_Variance), columns = 3:(3 + topN_var),
                aes(colour=Time.x))

LesionArea_Models<- colData_HighestGenes %>% filter(Diet.x == "HFD") %>% 
  select(Diet.x, Time.x, Lesion.area, genes_high_Variance) %>%
  gather(Gene, Expression, starts_with("ENSMUS")) %>%
  group_by(Gene) %>%
  nest() %>%
  mutate(LinearModel = map(data, ~lm(Lesion.area ~ Expression + Time.x, data = .)),
         ModelStats = map(LinearModel, broom::tidy)) %>%
  unnest(ModelStats)

LesionArea_Models %>% ggplot(aes(x = Gene, y = -log(p.value, 10))) + 
  geom_bar(aes(color = term), stat = "identity", position = "dodge") + 
  geom_hline(yintercept = -log(0.05, 10))

# Remove highest variance genes
#   We have some extremely high variance genes
#   Remove genes with variance greater than 99.9% percentile of gene variances
dds_RemovedHighVariance <- dds[rowVars(counts(dds, normalized = TRUE)) < quantile(rowVars(counts(dds, normalized = TRUE)), 0.9),]

# Calculate VST after filtering genes
mcols(dds_RemovedHighVariance)$vst <- varianceStabilizingTransformation(dds_RemovedHighVariance)
#
dds_mds_RemovedHighVariance <- plot_deseq_mds(mcols(dds_RemovedHighVariance)$vst, c("Diet.x", "TimeNum"), html = "MDS Without highest variance genes")
                