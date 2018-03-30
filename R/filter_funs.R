gene_prevalence_filter5   <- function(x){ sum(x>0) > 0.05*length(x) }
