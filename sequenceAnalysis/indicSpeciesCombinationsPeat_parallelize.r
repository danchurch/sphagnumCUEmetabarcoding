whichCluster = commandArgs(trailingOnly=TRUE)
library(indicspecies)
library(phyloseq)
library(vegan)

print(paste0("indicSpeciesCombinationsPeat",whichCluster,".rda"))

load("ps007_cleaned_sqrt_prop.rda")
tax <- tax_table(ps007_cleaned_sqrt_prop)
otutab <- as.data.frame(otu_table(ps007_cleaned_sqrt_prop))
envData <- sample_data(ps007_cleaned_sqrt_prop)
gr=envData$cluster

sc <- indicators(otutab, gr, group=whichCluster,
                    control = how(nperm=999)) 

saveRDS(sc, file=paste0("indicSpeciesCombinationsPeat",whichCluster,".rda"))
