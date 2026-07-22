library(indicspecies)
library(phyloseq)
library(vegan)

load("ps007_cleaned_sqrt_prop.rda")
tax <- tax_table(ps007_cleaned_sqrt_prop)
otutab <- as.data.frame(otu_table(ps007_cleaned_sqrt_prop))
envData <- sample_data(ps007_cleaned_sqrt_prop)
gr=envData$cluster

sc.list <- list()
for (i in levels(envData$cluster)){
  sc.i <- indicators(otutab, gr, group=i,
                    control = how(nperm=999)) 
}
save(sc.list, file="indicSpeciesCombinationsPeat.rda")
