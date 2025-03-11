#========================
# Gut data 
#========================

library(phyloseq)
library("SpiecEasi")
library(huge)

data(amgut2.filt.phy)
otu_tab <- otu_table(amgut2.filt.phy)
otu_data <- otu_tab@.Data
data_R <- t(otu_data)

#data_norm <- SpiecEasi:::.spiec.easi.norm(data_R) # different normalization
#data_R <- data_norm
n <- 296
p <- 138
dim(data_R)
data_R <- huge::huge.npn(data_R)
daatta <- data_R

tax <- tax_table(amgut2.filt.phy)[,4]
tax_data <- tax@.Data
rownames(tax_data) <- 1:138
indices <- list()
for (taxa in tax) {
  indices[[taxa]] <- which(tax_data == taxa)
}


#========================
# alpha = p*10/(p*10+n)
#========================
library(HMFGraph)

set.seed(42)
tulos_10 <- HMFGraph::HMFGraph_GEM(data_R, alpha =p*10/(p*10+n))
tulos_10$beta
tulos_10$alpha

permutations_10 <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos_10, parallel=T, number_of_permutations = 50)
tulos_var_10 <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p)

setEPS()
postscript("gut_data_large_alpha.eps", width = 13, height = 10)  

qgraph::qgraph(tulos_var_10$adjacency_matrix,diag=F,usePCH=T,vsize= 2,
       color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                 "purple", "brown", "yellow", "orange"),
       groups = indices
)
sum(tulos_var_10$adjacency_matrix)/2

dev.off()

#========================
# alpha = CC
#========================


set.seed(42)
tulos <- HMFGraph::HMFGraph_GEM(data_R, epsilon1 = 0.001,epsilon2=0.001)
tulos_10$beta
tulos_10$alpha

max(eigen(tulos$omega)$values)/min(eigen(tulos$omega)$values)


permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=T)

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations)

setEPS()
postscript("gut_data_optimal_alpha.eps", width = 13, height = 10)  


qgraph::qgraph(tulos_var$adjacency_matrix,diag=F,usePCH=T,vsize= 2,
       color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                 "purple", "brown", "yellow", "orange"),
       groups = indices
)
sum(tulos_var$adjacency_matrix)/2

dev.off()