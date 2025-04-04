#========================
# Gut data 
#========================

library(phyloseq)
library(SpiecEasi)
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

# 
# #========================
# # alpha = p*10/(p*10+n)
# #========================
# library(HMFGraph)
# 
# set.seed(42)
# tulos_10 <- HMFGraph::HMFGraph_GEM(data_R, alpha =p*10/(p*10+n))
# tulos_10$beta
# tulos_10$alpha
# 
# permutations_10 <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos_10, parallel=T, number_of_permutations = 50)
# tulos_var_10 <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p)
# 
# setEPS()
# postscript("gut_data_large_alpha.eps", width = 13, height = 10)  
# 
# qgraph::qgraph(tulos_var_10$adjacency_matrix,diag=F,usePCH=T,vsize= 2,
#        color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
#                  "purple", "brown", "yellow", "orange"),
#        groups = indices
# )
# sum(tulos_var_10$adjacency_matrix)/2
# 
# dev.off()
# 
# #========================
# # alpha = CC
# #========================
# 
# 
# set.seed(42)
# tulos <- HMFGraph::HMFGraph_GEM(data_R)
# tulos$beta
# tulos$alpha
# 
# max(eigen(tulos$omega)$values)/min(eigen(tulos$omega)$values)
# 
# 
# permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=T)
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations)
# 
# setEPS()
# postscript("gut_data_optimal_alpha.eps", width = 13, height = 10)  
# 
# 
# qgraph::qgraph(tulos_var$adjacency_matrix,diag=F,usePCH=T,vsize= 2,
#        color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
#                  "purple", "brown", "yellow", "orange"),
#        groups = indices
# )
# sum(tulos_var$adjacency_matrix)/2
# 
# dev.off()
# 



#========================
# both, alpha = p*10/(p*10+n), alpha selected with cc-method
#========================
library(HMFGraph)

set.seed(42)
tulos <- HMFGraph::HMFGraph_GEM(data_R)
tulos$beta
tulos$alpha

max(eigen(tulos$omega)$values)/min(eigen(tulos$omega)$values)


permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=T)

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations)



set.seed(42)
tulos_10 <- HMFGraph::HMFGraph_GEM(data_R, alpha =p*10/(p*10+n))
tulos_10$beta
tulos_10$alpha

permutations_10 <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos_10, parallel=T, number_of_permutations = 50)
tulos_var_10 <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p)

set.seed(42)
tulos <- HMFGraph::HMFGraph_GEM(data_R)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=T)
tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations)

setEPS()
postscript("gut_data_both.eps", width = 20, height = 12)  


par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.5,0.05))

par(mar = c(0.01, 0.01, 0.01, 0.01))
qgraph::qgraph(tulos_var$adjacency_matrix,diag=F,usePCH=T,vsize= 3,
               color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                         "purple", "brown", "yellow", "orange"),
               groups = indices, title="A", legend=F,title.cex=4, labels=F
)


par(mar = c(0.01, 0.01, 0.01, 0.01))
qgraph::qgraph(tulos_var_10$adjacency_matrix,diag=F,usePCH=T,vsize= 3,
               color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                         "purple", "brown", "yellow", "orange"),
               groups = indices,title="B", legend=F,title.cex=4,label.cex=1.5, labels=F
)
par(mar = c(0.01, 0.01, 0.01, 0.01))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                 "purple", "brown", "yellow", "orange")

indices_names <- c("o__Clostridiales",
                   "o__Bacteroidales",
                   "o__Enterobacteriales",
                   "o__Bifidobacteriales",
                   "o__Lactobacillales",
                   "o__Coriobacteriales",
                   "o__Oceanospirillales",
                   "o__Verrucomicrobiales",
                   "o__Burkholderiales",
                   "o__Erysipelotrichales")
legend(x = "top",inset = 0,
       legend = indices_names,ncol=5,border=NA,
       col=rep("black", length(indices_names)), pt.bg=plot_colors, cex=2.2, xpd = TRUE,pch=21)




dev.off()



# #==============================================================================================================================
# # Here is a demonstration of how the expected number of connections impact the recovered correlation structure
# # The result doesn't change significantly even with high number of expected connections.
# #==============================================================================================================================
# par(mfrow=c(4,2))
# 
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = 0)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = round(p/3,0) )
# qgraph::qgraph(tulos_var$adjacency_matrix)
# round(p/3,0)
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = round(p/2,0))
# qgraph::qgraph(tulos_var$adjacency_matrix)
# round(p/2,0)
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# p
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p*2)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# p*2
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p*5)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# p*5
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p*10)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# p*10
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p*100)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# p*100
# sum(tulos_var$adjacency_matrix)/2
# 
# 
# 
# 
# 
# 
# #==============================================================================================================================
# # Here is a demonstration of how the expected number of connections impact the recovered correlation structure
# # The result doesn't change significantly even with high number of expected connections.
# #==============================================================================================================================
# par(mfrow=c(4,2))
# 
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = 0)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = round(p/3,0) )
# qgraph::qgraph(tulos_var$adjacency_matrix)
# round(p/3,0)
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = round(p/2,0))
# qgraph::qgraph(tulos_var$adjacency_matrix)
# round(p/2,0)
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# p
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*2)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# p*2
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*5)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# p*5
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*10)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# p*10
# sum(tulos_var$adjacency_matrix)/2
# 
# tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*100)
# qgraph::qgraph(tulos_var$adjacency_matrix)
# p*100
# sum(tulos_var$adjacency_matrix)/2

