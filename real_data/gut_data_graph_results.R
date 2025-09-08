#========================
# Gut data 
#========================

library(phyloseq)
library(SpiecEasi)
library(huge)

# data(amgut1.filt)
# amgut1.filt
data(amgut2.filt.phy)

otu_tab <- otu_table(amgut2.filt.phy)
otu_data <- otu_tab@.Data
data_R <- t(otu_data)

n <- dim(data_R)[1]
p <-  dim(data_R)[2]
dim(data_R)
data_R <- huge::huge.npn(data_R)
daatta <- data_R

tax_table(amgut2.filt.phy)[,5]

tax <- tax_table(amgut2.filt.phy)[,4]
tax_data <- tax@.Data
rownames(tax_data) <- 1:138
indices <- list()
for (taxa in tax) {
  indices[[taxa]] <- which(tax_data == taxa)
}

#========================
# both, alpha = p*10/(p*10+n), alpha selected with cc-method
#========================
library(HMFGraph)

set.seed(42)
tulos <- HMFGraph::HMFGraph_GEM(data_R)
tulos$beta
tulos$alpha

permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=T)

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations)

set.seed(42)
tulos_10 <- HMFGraph::HMFGraph_GEM(data_R, alpha =10*p/(10*p+n))

permutations_10 <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos_10, parallel=T, number_of_permutations = 50)
tulos_var_10 <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p)

setEPS()
postscript("Fig7.eps", width = 20, height = 12)  


par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.5,0.05))

par(mar = c(0.01, 0.01, 0.01, 0.01))
gfgf1 <- qgraph::qgraph(tulos_var$adjacency_matrix,diag=F,usePCH=T ,vsize= 3,
               color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                         "purple", "brown", "yellow", "orange"),
               groups = indices, title="A", legend=F,title.cex=4, labels=F
)

par(mar = c(0.01, 0.01, 0.01, 0.01))
gfgf2 <-qgraph::qgraph(tulos_var_10$adjacency_matrix,diag=F,usePCH=T,vsize= 3,
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


tax <- tax_table(amgut2.filt.phy)[,5]
tax_data <- tax@.Data
rownames(tax_data) <- 1:138
indices <- list()
for (taxa in tax) {
  indices[[taxa]] <- which(tax_data == taxa)
}


setEPS()
postscript("FigS16.eps", width = 25, height = 12)  


par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.5,0.05))

par(mar = c(0.01, 0.01, 0.01, 0.01))
gfgf1 <- qgraph::qgraph(tulos_var$adjacency_matrix,diag=F,usePCH=T ,vsize= 3,
                        color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                                  "purple", "brown", "yellow", "green", "white"),
                        groups = indices, title="A", legend=F,title.cex=4, labels=F
)

par(mar = c(0.01, 0.01, 0.01, 0.01))
gfgf2 <-qgraph::qgraph(tulos_var_10$adjacency_matrix,diag=F,usePCH=T,vsize= 3,
                       color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                                 "purple", "brown", "yellow",  "green", "white"),
                       groups = indices,title="B", legend=F,title.cex=4,label.cex=1.5, labels=F
)
par(mar = c(0.01, 0.01, 0.01, 0.01))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                 "purple", "brown", "yellow",  "green" , "white")


indices_names <- c("f__Ruminococcaceae",
                   "f__Bacteroidaceae",
                   "f__Lachnospiraceae",
                   "f__Enterobacteriaceae",
                   "f__Bifidobacteriaceae",
                   "not known",
                   "f__Enterococcaceae",
                   "f__Porphyromonadaceae",
                   "f__Coriobacteriaceae",
                   "f__Desulfovibrionaceae",
                   "f__Veillonellaceae",
                   "other")

legend(x = "top",inset = 0,
       legend = indices_names,ncol=6,border=NA,
       col=rep("black", length(indices_names)), pt.bg=plot_colors, cex=2, xpd = TRUE,pch=21)


dev.off()



tax <- tax_table(amgut2.filt.phy)[,2]
tax_data <- tax@.Data
rownames(tax_data) <- 1:138
indices <- list()
for (taxa in tax) {
  indices[[taxa]] <- which(tax_data == taxa)
}


setEPS()
postscript("FigS17.eps", width = 20, height = 12)  


par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.5,0.05))

par(mar = c(0.01, 0.01, 0.01, 0.01))
gfgf1 <- qgraph::qgraph(tulos_var$adjacency_matrix,diag=F,usePCH=T ,vsize= 3,
                        color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                                  "purple", "brown", "yellow", "orange"),
                        groups = indices, title="A", legend=F,title.cex=4, labels=F
)

par(mar = c(0.01, 0.01, 0.01, 0.01))
gfgf2 <-qgraph::qgraph(tulos_var_10$adjacency_matrix,diag=F,usePCH=T,vsize= 3,
                       color = c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                                 "purple", "brown", "yellow", "orange"),
                       groups = indices,title="B", legend=F,title.cex=4,label.cex=1.5, labels=F
)
par(mar = c(0.01, 0.01, 0.01, 0.01))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c("lightblue", "lightsalmon", "lightgreen", "lightyellow", "lightpink", "blue", "red",
                 "purple", "brown", "yellow", "orange")

indices_names <- c("p__Firmicutes",
                   "p__Bacteroidetes",
                   "p__Proteobacteria",
                   "p__Actinobacteria",
                   "p__Verrucomicrobia",
                   "p__Tenericutes")

legend(x = "top",inset = 0,
       legend = indices_names,ncol=3,border=NA,
       col=rep("black", length(indices_names)), pt.bg=plot_colors, cex=2.2, xpd = TRUE,pch=21)


dev.off()



#==============================================================================================================================
# Here is a demonstration of how the expected number of connections impact the recovered correlation structure
# The result doesn't change significantly even with high number of expected connections.
#==============================================================================================================================
par(mfrow=c(1,1))
par(mfrow=c(4,2))


tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = 0)
qgraph::qgraph(tulos_var$adjacency_matrix)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = round(p/3,0) )
qgraph::qgraph(tulos_var$adjacency_matrix)
round(p/3,0)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = round(p/2,0))
qgraph::qgraph(tulos_var$adjacency_matrix)
round(p/2,0)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
qgraph::qgraph(tulos_var$adjacency_matrix)
p
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*2)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*2
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*5)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*5
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*10)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*10
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*100)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*100
sum(tulos_var$adjacency_matrix)/2


#==============================================================================================================================
# Here is a demonstration of how the target FDR impacts the recovered correlation structure
#==============================================================================================================================
par(mfrow=c(4,2))


tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0)
qgraph::qgraph(tulos_var$adjacency_matrix)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.05)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.1)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.15)
qgraph::qgraph(tulos_var$adjacency_matrix)
p
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.20)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.25)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.30)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.35)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

#==============================================================================================================================
# Here is a demonstration of how the expected number of connections impact the recovered correlation structure
# The result doesn't change significantly even with high number of expected connections.
#==============================================================================================================================
par(mfrow=c(4,2))


tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = 0)
tulos_10_var <- tulos_var
qgraph::qgraph(tulos_var$adjacency_matrix)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = round(p/3,0) )
qgraph::qgraph(tulos_var$adjacency_matrix)
round(p/3,0)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = round(p/2,0))
qgraph::qgraph(tulos_var$adjacency_matrix)
round(p/2,0)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p)
qgraph::qgraph(tulos_var$adjacency_matrix)
p
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p*2)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*2
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p*5)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*5
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p*10)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*10
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos_10,permutations_10, expected_connections = p*100)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*100
sum(tulos_var$adjacency_matrix)/2


#==============================================================================================================================
# Here is a demonstration of how the target FDR impacts the recovered correlation structure
#==============================================================================================================================
par(mfrow=c(4,2))


tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos_10,permutations_10, target_FDR = 0)
qgraph::qgraph(tulos_var$adjacency_matrix)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos_10,permutations_10, target_FDR = 0.05)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos_10,permutations_10, target_FDR = 0.1)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos_10,permutations_10, target_FDR = 0.15)
qgraph::qgraph(tulos_var$adjacency_matrix)
p
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos_10,permutations_10, target_FDR = 0.20)
tulos_var_10 <- tulos_var 
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos_10,permutations_10, target_FDR = 0.25)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos_10,permutations_10, target_FDR = 0.30)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos_10,permutations_10, target_FDR = 0.35)
qgraph::qgraph(tulos_var$adjacency_matrix)

sum(tulos_var$adjacency_matrix)/2
