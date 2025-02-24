#========================
# Riboflavin data 
#========================
data("riboflavin")
dim(riboflavin)

data_R <- read.csv("data/riboflavinv100.csv", header=T)
dim(data_R)

rownames(data_R)
data_R <- t(data_R[,-1])

data_R <- huge::huge.npn(data_R)

n <- dim(data_R)[1]
p <- dim(data_R)[2]


#========================
# alpha = CC
#========================
set.seed(42)

tulos <- HMFGraph::HMFGraph_GEM(data_R)

max(eigen(tulos$omega)$values)/min(eigen(tulos$omega)$values) # Condition number

permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=T)

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)


setEPS()
postscript("riboflavinv100_network.eps", width = 10, height = 10)  


qgraph::qgraph(tulos_var$adjacency_matrix)


dev.off()