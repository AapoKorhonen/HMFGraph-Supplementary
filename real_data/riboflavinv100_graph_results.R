#========================
# Riboflavin data 
#========================
library("hdi")
data("riboflavin")
dim(riboflavin)
dim(riboflavin$x)

vars <- var(riboflavin$x)
vars <- diag((vars))
vars_ord <- order(vars,decreasing = T )

data_R <- riboflavin$x[,c(sort(vars_ord[c(1:100)],decreasing = F )) ]

dim(data_R)

data_R <- huge::huge.npn(data_R)

n <- dim(data_R)[1]
p <- dim(data_R)[2]


#========================
# alpha = CC
#========================
set.seed(42)

tulos <- HMFGraph::HMFGraph_GEM(data_R,print_binary_search = T)

max(eigen(tulos$omega)$values)/min(eigen(tulos$omega)$values) # Condition number

permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=T)

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)


par(mfrow=c(1,1))

setEPS()
postscript("riboflavinv100_network.eps", width = 10, height = 10)  


qgraph::qgraph(tulos_var$adjacency_matrix)


dev.off()


tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
qgraph::qgraph(tulos_var$adjacency_matrix)



#==============================================================================================================================
# Here is a demonstration of how the expected number of connections impact the recovered correlation structure
# The result doesn't change significantly even with high number of expected connections.
#==============================================================================================================================
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
