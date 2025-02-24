########################################################################################
# This file is used to calculate the average clustering coefficent (ACC) for the 
# all simulated data
########################################################################################

library(igraph)

load(file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_75_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_150_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_adjacency.RData")




cc <- c()
for(i in 1:50){
  cc[i] <- igraph::transitivity(igraph::graph_from_adjacency_matrix(cluster_p_100_n_300_bdgraph_adjacency[,,i], mode = "undirected"), type = "average")
}

for(i in 1:50){
  cc[i+50] <- igraph::transitivity(igraph::graph_from_adjacency_matrix(cluster_p_100_n_150_bdgraph_adjacency[,,i], mode = "undirected"), type = "average")
}

for(i in 1:50){
  cc[i+100] <- igraph::transitivity(igraph::graph_from_adjacency_matrix(cluster_p_100_n_75_bdgraph_adjacency[,,i], mode = "undirected"), type = "average")
}

for(i in 1:50){
  cc[i+150] <- igraph::transitivity(igraph::graph_from_adjacency_matrix(cluster_p_100_n_75_bdgraph_adjacency[,,i], mode = "undirected"), type = "average")
}

mean(cc)

# The mean of ACC for BDgraph is 0.1951322


load(file = "simulated_data/huge/cluster_p_100_n_35_huge_adjacency.RData")
load(file = "simulated_data/huge/cluster_p_100_n_75_huge_adjacency.RData")
load(file = "simulated_data/huge/cluster_p_100_n_150_huge_adjacency.RData")
load(file = "simulated_data/huge/cluster_p_100_n_300_huge_adjacency.RData")


cc <- c()
for(i in 1:50){
  cc[i] <- igraph::transitivity(igraph::graph_from_adjacency_matrix(cluster_p_100_n_300_huge_adjacency[,,i], mode = "undirected"), type = "average")
}

for(i in 1:50){
  cc[i+50] <- igraph::transitivity(igraph::graph_from_adjacency_matrix(cluster_p_100_n_150_huge_adjacency[,,i], mode = "undirected"), type = "average")
}

for(i in 1:50){
  cc[i+100] <- igraph::transitivity(igraph::graph_from_adjacency_matrix(cluster_p_100_n_75_huge_adjacency[,,i], mode = "undirected"), type = "average")
}

for(i in 1:50){
  cc[i+150] <- igraph::transitivity(igraph::graph_from_adjacency_matrix(cluster_p_100_n_75_huge_adjacency[,,i], mode = "undirected"), type = "average")
}

mean(cc)

# The mean of ACC for huge is 0.303394