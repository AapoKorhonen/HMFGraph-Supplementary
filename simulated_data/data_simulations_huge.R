
##########################################################################
# 
# Generating Data With Huge R-package
#
##########################################################################

library(huge)

t1 = 50

n = 35; p = 100

#=========================================================================
# Cluster N = 35, P = 100
#=========================================================================

cluster_p_100_n_35_huge_data <- array( 0, dim = c( n ,p , t1 ) )
cluster_p_100_n_35_huge_adjacency <- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data  <- huge.generator(n = n, d =p, vis = F, graph ="cluster")
  
  cluster_p_100_n_35_huge_data[,,i] <-graph_data$data
  
  graph_data$omega <- round(graph_data$omega, 5)
  
  graph_data$omega[abs(graph_data$omega) > 0] <- 1
  
  diag(graph_data$omega) <- 0
  
  cluster_p_100_n_35_huge_adjacency[,,i] <-graph_data$omega
}

save(cluster_p_100_n_35_huge_data,file = "simulated_data/huge/cluster_p_100_n_35_huge_data.RData")
save(cluster_p_100_n_35_huge_adjacency ,file = "simulated_data/huge/cluster_p_100_n_35_huge_adjacency.RData")


#=========================================================================
# Scale-Free N = 35, P = 100
#=========================================================================


scale_free_p_100_n_35_huge_data <- array( 0, dim = c( n ,p , t1 ) )
scale_free_p_100_n_35_huge_adjacency <- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- huge.generator(n = n, d =p, vis = F, graph ="scale-free")
  
  scale_free_p_100_n_35_huge_data[,,i] <-graph_data$data
  
  graph_data$omega <- round(graph_data$omega, 5)
  
  graph_data$omega[abs(graph_data$omega) > 0] <- 1
  
  diag(graph_data$omega) <- 0
  
  scale_free_p_100_n_35_huge_adjacency[,,i] <-graph_data$omega
}

save(scale_free_p_100_n_35_huge_data ,file = "simulated_data/huge/scale_free_p_100_n_35_huge_data.RData")
save(scale_free_p_100_n_35_huge_adjacency ,file = "simulated_data/huge/scale_free_p_100_n_35_huge_adjacency.RData")



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################



n = 75; p = 100

#=========================================================================
# Cluster N = 75, P = 100
#=========================================================================

cluster_p_100_n_75_huge_data <- array( 0, dim = c( n ,p , t1 ) )
cluster_p_100_n_75_huge_adjacency <- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data<- huge.generator(n = n, d =p, vis = F, graph ="cluster")
  
  cluster_p_100_n_75_huge_data[,,i] <-graph_data$data
  
  graph_data$omega <- round(graph_data$omega, 5)
  
  graph_data$omega[abs(graph_data$omega) > 0] <- 1
  
  diag(graph_data$omega) <- 0
  
  cluster_p_100_n_75_huge_adjacency[,,i] <-graph_data$omega
}

save(cluster_p_100_n_75_huge_data ,file = "simulated_data/huge/cluster_p_100_n_75_huge_data.RData")
save(cluster_p_100_n_75_huge_adjacency ,file = "simulated_data/huge/cluster_p_100_n_75_huge_adjacency.RData")


#=========================================================================
# Scale-Free N = 75, P = 100
#=========================================================================


scale_free_p_100_n_75_huge_data <- array( 0, dim = c( n ,p , t1 ) )
scale_free_p_100_n_75_huge_adjacency <- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- huge.generator(n = n, d =p, vis = F, graph ="scale-free")
  
  scale_free_p_100_n_75_huge_data[,,i] <-graph_data$data
  
  graph_data$omega <- round(graph_data$omega, 5)
  
  graph_data$omega[abs(graph_data$omega) > 0] <- 1
  
  diag(graph_data$omega) <- 0
  
  scale_free_p_100_n_75_huge_adjacency[,,i] <-graph_data$omega
}

save(scale_free_p_100_n_75_huge_data ,file = "simulated_data/huge/scale_free_p_100_n_75_huge_data.RData")
save(scale_free_p_100_n_75_huge_adjacency ,file = "simulated_data/huge/scale_free_p_100_n_75_huge_adjacency.RData")



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################



n = 150; p = 100


#=========================================================================
# Cluster N = 150, P = 100
#=========================================================================

cluster_p_100_n_150_huge_data <- array( 0, dim = c( n ,p , t1 ) )
cluster_p_100_n_150_huge_adjacency <- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- huge.generator(n = n, d =p, vis = F, graph ="cluster")
  
  cluster_p_100_n_150_huge_data[,,i] <-graph_data$data
  
  graph_data$omega <- round(graph_data$omega, 5)
  
  graph_data$omega[abs(graph_data$omega) > 0] <- 1
  
  diag(graph_data$omega) <- 0
  
  cluster_p_100_n_150_huge_adjacency[,,i] <-graph_data$omega
}

save(cluster_p_100_n_150_huge_data ,file = "simulated_data/huge/cluster_p_100_n_150_huge_data.RData")
save(cluster_p_100_n_150_huge_adjacency ,file = "simulated_data/huge/cluster_p_100_n_150_huge_adjacency.RData")


#=========================================================================
# Scale-Free N = 150, P = 100
#=========================================================================

scale_free_p_100_n_150_huge_data <- array( 0, dim = c( n ,p , t1 ) )
scale_free_p_100_n_150_huge_adjacency <- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- huge.generator(n = n, d =p, vis = F, graph ="scale-free")
  
  scale_free_p_100_n_150_huge_data[,,i] <-graph_data$data
  
  graph_data$omega <- round(graph_data$omega, 5)
  
  graph_data$omega[abs(graph_data$omega) > 0] <- 1
  
  diag(graph_data$omega) <- 0
  
  scale_free_p_100_n_150_huge_adjacency[,,i] <-graph_data$omega
}

save(scale_free_p_100_n_150_huge_data ,file = "simulated_data/huge/scale_free_p_100_n_150_huge_data.RData")
save(scale_free_p_100_n_150_huge_adjacency ,file = "simulated_data/huge/scale_free_p_100_n_150_huge_adjacency.RData")



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################



n = 300; p = 100


#=========================================================================
# Cluster N = 300, P = 100
#=========================================================================

cluster_p_100_n_300_huge_data <- array( 0, dim = c( n ,p , t1 ) )
cluster_p_100_n_300_huge_adjacency <- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- huge.generator(n = n, d =p, vis = F, graph ="cluster")
  
  cluster_p_100_n_300_huge_data[,,i] <-graph_data$data
  
  graph_data$omega <- round(graph_data$omega, 5)
  
  graph_data$omega[abs(graph_data$omega) > 0] <- 1
  
  diag(graph_data$omega) <- 0
  
  cluster_p_100_n_300_huge_adjacency[,,i] <-graph_data$omega
}

save(cluster_p_100_n_300_huge_data ,file = "simulated_data/huge/cluster_p_100_n_300_huge_data.RData")
save(cluster_p_100_n_300_huge_adjacency ,file = "simulated_data/huge/cluster_p_100_n_300_huge_adjacency.RData")


#=========================================================================
# Scale-Free N = 300, P = 100
#=========================================================================

scale_free_p_100_n_300_huge_data <- array( 0, dim = c( n ,p , t1 ) )
scale_free_p_100_n_300_huge_adjacency <- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data  <- huge.generator(n = n, d =p, vis = F, graph ="scale-free")
  
  scale_free_p_100_n_300_huge_data[,,i] <-graph_data$data
  
  graph_data$omega <- round(graph_data$omega, 5)
  
  graph_data$omega[abs(graph_data$omega) > 0] <- 1
  
  diag(graph_data$omega) <- 0
  
  scale_free_p_100_n_300_huge_adjacency[,,i] <-graph_data$omega
}

save(scale_free_p_100_n_300_huge_data ,file = "simulated_data/huge/scale_free_p_100_n_300_huge_data.RData")
save(scale_free_p_100_n_300_huge_adjacency ,file = "simulated_data/huge/scale_free_p_100_n_300_huge_adjacency.RData")


