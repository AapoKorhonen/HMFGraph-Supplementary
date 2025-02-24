

##########################################################################
# 
# Genereting Data With BDgraph R-package
#
##########################################################################


library(BDgraph)

t1 = 50

n = 35; p = 100

#=========================================================================
# Random N = 35, P = 100
#=========================================================================

random_p_100_n_35_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
random_p_100_n_35_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "random", vis = F, prob = 0.05)
  
  random_p_100_n_35_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  random_p_100_n_35_bdgraph_adjacency[,,i] <-graph_data$K
}

save(random_p_100_n_35_bdgraph_data,file = "simulated_data/bdgraph/random_p_100_n_35_bdgraph_data.RData")
save(random_p_100_n_35_bdgraph_adjacency,file = "simulated_data/bdgraph/random_p_100_n_35_bdgraph_adjacency.RData")


#=========================================================================
# Cluster N = 35, P = 100
#=========================================================================

cluster_p_100_n_35_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
cluster_p_100_n_35_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "cluster", vis = F )
  
  cluster_p_100_n_35_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  cluster_p_100_n_35_bdgraph_adjacency[,,i] <-graph_data$K
}

save(cluster_p_100_n_35_bdgraph_data,file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_data.RData")
save(cluster_p_100_n_35_bdgraph_adjacency,file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_adjacency.RData")


#=========================================================================
# Scale-Free N = 35, P = 100
#=========================================================================


scale_free_p_100_n_35_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
scale_free_p_100_n_35_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "scale-free", vis = F )
  
  scale_free_p_100_n_35_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  scale_free_p_100_n_35_bdgraph_adjacency[,,i] <-graph_data$K
}

save(scale_free_p_100_n_35_bdgraph_data,file = "simulated_data/bdgraph/scale_free_p_100_n_35_bdgraph_data.RData")
save(scale_free_p_100_n_35_bdgraph_adjacency,file = "simulated_data/bdgraph/scale_free_p_100_n_35_bdgraph_adjacency.RData")



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################


n = 75; p = 100

#=========================================================================
# Random N = 75, P = 100
#=========================================================================

random_p_100_n_75_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
random_p_100_n_75_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "random", vis = F, prob = 0.05)
  
  random_p_100_n_75_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  random_p_100_n_75_bdgraph_adjacency[,,i] <-graph_data$K
}

save(random_p_100_n_75_bdgraph_data,file = "simulated_data/bdgraph/random_p_100_n_75_bdgraph_data.RData")
save(random_p_100_n_75_bdgraph_adjacency,file = "simulated_data/bdgraph/random_p_100_n_75_bdgraph_adjacency.RData")


#=========================================================================
# Cluster N = 75, P = 100
#=========================================================================

cluster_p_100_n_75_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
cluster_p_100_n_75_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "cluster", vis = F )
  
  cluster_p_100_n_75_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  cluster_p_100_n_75_bdgraph_adjacency[,,i] <-graph_data$K
}

save(cluster_p_100_n_75_bdgraph_data,file = "simulated_data/bdgraph/cluster_p_100_n_75_bdgraph_data.RData")
save(cluster_p_100_n_75_bdgraph_adjacency,file = "simulated_data/bdgraph/cluster_p_100_n_75_bdgraph_adjacency.RData")


#=========================================================================
# Scale-Free N = 75, P = 100
#=========================================================================


scale_free_p_100_n_75_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
scale_free_p_100_n_75_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "scale-free", vis = F )
  
  scale_free_p_100_n_75_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  scale_free_p_100_n_75_bdgraph_adjacency[,,i] <-graph_data$K
}

save(scale_free_p_100_n_75_bdgraph_data,file = "simulated_data/bdgraph/scale_free_p_100_n_75_bdgraph_data.RData")
save(scale_free_p_100_n_75_bdgraph_adjacency,file = "simulated_data/bdgraph/scale_free_p_100_n_75_bdgraph_adjacency.RData")



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################



n = 150; p = 100

#=========================================================================
# Random N = 150, P = 100
#=========================================================================

random_p_100_n_150_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
random_p_100_n_150_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "random", vis = F, prob = 0.05)
  
  random_p_100_n_150_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  random_p_100_n_150_bdgraph_adjacency[,,i] <-graph_data$K
}

save(random_p_100_n_150_bdgraph_data,file = "simulated_data/bdgraph/random_p_100_n_150_bdgraph_data.RData")
save(random_p_100_n_150_bdgraph_adjacency,file = "simulated_data/bdgraph/random_p_100_n_150_bdgraph_adjacency.RData")


#=========================================================================
# Cluster N = 150, P = 100
#=========================================================================

cluster_p_100_n_150_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
cluster_p_100_n_150_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "cluster", vis = F )
  
  cluster_p_100_n_150_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  cluster_p_100_n_150_bdgraph_adjacency[,,i] <-graph_data$K
}

save(cluster_p_100_n_150_bdgraph_data,file = "simulated_data/bdgraph/cluster_p_100_n_150_bdgraph_data.RData")
save(cluster_p_100_n_150_bdgraph_adjacency,file = "simulated_data/bdgraph/cluster_p_100_n_150_bdgraph_adjacency.RData")


#=========================================================================
# Scale-Free N = 150, P = 100
#=========================================================================

scale_free_p_100_n_150_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
scale_free_p_100_n_150_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "scale-free", vis = F )
  
  scale_free_p_100_n_150_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  scale_free_p_100_n_150_bdgraph_adjacency[,,i] <-graph_data$K
}

save(scale_free_p_100_n_150_bdgraph_data,file = "simulated_data/bdgraph/scale_free_p_100_n_150_bdgraph_data.RData")
save(scale_free_p_100_n_150_bdgraph_adjacency,file = "simulated_data/bdgraph/scale_free_p_100_n_150_bdgraph_adjacency.RData")



##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################



n = 300; p = 100

#=========================================================================
# Random N = 300, P = 100
#=========================================================================

random_p_100_n_300_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
random_p_100_n_300_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "random", vis = F, prob = 0.02)
  
  random_p_100_n_300_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  random_p_100_n_300_bdgraph_adjacency[,,i] <-graph_data$K
}

save(random_p_100_n_300_bdgraph_data,file = "simulated_data/bdgraph/random_p_100_n_300_bdgraph_data.RData")
save(random_p_100_n_300_bdgraph_adjacency,file = "simulated_data/bdgraph/random_p_100_n_300_bdgraph_adjacency.RData")


#=========================================================================
# Cluster N = 300, P = 100
#=========================================================================

cluster_p_100_n_300_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
cluster_p_100_n_300_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "cluster", vis = F )
  
  cluster_p_100_n_300_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  cluster_p_100_n_300_bdgraph_adjacency[,,i] <-graph_data$K
}

save(cluster_p_100_n_300_bdgraph_data,file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_data.RData")
save(cluster_p_100_n_300_bdgraph_adjacency,file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_adjacency.RData")


#=========================================================================
# Scale-Free N = 300, P = 100
#=========================================================================

scale_free_p_100_n_300_bdgraph_data<- array( 0, dim = c( n ,p , t1 ) )
scale_free_p_100_n_300_bdgraph_adjacency<- array( 0, dim = c( p ,p , t1 ) )


for(i in 1:t1){
  set.seed(i)
  graph_data <- bdgraph.sim( n =n, p = p,graph = "scale-free", vis = F )
  
  scale_free_p_100_n_300_bdgraph_data[,,i] <-graph_data$data
  
  graph_data$K <- round(graph_data$K, 5)
  
  graph_data$K[abs(graph_data$K) > 0] <- 1
  
  diag(graph_data$K) <- 0
  
  scale_free_p_100_n_300_bdgraph_adjacency[,,i] <-graph_data$K
}

save(scale_free_p_100_n_300_bdgraph_data,file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_data.RData")
save(scale_free_p_100_n_300_bdgraph_adjacency,file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_adjacency.RData")
