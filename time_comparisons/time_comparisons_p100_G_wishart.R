library(microbenchmark)
library(HMFGraph)
library(BDgraph)
library(BGGM)
library(parallel)
library(doSNOW)
library(foreach)
library(progress)

###############################
# Tests run with:
# CPU: Intel Core i7-11700F
# RAM: 32 GB
###############################

times <- 5

#=======================================
#=======================================
# P = 100
#=======================================
#=======================================

set.seed(42)
p <- 100
n <- 2*p
graph_data <- bdgraph.sim( n =n, p = p,graph = "scale-free", vis = F )

alpha <- HMFGraph::alpha_binary_search(graph_data$data)

benchmark_Result100_G_wishart <- microbenchmark(bdgraph( data =graph_data$data,n=n,verbose = FALSE , iter = 5000, burnin =50),
                times = times
)

benchmark_Result100_G_wishart
save(benchmark_Result100_G_wishart , file="time_comparisons/p_100/benchmark_Result100_G_wishart.RData")
