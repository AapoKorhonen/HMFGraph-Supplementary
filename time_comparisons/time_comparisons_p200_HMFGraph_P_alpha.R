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
# P = 200
#=======================================
#=======================================

set.seed(42)
p <- 200
n <- 2*p
graph_data <- bdgraph.sim( n =n, p = p,graph = "scale-free", vis = F )

alpha <- HMFGraph::alpha_binary_search(graph_data$data)

benchmark_Result200_HMFGraph_P_alpha <- microbenchmark( HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data), number_of_permutations = 50,parallel = T),
                                                  times = times
)

benchmark_Result200_HMFGraph_P_alpha
save(benchmark_Result200_HMFGraph_P_alpha , file="time_comparisons/p_200/benchmark_Result200_HMFGraph_P_alpha.RData")
