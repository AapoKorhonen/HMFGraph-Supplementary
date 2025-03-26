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
# P = 400
#=======================================
#=======================================

set.seed(42)
p <- 400
n <- 2*p
graph_data <- bdgraph.sim( n =n, p = p,graph = "scale-free", vis = F )

alpha <- HMFGraph::alpha_binary_search(graph_data$data)

benchmark_Result400_BGGM <- microbenchmark(BGGM::estimate(graph_data$data, type = "continuous",
                                                          iter = 5000),
                                           times = times
)

benchmark_Result400_BGGM
save(benchmark_Result400_BGGM , file="time_comparisons/p_400/benchmark_Result400_BGGM.RData")
