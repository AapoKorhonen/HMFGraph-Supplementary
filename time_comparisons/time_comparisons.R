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

benchmark_Result100 <- microbenchmark( HMFGraph_GEM(graph_data$data, alpha = alpha), 
                HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data, alpha = alpha), number_of_permutations = 50,parallel = T),
                HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data), number_of_permutations = 50,parallel = T),
                HMFGraph_gibbs_sampler(graph_data$data, alpha = alpha ,iters = 5000, burn_in = 50),
                bdgraph( data =graph_data$data,n=n,verbose = FALSE , iter = 5000, burnin =50),
                BGGM::estimate(graph_data$data, type = "continuous",
                               iter = 5000),
                times = 5
)

benchmark_Result100
save(benchmark_Result100 , file="time_comparisons/p_100/benchmark_Result100.RData")

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

benchmark_Result200 <-microbenchmark( HMFGraph_GEM(graph_data$data, alpha = alpha  ), 
                HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data, alpha = alpha  ), number_of_permutations = 50,parallel = T),
                HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data), number_of_permutations = 50,parallel = T),
                HMFGraph_gibbs_sampler(graph_data$data, alpha = alpha ,iters = 5000, burn_in = 50),
                bdgraph( data =graph_data$data,n=n,verbose = FALSE , iter = 5000, burnin =50),
                BGGM::estimate(graph_data$data, type = "continuous",
                               iter = 5000),
                times = 5
)

benchmark_Result200
save(benchmark_Result200 , file="time_comparisons/p_200/benchmark_Result200.RData")


#=======================================
#=======================================
# P = 300
#=======================================
#=======================================


set.seed(42)
p <- 300
n <- 2*p
graph_data <- bdgraph.sim( n =n, p = p,graph = "scale-free", vis = F )

alpha <- HMFGraph::alpha_binary_search(graph_data$data)

benchmark_Result300 <-microbenchmark( HMFGraph_GEM(graph_data$data, alpha = alpha), 
                HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data, alpha = alpha), number_of_permutations = 50,parallel = T),
                HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data), number_of_permutations = 50,parallel = T),
                HMFGraph_gibbs_sampler(graph_data$data, alpha = alpha,iters = 5000, burn_in = 50),
                bdgraph( data =graph_data$data,n=n,verbose = FALSE , iter = 5000, burnin =50),
                BGGM::estimate(graph_data$data, type = "continuous",
                               iter = 5000),
                times = 5
)

benchmark_Result300
save(benchmark_Result300 , file="time_comparisons/p_300/benchmark_Result300.RData")


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

benchmark_Result400 <-microbenchmark( HMFGraph_GEM(graph_data$data, alpha = alpha), 
                HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data, alpha = alpha), number_of_permutations = 50,parallel = T),
                HMFGraph_GEM_permutations(graph_data$data, HMFGraph_GEM(graph_data$data), number_of_permutations = 50,parallel = T),
                HMFGraph_gibbs_sampler(graph_data$data, alpha =  alpha,iters = 5000, burn_in = 50),
                bdgraph( data =graph_data$data,n=n,verbose = FALSE , iter = 5000, burnin =50),
                BGGM::estimate(graph_data$data, type = "continuous",
                               iter = 5000),
                times = 5
)

benchmark_Result400
save(benchmark_Result400 , file="time_comparisons/p_400/benchmark_Result400.RData")
