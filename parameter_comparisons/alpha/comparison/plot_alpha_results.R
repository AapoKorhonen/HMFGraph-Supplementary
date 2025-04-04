
################################################################################################################################################################
#==============================================================================================================================================================
# This file is for plotting out the results, bdgraph and huge, cluster and scale_free, with multiple beta values
#==============================================================================================================================================================
################################################################################################################################################################

source("functions/functions_for_result_handeling.R")
library(RColorBrewer)

colors <- brewer.pal(5, "Dark2")  


#===============================================================================
# Loading all results
#===============================================================================

load(file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_75_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_150_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_adjacency.RData")

true_files_bC <- list(cluster_p_100_n_35_bdgraph_adjacency,
                   cluster_p_100_n_75_bdgraph_adjacency,
                   cluster_p_100_n_150_bdgraph_adjacency,
                   cluster_p_100_n_300_bdgraph_adjacency)
#===============================================================================

# beta = 0.9

load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n300_bdgraph_data.RData")

results_HMF_Z_CC_bC <- list(results_HMF_Z_CC_cluster_p100_n35_bdgraph_data,
                         results_HMF_Z_CC_cluster_p100_n75_bdgraph_data,
                         results_HMF_Z_CC_cluster_p100_n150_bdgraph_data,
                         results_HMF_Z_CC_cluster_p100_n300_bdgraph_data)

load(file="results/bdgraph/results_HMF_Z_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_cluster_p100_n300_bdgraph_data.RData")

results_HMF_Z_a10_bC <- list(results_HMF_Z_cluster_p100_n35_bdgraph_data,
                            results_HMF_Z_cluster_p100_n75_bdgraph_data,
                            results_HMF_Z_cluster_p100_n150_bdgraph_data,
                            results_HMF_Z_cluster_p100_n300_bdgraph_data)

load(file="results/bdgraph/results_HMF_Z2x_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z2x_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z2x_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z2x_cluster_p100_n300_bdgraph_data.RData")

results_HMF_Z_a2_bC <- list(results_HMF_Z2x_cluster_p100_n35_bdgraph_data,
                          results_HMF_Z2x_cluster_p100_n75_bdgraph_data,
                          results_HMF_Z2x_cluster_p100_n150_bdgraph_data,
                          results_HMF_Z2x_cluster_p100_n300_bdgraph_data)


load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_cluster_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_cluster_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_cluster_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_cluster_p100_n300_bdgraph_data.RData")

results_HMF_Z_a090_bC <- list(results_HMF_Z_a090_cluster_p100_n35_bdgraph_data,
                           results_HMF_Z_a090_cluster_p100_n75_bdgraph_data,
                           results_HMF_Z_a090_cluster_p100_n150_bdgraph_data,
                           results_HMF_Z_a090_cluster_p100_n300_bdgraph_data)



load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_cluster_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_cluster_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_cluster_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_cluster_p100_n300_bdgraph_data.RData")

results_HMF_Z_a070_bC <- list(results_HMF_Z_a070_cluster_p100_n35_bdgraph_data,
                           results_HMF_Z_a070_cluster_p100_n75_bdgraph_data,
                           results_HMF_Z_a070_cluster_p100_n150_bdgraph_data,
                           results_HMF_Z_a070_cluster_p100_n300_bdgraph_data)


#===============================================================================


load(file = "simulated_data/bdgraph/scale_free_p_100_n_35_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_75_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_150_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_adjacency.RData")

true_files_bs <- list(scale_free_p_100_n_35_bdgraph_adjacency,
                   scale_free_p_100_n_75_bdgraph_adjacency,
                   scale_free_p_100_n_150_bdgraph_adjacency,
                   scale_free_p_100_n_300_bdgraph_adjacency)


load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_CC_bs <- list(results_HMF_Z_CC_scale_free_p100_n35_bdgraph_data,
                         results_HMF_Z_CC_scale_free_p100_n75_bdgraph_data,
                         results_HMF_Z_CC_scale_free_p100_n150_bdgraph_data,
                         results_HMF_Z_CC_scale_free_p100_n300_bdgraph_data)



load(file="results/bdgraph/results_HMF_Z_scale_free_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_scale_free_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_scale_free_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_a10_bs <- list(results_HMF_Z_scale_free_p100_n35_bdgraph_data,
                          results_HMF_Z_scale_free_p100_n75_bdgraph_data,
                          results_HMF_Z_scale_free_p100_n150_bdgraph_data,
                          results_HMF_Z_scale_free_p100_n300_bdgraph_data)

load(file="results/bdgraph/results_HMF_Z2x_scale_free_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z2x_scale_free_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z2x_scale_free_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z2x_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_a2_bs <- list(results_HMF_Z2x_scale_free_p100_n35_bdgraph_data,
                         results_HMF_Z2x_scale_free_p100_n75_bdgraph_data,
                         results_HMF_Z2x_scale_free_p100_n150_bdgraph_data,
                         results_HMF_Z2x_scale_free_p100_n300_bdgraph_data)


load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_scale_free_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_scale_free_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_scale_free_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_a090_bs <- list(results_HMF_Z_a090_scale_free_p100_n35_bdgraph_data,
                           results_HMF_Z_a090_scale_free_p100_n75_bdgraph_data,
                           results_HMF_Z_a090_scale_free_p100_n150_bdgraph_data,
                           results_HMF_Z_a090_scale_free_p100_n300_bdgraph_data)



load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_scale_free_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_scale_free_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_scale_free_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_a070_bs <- list(results_HMF_Z_a070_scale_free_p100_n35_bdgraph_data,
                           results_HMF_Z_a070_scale_free_p100_n75_bdgraph_data,
                           results_HMF_Z_a070_scale_free_p100_n150_bdgraph_data,
                           results_HMF_Z_a070_scale_free_p100_n300_bdgraph_data)


#===============================================================================

load(file = "simulated_data/huge/cluster_p_100_n_35_huge_adjacency.RData")
load(file = "simulated_data/huge/cluster_p_100_n_75_huge_adjacency.RData")
load(file = "simulated_data/huge/cluster_p_100_n_150_huge_adjacency.RData")
load(file = "simulated_data/huge/cluster_p_100_n_300_huge_adjacency.RData")

true_files_hC <- list(cluster_p_100_n_35_huge_adjacency,
                   cluster_p_100_n_75_huge_adjacency,
                   cluster_p_100_n_150_huge_adjacency,
                   cluster_p_100_n_300_huge_adjacency)

# beta = 0.9

load(file="results/huge/results_HMF_Z_CC_cluster_p100_n35_huge_data.RData")
load(file="results/huge/results_HMF_Z_CC_cluster_p100_n75_huge_data.RData")
load(file="results/huge/results_HMF_Z_CC_cluster_p100_n150_huge_data.RData")
load(file="results/huge/results_HMF_Z_CC_cluster_p100_n300_huge_data.RData")

results_HMF_Z_CC_hC <- list(results_HMF_Z_CC_cluster_p100_n35_huge_data,
                         results_HMF_Z_CC_cluster_p100_n75_huge_data,
                         results_HMF_Z_CC_cluster_p100_n150_huge_data,
                         results_HMF_Z_CC_cluster_p100_n300_huge_data)

load(file="results/huge/results_HMF_Z_cluster_p100_n35_huge_data.RData")
load(file="results/huge/results_HMF_Z_cluster_p100_n75_huge_data.RData")
load(file="results/huge/results_HMF_Z_cluster_p100_n150_huge_data.RData")
load(file="results/huge/results_HMF_Z_cluster_p100_n300_huge_data.RData")

results_HMF_Z_a10_hC <- list(results_HMF_Z_cluster_p100_n35_huge_data,
                             results_HMF_Z_cluster_p100_n75_huge_data,
                             results_HMF_Z_cluster_p100_n150_huge_data,
                             results_HMF_Z_cluster_p100_n300_huge_data)

load(file="results/huge/results_HMF_Z2x_cluster_p100_n35_huge_data.RData")
load(file="results/huge/results_HMF_Z2x_cluster_p100_n75_huge_data.RData")
load(file="results/huge/results_HMF_Z2x_cluster_p100_n150_huge_data.RData")
load(file="results/huge/results_HMF_Z2x_cluster_p100_n300_huge_data.RData")

results_HMF_Z_a2_hC <- list(results_HMF_Z2x_cluster_p100_n35_huge_data,
                            results_HMF_Z2x_cluster_p100_n75_huge_data,
                            results_HMF_Z2x_cluster_p100_n150_huge_data,
                            results_HMF_Z2x_cluster_p100_n300_huge_data)


load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_cluster_p100_n35_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_cluster_p100_n75_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_cluster_p100_n150_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_cluster_p100_n300_huge_data.RData")

results_HMF_Z_a090_hC <- list(results_HMF_Z_a090_cluster_p100_n35_huge_data,
                           results_HMF_Z_a090_cluster_p100_n75_huge_data,
                           results_HMF_Z_a090_cluster_p100_n150_huge_data,
                           results_HMF_Z_a090_cluster_p100_n300_huge_data)



load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_cluster_p100_n35_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_cluster_p100_n75_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_cluster_p100_n150_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_cluster_p100_n300_huge_data.RData")

results_HMF_Z_a070_hC <- list(results_HMF_Z_a070_cluster_p100_n35_huge_data,
                           results_HMF_Z_a070_cluster_p100_n75_huge_data,
                           results_HMF_Z_a070_cluster_p100_n150_huge_data,
                           results_HMF_Z_a070_cluster_p100_n300_huge_data)



#===============================================================================


load(file = "simulated_data/huge/scale_free_p_100_n_35_huge_adjacency.RData")
load(file = "simulated_data/huge/scale_free_p_100_n_75_huge_adjacency.RData")
load(file = "simulated_data/huge/scale_free_p_100_n_150_huge_adjacency.RData")
load(file = "simulated_data/huge/scale_free_p_100_n_300_huge_adjacency.RData")

true_files_hs <- list(scale_free_p_100_n_35_huge_adjacency,
                   scale_free_p_100_n_75_huge_adjacency,
                   scale_free_p_100_n_150_huge_adjacency,
                   scale_free_p_100_n_300_huge_adjacency)
# beta = 0.9

load(file="results/huge/results_HMF_Z_CC_scale_free_p100_n35_huge_data.RData")
load(file="results/huge/results_HMF_Z_CC_scale_free_p100_n75_huge_data.RData")
load(file="results/huge/results_HMF_Z_CC_scale_free_p100_n150_huge_data.RData")
load(file="results/huge/results_HMF_Z_CC_scale_free_p100_n300_huge_data.RData")

results_HMF_Z_CC_hs <- list(results_HMF_Z_CC_scale_free_p100_n35_huge_data,
                         results_HMF_Z_CC_scale_free_p100_n75_huge_data,
                         results_HMF_Z_CC_scale_free_p100_n150_huge_data,
                         results_HMF_Z_CC_scale_free_p100_n300_huge_data)


load(file="results/huge/results_HMF_Z_scale_free_p100_n35_huge_data.RData")
load(file="results/huge/results_HMF_Z_scale_free_p100_n75_huge_data.RData")
load(file="results/huge/results_HMF_Z_scale_free_p100_n150_huge_data.RData")
load(file="results/huge/results_HMF_Z_scale_free_p100_n300_huge_data.RData")

results_HMF_Z_a10_hs <- list(results_HMF_Z_scale_free_p100_n35_huge_data,
                             results_HMF_Z_scale_free_p100_n75_huge_data,
                             results_HMF_Z_scale_free_p100_n150_huge_data,
                             results_HMF_Z_scale_free_p100_n300_huge_data)

load(file="results/huge/results_HMF_Z2x_scale_free_p100_n35_huge_data.RData")
load(file="results/huge/results_HMF_Z2x_scale_free_p100_n75_huge_data.RData")
load(file="results/huge/results_HMF_Z2x_scale_free_p100_n150_huge_data.RData")
load(file="results/huge/results_HMF_Z2x_scale_free_p100_n300_huge_data.RData")

results_HMF_Z_a2_hs <- list(results_HMF_Z2x_scale_free_p100_n35_huge_data,
                            results_HMF_Z2x_scale_free_p100_n75_huge_data,
                            results_HMF_Z2x_scale_free_p100_n150_huge_data,
                            results_HMF_Z2x_scale_free_p100_n300_huge_data)



load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_scale_free_p100_n35_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_scale_free_p100_n75_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_scale_free_p100_n150_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a090_scale_free_p100_n300_huge_data.RData")

results_HMF_Z_a090_hs <- list(results_HMF_Z_a090_scale_free_p100_n35_huge_data,
                           results_HMF_Z_a090_scale_free_p100_n75_huge_data,
                           results_HMF_Z_a090_scale_free_p100_n150_huge_data,
                           results_HMF_Z_a090_scale_free_p100_n300_huge_data)



load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_scale_free_p100_n35_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_scale_free_p100_n75_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_scale_free_p100_n150_huge_data.RData")
load(file="parameter_comparisons/alpha/results/results_HMF_Z_a070_scale_free_p100_n300_huge_data.RData")

results_HMF_Z_a070_hs <- list(results_HMF_Z_a070_scale_free_p100_n35_huge_data,
                           results_HMF_Z_a070_scale_free_p100_n75_huge_data,
                           results_HMF_Z_a070_scale_free_p100_n150_huge_data,
                           results_HMF_Z_a070_scale_free_p100_n300_huge_data)


#===============================================================================
# calculating all results
#===============================================================================

#===============================================================================

a090_results_bC <- matrix(1, ncol=16, nrow=4)
x <- c(35,75,150,300)

a090_results_bC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_bC[[1]],true_files_bC[[1]]))
a090_results_bC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_bC[[2]],true_files_bC[[2]]))
a090_results_bC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_bC[[3]],true_files_bC[[3]]))
a090_results_bC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_bC[[4]],true_files_bC[[4]]))



CC_results_bC <- matrix(1, ncol=16, nrow=4)

CC_results_bC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bC[[1]],true_files_bC[[1]]))
CC_results_bC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bC[[2]],true_files_bC[[2]]))
CC_results_bC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bC[[3]],true_files_bC[[3]]))
CC_results_bC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bC[[4]],true_files_bC[[4]]))



a070_results_bC <- matrix(1, ncol=16, nrow=4)

a070_results_bC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_bC[[1]],true_files_bC[[1]]))
a070_results_bC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_bC[[2]],true_files_bC[[2]]))
a070_results_bC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_bC[[3]],true_files_bC[[3]]))
a070_results_bC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_bC[[4]],true_files_bC[[4]]))

a10_results_bC <- matrix(1, ncol=16, nrow=4)


a10_results_bC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_bC[[1]],true_files_bC[[1]]))
a10_results_bC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_bC[[2]],true_files_bC[[2]]))
a10_results_bC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_bC[[3]],true_files_bC[[3]]))
a10_results_bC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_bC[[4]],true_files_bC[[4]]))


a2_results_bC <- matrix(1, ncol=16, nrow=4)


a2_results_bC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_bC[[1]],true_files_bC[[1]]))
a2_results_bC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_bC[[2]],true_files_bC[[2]]))
a2_results_bC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_bC[[3]],true_files_bC[[3]]))
a2_results_bC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_bC[[4]],true_files_bC[[4]]))


#===============================================================================

a090_results_bs <- matrix(1, ncol=16, nrow=4)
x <- c(35,75,150,300)

a090_results_bs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_bs[[1]],true_files_bs[[1]]))
a090_results_bs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_bs[[2]],true_files_bs[[2]]))
a090_results_bs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_bs[[3]],true_files_bs[[3]]))
a090_results_bs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_bs[[4]],true_files_bs[[4]]))



CC_results_bs <- matrix(1, ncol=16, nrow=4)

CC_results_bs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bs[[1]],true_files_bs[[1]]))
CC_results_bs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bs[[2]],true_files_bs[[2]]))
CC_results_bs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bs[[3]],true_files_bs[[3]]))
CC_results_bs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bs[[4]],true_files_bs[[4]]))



a070_results_bs <- matrix(1, ncol=16, nrow=4)

a070_results_bs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_bs[[1]],true_files_bs[[1]]))
a070_results_bs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_bs[[2]],true_files_bs[[2]]))
a070_results_bs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_bs[[3]],true_files_bs[[3]]))
a070_results_bs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_bs[[4]],true_files_bs[[4]]))

a10_results_bs <- matrix(1, ncol=16, nrow=4)


a10_results_bs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_bs[[1]],true_files_bs[[1]]))
a10_results_bs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_bs[[2]],true_files_bs[[2]]))
a10_results_bs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_bs[[3]],true_files_bs[[3]]))
a10_results_bs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_bs[[4]],true_files_bs[[4]]))


a2_results_bs <- matrix(1, ncol=16, nrow=4)


a2_results_bs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_bs[[1]],true_files_bs[[1]]))
a2_results_bs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_bs[[2]],true_files_bs[[2]]))
a2_results_bs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_bs[[3]],true_files_bs[[3]]))
a2_results_bs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_bs[[4]],true_files_bs[[4]]))


#===============================================================================

a090_results_hC <- matrix(1, ncol=16, nrow=4)
x <- c(35,75,150,300)

a090_results_hC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_hC[[1]],true_files_hC[[1]]))
a090_results_hC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_hC[[2]],true_files_hC[[2]]))
a090_results_hC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_hC[[3]],true_files_hC[[3]]))
a090_results_hC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_hC[[4]],true_files_hC[[4]]))



CC_results_hC <- matrix(1, ncol=16, nrow=4)

CC_results_hC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hC[[1]],true_files_hC[[1]]))
CC_results_hC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hC[[2]],true_files_hC[[2]]))
CC_results_hC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hC[[3]],true_files_hC[[3]]))
CC_results_hC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hC[[4]],true_files_hC[[4]]))



a070_results_hC <- matrix(1, ncol=16, nrow=4)

a070_results_hC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_hC[[1]],true_files_hC[[1]]))
a070_results_hC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_hC[[2]],true_files_hC[[2]]))
a070_results_hC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_hC[[3]],true_files_hC[[3]]))
a070_results_hC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_hC[[4]],true_files_hC[[4]]))

a10_results_hC <- matrix(1, ncol=16, nrow=4)


a10_results_hC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_hC[[1]],true_files_hC[[1]]))
a10_results_hC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_hC[[2]],true_files_hC[[2]]))
a10_results_hC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_hC[[3]],true_files_hC[[3]]))
a10_results_hC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_hC[[4]],true_files_hC[[4]]))


a2_results_hC <- matrix(1, ncol=16, nrow=4)


a2_results_hC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_hC[[1]],true_files_hC[[1]]))
a2_results_hC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_hC[[2]],true_files_hC[[2]]))
a2_results_hC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_hC[[3]],true_files_hC[[3]]))
a2_results_hC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_hC[[4]],true_files_hC[[4]]))


#===============================================================================

a090_results_hs <- matrix(1, ncol=16, nrow=4)
x <- c(35,75,150,300)

a090_results_hs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_hs[[1]],true_files_hs[[1]]))
a090_results_hs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_hs[[2]],true_files_hs[[2]]))
a090_results_hs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_hs[[3]],true_files_hs[[3]]))
a090_results_hs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a090_hs[[4]],true_files_hs[[4]]))



CC_results_hs <- matrix(1, ncol=16, nrow=4)

CC_results_hs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hs[[1]],true_files_hs[[1]]))
CC_results_hs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hs[[2]],true_files_hs[[2]]))
CC_results_hs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hs[[3]],true_files_hs[[3]]))
CC_results_hs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hs[[4]],true_files_hs[[4]]))



a070_results_hs <- matrix(1, ncol=16, nrow=4)

a070_results_hs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_hs[[1]],true_files_hs[[1]]))
a070_results_hs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_hs[[2]],true_files_hs[[2]]))
a070_results_hs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_hs[[3]],true_files_hs[[3]]))
a070_results_hs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a070_hs[[4]],true_files_hs[[4]]))

a10_results_hs <- matrix(1, ncol=16, nrow=4)


a10_results_hs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_hs[[1]],true_files_hs[[1]]))
a10_results_hs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_hs[[2]],true_files_hs[[2]]))
a10_results_hs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_hs[[3]],true_files_hs[[3]]))
a10_results_hs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a10_hs[[4]],true_files_hs[[4]]))


a2_results_hs <- matrix(1, ncol=16, nrow=4)


a2_results_hs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_hs[[1]],true_files_hs[[1]]))
a2_results_hs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_hs[[2]],true_files_hs[[2]]))
a2_results_hs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_hs[[3]],true_files_hs[[3]]))
a2_results_hs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_a2_hs[[4]],true_files_hs[[4]]))

#===============================================================================
# Plotting the results
#===============================================================================


# 1 = accuracy, 
# 2 = balanced accuracy, 
# 3 = MCC, 
# 4 = F1,
# 5 = TPR, 
# 6 = TNR, 
# 7 = PPV, 
# 8 = NPV,
# 9 = FNR, 
# 10 = FPR, 
# 11 = FOR, 
# 12 = LRp,
# 13 = LRn, 
# 14 = FDR, 
# 15 = Averace Clustering coefficients (ACC), 
# 16 = NMI

names <- c("accuracy", "bal-accuracy", "MCC", "F1", "TPR", "TNR", "PPV","NPV", "FNR",
           "FPR","FOR", "LRp", "LRn", "FDR", "ACC", "NMI")

value <- 4 # F1



setEPS()
postscript("alpha_results.eps", width = 12, height = 15)   

par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.1,0.1,0.05))


plot(x=x, y=CC_results_hs[,value],col = colors[1] ,type="l", ylim=c(0,1), 
     ylab = paste0(names[value]), xlab="sample size (n)", main="huge, Scale-free"
     , lwd = 2,cex.lab = 1.2,cex.main=2, lty=1)
lines(x=x,y=a10_results_hs[,value],col=colors[2], lwd = 2,cex.lab = 1.2,cex.main=2, lty=2)
lines(x=x,y=a2_results_hs[,value],col=colors[3], lwd = 2,cex.lab = 1.2,cex.main=2, lty=3)
lines(x=x,y=a070_results_hs[,value],col=colors[4], lwd = 2,cex.lab = 1.2,cex.main=2, lty=4)
lines(x=x,y=a090_results_hs[,value],col=colors[5], lwd = 2,cex.lab = 1.2,cex.main=2, lty=5)


plot(x=x, y=CC_results_hC[,value],col = colors[1], type="l", ylim=c(0,1), 
     ylab = paste0(names[value]), xlab="sample size (n)", main="huge, Cluster"
     , lwd = 2,cex.lab = 1.2,cex.main=2, lty=1)
lines(x=x,y=a10_results_hC[,value],col=colors[2], lwd = 2,cex.lab = 1.2,cex.main=2, lty=2)
lines(x=x,y=a2_results_hC[,value],col=colors[3], lwd = 2,cex.lab = 1.2,cex.main=2, lty=3)
lines(x=x,y=a070_results_hC[,value],col=colors[4], lwd = 2,cex.lab = 1.2,cex.main=2, lty=4)
lines(x=x,y=a090_results_hC[,value],col=colors[5], lwd = 2,cex.lab = 1.2,cex.main=2, lty=5)


plot(x=x, y=CC_results_bs[,value],col = colors[1], type="l", ylim=c(0,1), 
     ylab = paste0(names[value]), xlab="sample size (n)", main="Bdgraph, Scale-free"
     , lwd = 2,cex.lab = 1.2,cex.main=2, lty=1)
lines(x=x,y=a10_results_bs[,value],col=colors[2], lwd = 2,cex.lab = 1.2,cex.main=2, lty=2)
lines(x=x,y=a2_results_bs[,value],col=colors[3], lwd = 2,cex.lab = 1.2,cex.main=2, lty=3)
lines(x=x,y=a070_results_bs[,value],col=colors[4], lwd = 2,cex.lab = 1.2,cex.main=2, lty=4)
lines(x=x,y=a090_results_bs[,value],col=colors[5], lwd = 2,cex.lab = 1.2,cex.main=2, lty=5)


plot(x=x, y=CC_results_bC[,value],col = colors[1], type="l", ylim=c(0,1), 
     ylab = paste0(names[value]), xlab="sample size (n)", main="Bdgraph, Cluster"
     , lwd = 2,cex.lab = 1.2,cex.main=2, lty=1)
lines(x=x,y=a10_results_bC[,value],col=colors[2], lwd = 2,cex.lab = 1.2,cex.main=2, lty=2)
lines(x=x,y=a2_results_bC[,value],col=colors[3], lwd = 2,cex.lab = 1.2,cex.main=2, lty=3)
lines(x=x,y=a070_results_bC[,value],col=colors[4], lwd = 2,cex.lab = 1.2,cex.main=2, lty=4)
lines(x=x,y=a090_results_bC[,value],col=colors[5], lwd = 2,cex.lab = 1.2,cex.main=2, lty=5)


par(mar = c(1, 1, 1, 1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c(colors[1],colors[2], colors[3], colors[4], colors[5])
legend(x = "top",inset = 0,
       legend = c("CC-method", "alpha = 10*p/(10*p+n)", "alpha = 2*p/(2*p+n)","alpha = 0.7", "alpha = 0.9"), 
       col=plot_colors, lwd=2, cex=1.2, xpd = TRUE, horiz = TRUE, lty = c(1:5))


dev.off()



value <- 14 # FDR



setEPS()
postscript("alpha_results_fdr.eps", width = 12, height = 15)   

par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.1,0.1,0.05))


plot(x=x, y=CC_results_hs[,value],col = colors[1], type="l", ylim=c(0,1), 
     ylab = paste0(names[value]), xlab="sample size (n)", main="huge, Scale-free"
     , lwd = 2,cex.lab = 1.2,cex.main=2, lty=1)
lines(x=x,y=a10_results_hs[,value],col=colors[2], lwd = 2,cex.lab = 1.2,cex.main=2, lty=2)
lines(x=x,y=a2_results_hs[,value],col=colors[3], lwd = 2,cex.lab = 1.2,cex.main=2, lty=3)
lines(x=x,y=a070_results_hs[,value],col=colors[4], lwd = 2,cex.lab = 1.2,cex.main=2, lty=4)
lines(x=x,y=a090_results_hs[,value],col=colors[5], lwd = 2,cex.lab = 1.2,cex.main=2, lty=5)


plot(x=x, y=CC_results_hC[,value],col = colors[1], type="l", ylim=c(0,1), 
     ylab = paste0(names[value]), xlab="sample size (n)", main="huge, Cluster"
     , lwd = 2,cex.lab = 1.2,cex.main=2, lty=1)
lines(x=x,y=a10_results_hC[,value],col=colors[2], lwd = 2,cex.lab = 1.2,cex.main=2, lty=2)
lines(x=x,y=a2_results_hC[,value],col=colors[3], lwd = 2,cex.lab = 1.2,cex.main=2, lty=3)
lines(x=x,y=a070_results_hC[,value],col=colors[4], lwd = 2,cex.lab = 1.2,cex.main=2, lty=4)
lines(x=x,y=a090_results_hC[,value],col=colors[5], lwd = 2,cex.lab = 1.2,cex.main=2, lty=5)


plot(x=x, y=CC_results_bs[,value],col = colors[1], type="l", ylim=c(0,1), 
     ylab = paste0(names[value]), xlab="sample size (n)", main="Bdgraph, Scale-free"
     , lwd = 2,cex.lab = 1.2,cex.main=2, lty=1)
lines(x=x,y=a10_results_bs[,value],col=colors[2], lwd = 2,cex.lab = 1.2,cex.main=2, lty=2)
lines(x=x,y=a2_results_bs[,value],col=colors[3], lwd = 2,cex.lab = 1.2,cex.main=2, lty=3)
lines(x=x,y=a070_results_bs[,value],col=colors[4], lwd = 2,cex.lab = 1.2,cex.main=2, lty=4)
lines(x=x,y=a090_results_bs[,value],col=colors[5], lwd = 2,cex.lab = 1.2,cex.main=2, lty=5)


plot(x=x, y=CC_results_bC[,value],col = colors[1], type="l", ylim=c(0,1), 
     ylab = paste0(names[value]), xlab="sample size (n)", main="Bdgraph, Cluster"
     , lwd = 2,cex.lab = 1.2,cex.main=2, lty=1)
lines(x=x,y=a10_results_bC[,value],col=colors[2], lwd = 2,cex.lab = 1.2,cex.main=2, lty=2)
lines(x=x,y=a2_results_bC[,value],col=colors[3], lwd = 2,cex.lab = 1.2,cex.main=2, lty=3)
lines(x=x,y=a070_results_bC[,value],col=colors[4], lwd = 2,cex.lab = 1.2,cex.main=2, lty=4)
lines(x=x,y=a090_results_bC[,value],col=colors[5], lwd = 2,cex.lab = 1.2,cex.main=2, lty=5)


par(mar = c(1, 1, 1, 1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c(colors[1],colors[2], colors[3], colors[4], colors[5])
legend(x = "top",inset = 0,
       legend = c("CC-method", "alpha = 10*p/(10*p+n)", "alpha = 2*p/(2*p+n)","alpha = 0.7", "alpha = 0.9"), 
       col=plot_colors, lwd=2, cex=1.2, xpd = TRUE, horiz = TRUE, lty=c(1:5))


dev.off()


