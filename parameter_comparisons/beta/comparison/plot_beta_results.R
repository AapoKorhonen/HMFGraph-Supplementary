
################################################################################################################################################################
#==============================================================================================================================================================
# This file is for plotting out the results, bdgraph and huge, cluster and scale_free, with multiple beta values
#==============================================================================================================================================================
################################################################################################################################################################

source("functions/functions_for_result_handeling.R")


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


load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_cluster_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_cluster_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_cluster_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_cluster_p100_n300_bdgraph_data.RData")

results_HMF_Z_b095_bC <- list(results_HMF_Z_b095_cluster_p100_n35_bdgraph_data,
                           results_HMF_Z_b095_cluster_p100_n75_bdgraph_data,
                           results_HMF_Z_b095_cluster_p100_n150_bdgraph_data,
                           results_HMF_Z_b095_cluster_p100_n300_bdgraph_data)



load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_cluster_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_cluster_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_cluster_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_cluster_p100_n300_bdgraph_data.RData")

results_HMF_Z_b080_bC <- list(results_HMF_Z_b080_cluster_p100_n35_bdgraph_data,
                           results_HMF_Z_b080_cluster_p100_n75_bdgraph_data,
                           results_HMF_Z_b080_cluster_p100_n150_bdgraph_data,
                           results_HMF_Z_b080_cluster_p100_n300_bdgraph_data)


load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_cluster_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_cluster_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_cluster_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_cluster_p100_n300_bdgraph_data.RData")

results_HMF_Z_b070_bC <- list(results_HMF_Z_b070_cluster_p100_n35_bdgraph_data,
                           results_HMF_Z_b070_cluster_p100_n75_bdgraph_data,
                           results_HMF_Z_b070_cluster_p100_n150_bdgraph_data,
                           results_HMF_Z_b070_cluster_p100_n300_bdgraph_data)

load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_cluster_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_cluster_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_cluster_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_cluster_p100_n300_bdgraph_data.RData")

results_HMF_Z_b060_bC <- list(results_HMF_Z_b060_cluster_p100_n35_bdgraph_data,
                           results_HMF_Z_b060_cluster_p100_n75_bdgraph_data,
                           results_HMF_Z_b060_cluster_p100_n150_bdgraph_data,
                           results_HMF_Z_b060_cluster_p100_n300_bdgraph_data)

#===============================================================================


load(file = "simulated_data/bdgraph/scale_free_p_100_n_35_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_75_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_150_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_adjacency.RData")

true_files_bs <- list(scale_free_p_100_n_35_bdgraph_adjacency,
                   scale_free_p_100_n_75_bdgraph_adjacency,
                   scale_free_p_100_n_150_bdgraph_adjacency,
                   scale_free_p_100_n_300_bdgraph_adjacency)
# beta = 0.9

load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_CC_bs <- list(results_HMF_Z_CC_scale_free_p100_n35_bdgraph_data,
                         results_HMF_Z_CC_scale_free_p100_n75_bdgraph_data,
                         results_HMF_Z_CC_scale_free_p100_n150_bdgraph_data,
                         results_HMF_Z_CC_scale_free_p100_n300_bdgraph_data)


load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_scale_free_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_scale_free_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_scale_free_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_b095_bs <- list(results_HMF_Z_b095_scale_free_p100_n35_bdgraph_data,
                           results_HMF_Z_b095_scale_free_p100_n75_bdgraph_data,
                           results_HMF_Z_b095_scale_free_p100_n150_bdgraph_data,
                           results_HMF_Z_b095_scale_free_p100_n300_bdgraph_data)



load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_scale_free_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_scale_free_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_scale_free_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_b080_bs <- list(results_HMF_Z_b080_scale_free_p100_n35_bdgraph_data,
                           results_HMF_Z_b080_scale_free_p100_n75_bdgraph_data,
                           results_HMF_Z_b080_scale_free_p100_n150_bdgraph_data,
                           results_HMF_Z_b080_scale_free_p100_n300_bdgraph_data)


load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_scale_free_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_scale_free_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_scale_free_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_b070_bs <- list(results_HMF_Z_b070_scale_free_p100_n35_bdgraph_data,
                           results_HMF_Z_b070_scale_free_p100_n75_bdgraph_data,
                           results_HMF_Z_b070_scale_free_p100_n150_bdgraph_data,
                           results_HMF_Z_b070_scale_free_p100_n300_bdgraph_data)

load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_scale_free_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_scale_free_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_scale_free_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_b060_bs <- list(results_HMF_Z_b060_scale_free_p100_n35_bdgraph_data,
                           results_HMF_Z_b060_scale_free_p100_n75_bdgraph_data,
                           results_HMF_Z_b060_scale_free_p100_n150_bdgraph_data,
                           results_HMF_Z_b060_scale_free_p100_n300_bdgraph_data)

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


load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_cluster_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_cluster_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_cluster_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_cluster_p100_n300_huge_data.RData")

results_HMF_Z_b095_hC <- list(results_HMF_Z_b095_cluster_p100_n35_huge_data,
                           results_HMF_Z_b095_cluster_p100_n75_huge_data,
                           results_HMF_Z_b095_cluster_p100_n150_huge_data,
                           results_HMF_Z_b095_cluster_p100_n300_huge_data)



load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_cluster_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_cluster_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_cluster_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_cluster_p100_n300_huge_data.RData")

results_HMF_Z_b080_hC <- list(results_HMF_Z_b080_cluster_p100_n35_huge_data,
                           results_HMF_Z_b080_cluster_p100_n75_huge_data,
                           results_HMF_Z_b080_cluster_p100_n150_huge_data,
                           results_HMF_Z_b080_cluster_p100_n300_huge_data)


load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_cluster_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_cluster_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_cluster_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_cluster_p100_n300_huge_data.RData")

results_HMF_Z_b070_hC <- list(results_HMF_Z_b070_cluster_p100_n35_huge_data,
                           results_HMF_Z_b070_cluster_p100_n75_huge_data,
                           results_HMF_Z_b070_cluster_p100_n150_huge_data,
                           results_HMF_Z_b070_cluster_p100_n300_huge_data)

load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_cluster_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_cluster_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_cluster_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_cluster_p100_n300_huge_data.RData")

results_HMF_Z_b060_hC <- list(results_HMF_Z_b060_cluster_p100_n35_huge_data,
                           results_HMF_Z_b060_cluster_p100_n75_huge_data,
                           results_HMF_Z_b060_cluster_p100_n150_huge_data,
                           results_HMF_Z_b060_cluster_p100_n300_huge_data)


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


load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_scale_free_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_scale_free_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_scale_free_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_scale_free_p100_n300_huge_data.RData")

results_HMF_Z_b095_hs <- list(results_HMF_Z_b095_scale_free_p100_n35_huge_data,
                           results_HMF_Z_b095_scale_free_p100_n75_huge_data,
                           results_HMF_Z_b095_scale_free_p100_n150_huge_data,
                           results_HMF_Z_b095_scale_free_p100_n300_huge_data)



load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_scale_free_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_scale_free_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_scale_free_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_scale_free_p100_n300_huge_data.RData")

results_HMF_Z_b080_hs <- list(results_HMF_Z_b080_scale_free_p100_n35_huge_data,
                           results_HMF_Z_b080_scale_free_p100_n75_huge_data,
                           results_HMF_Z_b080_scale_free_p100_n150_huge_data,
                           results_HMF_Z_b080_scale_free_p100_n300_huge_data)


load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_scale_free_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_scale_free_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_scale_free_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_scale_free_p100_n300_huge_data.RData")

results_HMF_Z_b070_hs <- list(results_HMF_Z_b070_scale_free_p100_n35_huge_data,
                           results_HMF_Z_b070_scale_free_p100_n75_huge_data,
                           results_HMF_Z_b070_scale_free_p100_n150_huge_data,
                           results_HMF_Z_b070_scale_free_p100_n300_huge_data)

load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_scale_free_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_scale_free_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_scale_free_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_scale_free_p100_n300_huge_data.RData")

results_HMF_Z_b060_hs <- list(results_HMF_Z_b060_scale_free_p100_n35_huge_data,
                           results_HMF_Z_b060_scale_free_p100_n75_huge_data,
                           results_HMF_Z_b060_scale_free_p100_n150_huge_data,
                           results_HMF_Z_b060_scale_free_p100_n300_huge_data)


#===============================================================================
# calculating all results
#===============================================================================

#===============================================================================

b095_results_bC <- matrix(1, ncol=16, nrow=4)
x <- c(35,75,150,300)

b095_results_bC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_bC[[1]],true_files_bC[[1]]))
b095_results_bC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_bC[[2]],true_files_bC[[2]]))
b095_results_bC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_bC[[3]],true_files_bC[[3]]))
b095_results_bC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_bC[[4]],true_files_bC[[4]]))



b090_results_bC <- matrix(1, ncol=16, nrow=4)

b090_results_bC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bC[[1]],true_files_bC[[1]]))
b090_results_bC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bC[[2]],true_files_bC[[2]]))
b090_results_bC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bC[[3]],true_files_bC[[3]]))
b090_results_bC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bC[[4]],true_files_bC[[4]]))



b080_results_bC <- matrix(1, ncol=16, nrow=4)

b080_results_bC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_bC[[1]],true_files_bC[[1]]))
b080_results_bC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_bC[[2]],true_files_bC[[2]]))
b080_results_bC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_bC[[3]],true_files_bC[[3]]))
b080_results_bC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_bC[[4]],true_files_bC[[4]]))

b070_results_bC <- matrix(1, ncol=16, nrow=4)


b070_results_bC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_bC[[1]],true_files_bC[[1]]))
b070_results_bC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_bC[[2]],true_files_bC[[2]]))
b070_results_bC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_bC[[3]],true_files_bC[[3]]))
b070_results_bC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_bC[[4]],true_files_bC[[4]]))


b060_results_bC <- matrix(1, ncol=16, nrow=4)


b060_results_bC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_bC[[1]],true_files_bC[[1]]))
b060_results_bC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_bC[[2]],true_files_bC[[2]]))
b060_results_bC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_bC[[3]],true_files_bC[[3]]))
b060_results_bC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_bC[[4]],true_files_bC[[4]]))


#===============================================================================

b095_results_bs <- matrix(1, ncol=16, nrow=4)
x <- c(35,75,150,300)

b095_results_bs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_bs[[1]],true_files_bs[[1]]))
b095_results_bs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_bs[[2]],true_files_bs[[2]]))
b095_results_bs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_bs[[3]],true_files_bs[[3]]))
b095_results_bs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_bs[[4]],true_files_bs[[4]]))



b090_results_bs <- matrix(1, ncol=16, nrow=4)

b090_results_bs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bs[[1]],true_files_bs[[1]]))
b090_results_bs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bs[[2]],true_files_bs[[2]]))
b090_results_bs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bs[[3]],true_files_bs[[3]]))
b090_results_bs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_bs[[4]],true_files_bs[[4]]))



b080_results_bs <- matrix(1, ncol=16, nrow=4)

b080_results_bs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_bs[[1]],true_files_bs[[1]]))
b080_results_bs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_bs[[2]],true_files_bs[[2]]))
b080_results_bs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_bs[[3]],true_files_bs[[3]]))
b080_results_bs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_bs[[4]],true_files_bs[[4]]))

b070_results_bs <- matrix(1, ncol=16, nrow=4)


b070_results_bs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_bs[[1]],true_files_bs[[1]]))
b070_results_bs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_bs[[2]],true_files_bs[[2]]))
b070_results_bs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_bs[[3]],true_files_bs[[3]]))
b070_results_bs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_bs[[4]],true_files_bs[[4]]))


b060_results_bs <- matrix(1, ncol=16, nrow=4)


b060_results_bs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_bs[[1]],true_files_bs[[1]]))
b060_results_bs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_bs[[2]],true_files_bs[[2]]))
b060_results_bs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_bs[[3]],true_files_bs[[3]]))
b060_results_bs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_bs[[4]],true_files_bs[[4]]))


#===============================================================================

b095_results_hC <- matrix(1, ncol=16, nrow=4)
x <- c(35,75,150,300)

b095_results_hC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_hC[[1]],true_files_hC[[1]]))
b095_results_hC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_hC[[2]],true_files_hC[[2]]))
b095_results_hC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_hC[[3]],true_files_hC[[3]]))
b095_results_hC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_hC[[4]],true_files_hC[[4]]))



b090_results_hC <- matrix(1, ncol=16, nrow=4)

b090_results_hC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hC[[1]],true_files_hC[[1]]))
b090_results_hC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hC[[2]],true_files_hC[[2]]))
b090_results_hC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hC[[3]],true_files_hC[[3]]))
b090_results_hC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hC[[4]],true_files_hC[[4]]))



b080_results_hC <- matrix(1, ncol=16, nrow=4)

b080_results_hC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_hC[[1]],true_files_hC[[1]]))
b080_results_hC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_hC[[2]],true_files_hC[[2]]))
b080_results_hC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_hC[[3]],true_files_hC[[3]]))
b080_results_hC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_hC[[4]],true_files_hC[[4]]))

b070_results_hC <- matrix(1, ncol=16, nrow=4)


b070_results_hC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_hC[[1]],true_files_hC[[1]]))
b070_results_hC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_hC[[2]],true_files_hC[[2]]))
b070_results_hC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_hC[[3]],true_files_hC[[3]]))
b070_results_hC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_hC[[4]],true_files_hC[[4]]))


b060_results_hC <- matrix(1, ncol=16, nrow=4)


b060_results_hC[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_hC[[1]],true_files_hC[[1]]))
b060_results_hC[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_hC[[2]],true_files_hC[[2]]))
b060_results_hC[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_hC[[3]],true_files_hC[[3]]))
b060_results_hC[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_hC[[4]],true_files_hC[[4]]))


#===============================================================================

b095_results_hs <- matrix(1, ncol=16, nrow=4)
x <- c(35,75,150,300)

b095_results_hs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_hs[[1]],true_files_hs[[1]]))
b095_results_hs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_hs[[2]],true_files_hs[[2]]))
b095_results_hs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_hs[[3]],true_files_hs[[3]]))
b095_results_hs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095_hs[[4]],true_files_hs[[4]]))



b090_results_hs <- matrix(1, ncol=16, nrow=4)

b090_results_hs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hs[[1]],true_files_hs[[1]]))
b090_results_hs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hs[[2]],true_files_hs[[2]]))
b090_results_hs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hs[[3]],true_files_hs[[3]]))
b090_results_hs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC_hs[[4]],true_files_hs[[4]]))



b080_results_hs <- matrix(1, ncol=16, nrow=4)

b080_results_hs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_hs[[1]],true_files_hs[[1]]))
b080_results_hs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_hs[[2]],true_files_hs[[2]]))
b080_results_hs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_hs[[3]],true_files_hs[[3]]))
b080_results_hs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080_hs[[4]],true_files_hs[[4]]))

b070_results_hs <- matrix(1, ncol=16, nrow=4)


b070_results_hs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_hs[[1]],true_files_hs[[1]]))
b070_results_hs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_hs[[2]],true_files_hs[[2]]))
b070_results_hs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_hs[[3]],true_files_hs[[3]]))
b070_results_hs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070_hs[[4]],true_files_hs[[4]]))


b060_results_hs <- matrix(1, ncol=16, nrow=4)


b060_results_hs[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_hs[[1]],true_files_hs[[1]]))
b060_results_hs[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_hs[[2]],true_files_hs[[2]]))
b060_results_hs[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_hs[[3]],true_files_hs[[3]]))
b060_results_hs[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060_hs[[4]],true_files_hs[[4]]))

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

par(mfrow=c(2,2))

plot(x=x, y=b095_results_hs[,value], type="l", ylim=c(0,1), ylab = paste0(names[value]), xlab="n", main="huge, Scale-free")
lines(x=x,y=b090_results_hs[,value],col="red")
lines(x=x,y=b080_results_hs[,value],col="purple")
lines(x=x,y=b070_results_hs[,value],col="green")
lines(x=x,y=b060_results_hs[,value],col="brown")


plot(x=x, y=b095_results_hC[,value], type="l", ylim=c(0,1), ylab = paste0(names[value]), xlab="n", main="huge, Cluster")
lines(x=x,y=b090_results_hC[,value],col="red")
lines(x=x,y=b080_results_hC[,value],col="purple")
lines(x=x,y=b070_results_hC[,value],col="green")
lines(x=x,y=b060_results_hC[,value],col="brown")


plot(x=x, y=b095_results_bs[,value], type="l", ylim=c(0,1), ylab = paste0(names[value]), xlab="n", main="Bdgraph, Scale-free")
lines(x=x,y=b090_results_bs[,value],col="red")
lines(x=x,y=b080_results_bs[,value],col="purple")
lines(x=x,y=b070_results_bs[,value],col="green")
lines(x=x,y=b060_results_bs[,value],col="brown")


plot(x=x, y=b095_results_bC[,value], type="l", ylim=c(0,1), ylab = paste0(names[value]), xlab="n", main="Bdgraph, Cluster")
lines(x=x,y=b090_results_bC[,value],col="red")
lines(x=x,y=b080_results_bC[,value],col="purple")
lines(x=x,y=b070_results_bC[,value],col="green")
lines(x=x,y=b060_results_bC[,value],col="brown")




