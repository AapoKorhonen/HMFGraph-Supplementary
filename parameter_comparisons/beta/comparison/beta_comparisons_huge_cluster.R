################################################################################
#===============================================================================
# This file is for printing out the results, huge, cluster, with multiple beta values
#===============================================================================
################################################################################

source("functions/functions_for_result_handeling.R")


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

Values <- c(15,16,4) # MCC, FDR, F1, TPR 

round_value1 <- 2  # how many digits will be showed for the Mean 
round_value2 <- 2  # number of digits for SD


#===============================================================================
# Loading all results
#===============================================================================

load(file = "simulated_data/huge/cluster_p_100_n_35_huge_adjacency.RData")
load(file = "simulated_data/huge/cluster_p_100_n_75_huge_adjacency.RData")
load(file = "simulated_data/huge/cluster_p_100_n_150_huge_adjacency.RData")
load(file = "simulated_data/huge/cluster_p_100_n_300_huge_adjacency.RData")

true_files <- list(cluster_p_100_n_35_huge_adjacency,
                   cluster_p_100_n_75_huge_adjacency,
                   cluster_p_100_n_150_huge_adjacency,
                   cluster_p_100_n_300_huge_adjacency)

# beta = 0.9

load(file="results/huge/results_HMF_Z_CC_cluster_p100_n35_huge_data.RData")
load(file="results/huge/results_HMF_Z_CC_cluster_p100_n75_huge_data.RData")
load(file="results/huge/results_HMF_Z_CC_cluster_p100_n150_huge_data.RData")
load(file="results/huge/results_HMF_Z_CC_cluster_p100_n300_huge_data.RData")

results_HMF_Z_CC <- list(results_HMF_Z_CC_cluster_p100_n35_huge_data,
                         results_HMF_Z_CC_cluster_p100_n75_huge_data,
                         results_HMF_Z_CC_cluster_p100_n150_huge_data,
                         results_HMF_Z_CC_cluster_p100_n300_huge_data)


load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_cluster_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_cluster_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_cluster_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b095_cluster_p100_n300_huge_data.RData")

results_HMF_Z_b095 <- list(results_HMF_Z_b095_cluster_p100_n35_huge_data,
                           results_HMF_Z_b095_cluster_p100_n75_huge_data,
                           results_HMF_Z_b095_cluster_p100_n150_huge_data,
                           results_HMF_Z_b095_cluster_p100_n300_huge_data)



load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_cluster_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_cluster_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_cluster_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b080_cluster_p100_n300_huge_data.RData")

results_HMF_Z_b080 <- list(results_HMF_Z_b080_cluster_p100_n35_huge_data,
                           results_HMF_Z_b080_cluster_p100_n75_huge_data,
                           results_HMF_Z_b080_cluster_p100_n150_huge_data,
                           results_HMF_Z_b080_cluster_p100_n300_huge_data)


load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_cluster_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_cluster_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_cluster_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b070_cluster_p100_n300_huge_data.RData")

results_HMF_Z_b070 <- list(results_HMF_Z_b070_cluster_p100_n35_huge_data,
                           results_HMF_Z_b070_cluster_p100_n75_huge_data,
                           results_HMF_Z_b070_cluster_p100_n150_huge_data,
                           results_HMF_Z_b070_cluster_p100_n300_huge_data)

load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_cluster_p100_n35_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_cluster_p100_n75_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_cluster_p100_n150_huge_data.RData")
load(file="parameter_comparisons/beta/results/huge/results_HMF_Z_b060_cluster_p100_n300_huge_data.RData")

results_HMF_Z_b060 <- list(results_HMF_Z_b060_cluster_p100_n35_huge_data,
                           results_HMF_Z_b060_cluster_p100_n75_huge_data,
                           results_HMF_Z_b060_cluster_p100_n150_huge_data,
                           results_HMF_Z_b060_cluster_p100_n300_huge_data)

################################################################################
################################################################################
# RESULTS
################################################################################
################################################################################



################################################################################
#===============================================================================
# HMFGraph, Optimal CI, beta = 0.95
#===============================================================================
################################################################################

cat("HMFGraph, Optimal CI, beta = 0.95")

print_results(results_HMF_Z_b095,true_files, values = Values, round1=round_value1,round2=round_value2)

################################################################################
#===============================================================================
# HMFGraph, Optimal CI, beta = 0.9
#===============================================================================
################################################################################

cat("HMFGraph, Optimal CI, beta=0.9")

print_results(results_HMF_Z_CC,true_files, values = Values, round1=round_value1,round2=round_value2)


################################################################################
#===============================================================================
# HMFGraph, Optimal CI, beta = 0.80
#===============================================================================
################################################################################

cat("HMFGraph, Optimal CI, beta = 0.80")

print_results(results_HMF_Z_b080,true_files, values = Values, round1=round_value1,round2=round_value2)


################################################################################
#===============================================================================
# HMFGraph, Optimal CI, beta = 0.70
#===============================================================================
################################################################################

cat("HMFGraph, Optimal CI, beta = 0.70")

print_results(results_HMF_Z_b070,true_files, values = Values, round1=round_value1,round2=round_value2)


################################################################################
#===============================================================================
# HMFGraph, Optimal CI, beta = 0.60
#===============================================================================
################################################################################

cat("HMFGraph, Optimal CI, beta = 0.60")

print_results(results_HMF_Z_b060,true_files, values = Values, round1=round_value1,round2=round_value2)


