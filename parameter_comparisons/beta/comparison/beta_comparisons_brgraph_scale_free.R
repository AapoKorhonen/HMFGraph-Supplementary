################################################################################
#===============================================================================
# This file is for printing out the results, bdgraph, scale_free, with multiple beta values
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
# 15 = Averace scale_freeing coefficients (ACC), 
# 16 = NMI

Values <- c(3,14,4,5) # MCC, FDR, F1, TPR 

round_value1 <- 3  # how many digits will be showed for the Mean 
round_value2 <- 3  # number of digits for SD


#===============================================================================
# Loading all results
#===============================================================================

load(file = "simulated_data/bdgraph/scale_free_p_100_n_35_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_75_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_150_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_adjacency.RData")

true_files <- list(scale_free_p_100_n_35_bdgraph_adjacency,
                   scale_free_p_100_n_75_bdgraph_adjacency,
                   scale_free_p_100_n_150_bdgraph_adjacency,
                   scale_free_p_100_n_300_bdgraph_adjacency)

# beta = 0.9

load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_CC <- list(results_HMF_Z_CC_scale_free_p100_n35_bdgraph_data,
                         results_HMF_Z_CC_scale_free_p100_n75_bdgraph_data,
                         results_HMF_Z_CC_scale_free_p100_n150_bdgraph_data,
                         results_HMF_Z_CC_scale_free_p100_n300_bdgraph_data)


load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_scale_free_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_scale_free_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_scale_free_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b095_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_b095 <- list(results_HMF_Z_b095_scale_free_p100_n35_bdgraph_data,
                           results_HMF_Z_b095_scale_free_p100_n75_bdgraph_data,
                           results_HMF_Z_b095_scale_free_p100_n150_bdgraph_data,
                           results_HMF_Z_b095_scale_free_p100_n300_bdgraph_data)



load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_scale_free_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_scale_free_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_scale_free_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b080_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_b080 <- list(results_HMF_Z_b080_scale_free_p100_n35_bdgraph_data,
                           results_HMF_Z_b080_scale_free_p100_n75_bdgraph_data,
                           results_HMF_Z_b080_scale_free_p100_n150_bdgraph_data,
                           results_HMF_Z_b080_scale_free_p100_n300_bdgraph_data)


load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_scale_free_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_scale_free_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_scale_free_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b070_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_b070 <- list(results_HMF_Z_b070_scale_free_p100_n35_bdgraph_data,
                           results_HMF_Z_b070_scale_free_p100_n75_bdgraph_data,
                           results_HMF_Z_b070_scale_free_p100_n150_bdgraph_data,
                           results_HMF_Z_b070_scale_free_p100_n300_bdgraph_data)

load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_scale_free_p100_n35_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_scale_free_p100_n75_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_scale_free_p100_n150_bdgraph_data.RData")
load(file="parameter_comparisons/beta/results/bdgraph/results_HMF_Z_b060_scale_free_p100_n300_bdgraph_data.RData")

results_HMF_Z_b060 <- list(results_HMF_Z_b060_scale_free_p100_n35_bdgraph_data,
                           results_HMF_Z_b060_scale_free_p100_n75_bdgraph_data,
                           results_HMF_Z_b060_scale_free_p100_n150_bdgraph_data,
                           results_HMF_Z_b060_scale_free_p100_n300_bdgraph_data)

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


