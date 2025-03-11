################################################################################
#===============================================================================
# This file is for printing out the results, bdgraph, cluster
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

Values <- c(15,16,4,14) # MCC, FDR, F1, TPR 

round_value1 <- 2  # how many digits will be showed for the Mean 
round_value2 <- 2  # number of digits for SD


#===============================================================================
# Loading all results
#===============================================================================

load(file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_75_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_150_bdgraph_adjacency.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_adjacency.RData")

true_files <- list(cluster_p_100_n_35_bdgraph_adjacency,
                   cluster_p_100_n_75_bdgraph_adjacency,
                   cluster_p_100_n_150_bdgraph_adjacency,
                   cluster_p_100_n_300_bdgraph_adjacency)

load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n300_bdgraph_data.RData")

results_HMF_Z_CC <- list(results_HMF_Z_CC_cluster_p100_n35_bdgraph_data,
                         results_HMF_Z_CC_cluster_p100_n75_bdgraph_data,
                         results_HMF_Z_CC_cluster_p100_n150_bdgraph_data,
                         results_HMF_Z_CC_cluster_p100_n300_bdgraph_data)

load(file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n300_bdgraph_data.RData")

results_HMF_FDR_CC <- list(results_HMF_FDR_CC_cluster_p100_n35_bdgraph_data,
                           results_HMF_FDR_CC_cluster_p100_n75_bdgraph_data,
                           results_HMF_FDR_CC_cluster_p100_n150_bdgraph_data,
                           results_HMF_FDR_CC_cluster_p100_n300_bdgraph_data)

load(file="results/bdgraph/results_HMF_P_CC_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_P_CC_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_P_CC_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_HMF_P_CC_cluster_p100_n300_bdgraph_data.RData")

results_HMF_P_CC <- list(results_HMF_P_CC_cluster_p100_n35_bdgraph_data,
                         results_HMF_P_CC_cluster_p100_n75_bdgraph_data,
                         results_HMF_P_CC_cluster_p100_n150_bdgraph_data,
                         results_HMF_P_CC_cluster_p100_n300_bdgraph_data)


load(file="results/bdgraph/results_glasso_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_glasso_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_glasso_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_glasso_cluster_p100_n300_bdgraph_data.RData")

results_glasso <- list(results_glasso_cluster_p100_n35_bdgraph_data,
                       results_glasso_cluster_p100_n75_bdgraph_data,
                       results_glasso_cluster_p100_n150_bdgraph_data,
                       results_glasso_cluster_p100_n300_bdgraph_data)


load(file="results/bdgraph/results_beam_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_beam_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_beam_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_beam_cluster_p100_n300_bdgraph_data.RData")

results_beam <- list(results_beam_cluster_p100_n35_bdgraph_data,
                     results_beam_cluster_p100_n75_bdgraph_data,
                     results_beam_cluster_p100_n150_bdgraph_data,
                     results_beam_cluster_p100_n300_bdgraph_data)


load(file="results/bdgraph/results_G_wishart_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_G_wishart_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_G_wishart_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_G_wishart_cluster_p100_n300_bdgraph_data.RData")

results_G_wishart <- list(results_G_wishart_cluster_p100_n35_bdgraph_data,
                          results_G_wishart_cluster_p100_n75_bdgraph_data,
                          results_G_wishart_cluster_p100_n150_bdgraph_data,
                          results_G_wishart_cluster_p100_n300_bdgraph_data)

load(file="results/bdgraph/results_clevel_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_clevel_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_clevel_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_clevel_cluster_p100_n300_bdgraph_data.RData")

results_clevel <- list(results_clevel_cluster_p100_n35_bdgraph_data,
                       results_clevel_cluster_p100_n75_bdgraph_data,
                       results_clevel_cluster_p100_n150_bdgraph_data,
                       results_clevel_cluster_p100_n300_bdgraph_data)


load(file="results/bdgraph/results_clevel_d_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_clevel_d_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_clevel_d_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_clevel_d_cluster_p100_n300_bdgraph_data.RData")

results_clevel_d <- list(results_clevel_d_cluster_p100_n35_bdgraph_data,
                         results_clevel_d_cluster_p100_n75_bdgraph_data,
                         results_clevel_d_cluster_p100_n150_bdgraph_data,
                         results_clevel_d_cluster_p100_n300_bdgraph_data)

load(file="results/bdgraph/results_THAV_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_THAV_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_THAV_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_THAV_cluster_p100_n300_bdgraph_data.RData")

results_THAV <- list(results_THAV_cluster_p100_n35_bdgraph_data,
                     results_THAV_cluster_p100_n75_bdgraph_data,
                     results_THAV_cluster_p100_n150_bdgraph_data,
                     results_THAV_cluster_p100_n300_bdgraph_data)

load(file="results/bdgraph/results_TIGER_cluster_p100_n35_bdgraph_data.RData")
load(file="results/bdgraph/results_TIGER_cluster_p100_n75_bdgraph_data.RData")
load(file="results/bdgraph/results_TIGER_cluster_p100_n150_bdgraph_data.RData")
load(file="results/bdgraph/results_TIGER_cluster_p100_n300_bdgraph_data.RData")

results_TIGER <- list(results_TIGER_cluster_p100_n35_bdgraph_data,
                      results_TIGER_cluster_p100_n75_bdgraph_data,
                      results_TIGER_cluster_p100_n150_bdgraph_data,
                      results_TIGER_cluster_p100_n300_bdgraph_data)




################################################################################
################################################################################
# RESULTS
################################################################################
################################################################################



################################################################################
#===============================================================================
# HMFGraph, Optimal CI
#===============================================================================
################################################################################

cat("HMFGraph, Optimal CI")

print_results(results_HMF_Z_CC,true_files, values = Values, round1=round_value1,round2=round_value2)

################################################################################
#===============================================================================
# HMFGraph, CI = 0.90
#===============================================================================
################################################################################

cat("HMFGraph, CI = 0.90")

print_results(results_HMF_P_CC,true_files, values = Values, round1=round_value1,round2=round_value2)

################################################################################
#===============================================================================
# HMFGraph, FDR = 0.2
#===============================================================================
################################################################################

cat("HMFGraph, FDR = 0.2")

print_results(results_HMF_FDR_CC,true_files, values = Values, round1=round_value1,round2=round_value2)

################################################################################
#===============================================================================
# GLasso, STARS
#===============================================================================
################################################################################

cat("Glasso")

print_results(results_glasso,true_files, values = Values, round1=round_value1,round2=round_value2)

################################################################################
#===============================================================================
# BEAM
#===============================================================================
################################################################################

cat("BEAM")

print_results(results_beam,true_files, values = Values, round1=round_value1,round2=round_value2)

################################################################################
#===============================================================================
# G-wishart
#===============================================================================
################################################################################

cat("G-wishart")

print_results(results_G_wishart,true_files, values = Values, round1=round_value1,round2=round_value2)


################################################################################
#===============================================================================
# CLEVEL, alpha (0.20)
#===============================================================================
################################################################################

cat("CLEVEL")

print_results(results_clevel,true_files, values = Values, round1=round_value1,round2=round_value2)

################################################################################
#===============================================================================
# CLEVEL, default alpha (0.05)
#===============================================================================
################################################################################

cat("CLEVEL, default alpha (0.05)")

print_results(results_clevel_d,true_files, values = Values, round1=round_value1,round2=round_value2)


################################################################################
#===============================================================================
# THAV
#===============================================================================
################################################################################

cat("THAV")

print_results(results_THAV,true_files, values = Values, round1=round_value1,round2=round_value2)

################################################################################
#===============================================================================
# TIGER
#===============================================================================
################################################################################

cat("TIGER")

print_results(results_TIGER,true_files, values = Values, round1=round_value1,round2=round_value2)





