
################################################################################
#===============================================================================
# This file is for plotting out the results, bdgraph, scale_free, with multiple beta values
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

value <- 16 # MCC, FDR, F1, TPR 

round_value1 <- 2  # how many digits will be showed for the Mean 
round_value2 <- 2  # number of digits for SD


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

#===============================================================================
# calculating all results
#===============================================================================

b095_results <- matrix(1, ncol=16, nrow=4)
x <- c(35,75,150,300)

b095_results[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095[[1]],true_files[[1]]))
b095_results[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095[[2]],true_files[[2]]))
b095_results[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095[[3]],true_files[[3]]))
b095_results[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b095[[4]],true_files[[4]]))



b090_results <- matrix(1, ncol=16, nrow=4)

b090_results[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC[[1]],true_files[[1]]))
b090_results[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC[[2]],true_files[[2]]))
b090_results[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC[[3]],true_files[[3]]))
b090_results[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_CC[[4]],true_files[[4]]))



b080_results <- matrix(1, ncol=16, nrow=4)

b080_results[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080[[1]],true_files[[1]]))
b080_results[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080[[2]],true_files[[2]]))
b080_results[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080[[3]],true_files[[3]]))
b080_results[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b080[[4]],true_files[[4]]))

b070_results <- matrix(1, ncol=16, nrow=4)


b070_results[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070[[1]],true_files[[1]]))
b070_results[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070[[2]],true_files[[2]]))
b070_results[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070[[3]],true_files[[3]]))
b070_results[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b070[[4]],true_files[[4]]))


b060_results <- matrix(1, ncol=16, nrow=4)


b060_results[1,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060[[1]],true_files[[1]]))
b060_results[2,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060[[2]],true_files[[2]]))
b060_results[3,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060[[3]],true_files[[3]]))
b060_results[4,] <- colMeans(calculate_results_from_file(results_HMF_Z_b060[[4]],true_files[[4]]))


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
# 15 = Averace scale_freeing coefficients (ACC), 
# 16 = NMI

names <- c("accuracy", "bal-accuracy", "MCC", "F1", "TPR", "TNR", "PPV","NPV", "FNR",
           "FPR","FOR", "LRp", "LRn", "FDR", "ACC", "NMI")

value <- 4 # MCC, FDR, F1, TPR 


plot(x=x, y=b095_results[,value], type="l", ylim=c(0,1), ylab = paste0(names[value]), xlab="n")
lines(x=x,y=b090_results[,value],col="red")
lines(x=x,y=b080_results[,value],col="purple")
lines(x=x,y=b070_results[,value],col="green")
lines(x=x,y=b060_results[,value],col="brown")