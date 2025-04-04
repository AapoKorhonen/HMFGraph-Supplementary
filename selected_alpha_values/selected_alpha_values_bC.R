################################################################################
################################################################################
################################################################################
################################################################################
#
# bdgraph, cluster, results with alpha = CC
# 
#
################################################################################
################################################################################
################################################################################
################################################################################

library(parallel)
library(doSNOW)
library(foreach)
library(progress)

source("functions/functions_for_result_handeling.R")


################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = CC, cluster, n=35, p = 100
#===============================================================================
################################################################################


load(file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_adjacency.RData")


n <- dim(cluster_p_100_n_35_bdgraph_data)[1]
p <- dim(cluster_p_100_n_35_bdgraph_data)[2]
t1 <- dim(cluster_p_100_n_35_bdgraph_data)[3]





n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

alphas_bC_35 <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph'), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  
  data_R <-cluster_p_100_n_35_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_35_bdgraph_adjacency[,,i]
  
  alpha <- alpha_binary_search(data_R)
  alpha
  
  
}


stopCluster(cl)

save(alphas_bC_35, file="selected_alpha_values/results/alphas_bC_35.RData")



################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = CC, cluster, n=75, p = 100
#===============================================================================
################################################################################


load(file = "simulated_data/bdgraph/cluster_p_100_n_75_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_75_bdgraph_adjacency.RData")


n <- dim(cluster_p_100_n_75_bdgraph_data)[1]
p <- dim(cluster_p_100_n_75_bdgraph_data)[2]
t1 <- dim(cluster_p_100_n_75_bdgraph_data)[3]





n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

alphas_bC_75 <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph'), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  
  data_R <-cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]
  
  alpha <- alpha_binary_search(data_R)
  alpha
  
  
}

stopCluster(cl)

save(alphas_bC_75, file="selected_alpha_values/results/alphas_bC_75.RData")


################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = CC, cluster, n=150, p = 100
#===============================================================================
################################################################################


load(file = "simulated_data/bdgraph/cluster_p_100_n_150_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_150_bdgraph_adjacency.RData")


n <- dim(cluster_p_100_n_150_bdgraph_data)[1]
p <- dim(cluster_p_100_n_150_bdgraph_data)[2]
t1 <- dim(cluster_p_100_n_150_bdgraph_data)[3]

n_cores <- parallel::detectCores() - 1

cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

alphas_bC_150 <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph'), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  
  alpha <- alpha_binary_search(data_R)
  alpha
  
}

save(alphas_bC_150, file="selected_alpha_values/results/alphas_bC_150.RData")


################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = CC, cluster, n=300, p = 100
#===============================================================================
################################################################################


load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_adjacency.RData")


n <- dim(cluster_p_100_n_300_bdgraph_data)[1]
p <- dim(cluster_p_100_n_300_bdgraph_data)[2]
t1 <- dim(cluster_p_100_n_300_bdgraph_data)[3]





n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

alphas_bC_300 <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph'), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  alpha <- alpha_binary_search(data_R)
  alpha
}

stopCluster(cl)


save(alphas_bC_300, file="selected_alpha_values/results/alphas_bC_300.RData")

