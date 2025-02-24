################################################################################
################################################################################
################################################################################
################################################################################
#
# Bdgraph, cluster, results with alpha = CC
# 
# HMFGraph, alpha selection based on condition number constraint method
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
#HMF Optimal CI AND FDR, alpha = CC, cluster, n=35, p = 100
#===============================================================================
################################################################################


load(file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_adjacency.RData")


n <- dim(cluster_p_100_n_35_bdgraph_data)[1]
p <- dim(cluster_p_100_n_35_bdgraph_data)[2]
t1 <- dim(cluster_p_100_n_35_bdgraph_data)[3]



################################################################################
#===============================================================================
#HMF Optimal CI, alpha = CC
#===============================================================================
################################################################################


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_HMF <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph'), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  
  data_R <-cluster_p_100_n_35_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_35_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph_GEM(data_R, print_t=F)
  permutations <- HMFGraph_GEM_permutations(data_R, tulos)
  tulos_var <- HMFGraph_GEM_optimal_CI(tulos,permutations)
  
  
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  tulos_var <- HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=0.2)
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- c(results, admat[ lower.tri(admat,diag=T) ])
  
  tulos_var <-  HMFGraph_GEM_CI(tulos, CI=0.9)
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- c(results, admat[ lower.tri(admat,diag=T) ])
  
  
  
  results
  
}


stopCluster(cl)

results_HMF_Z <-results_HMF[1:((dim(results_HMF)[1])/3),]

results_HMF_Z_CC_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z_CC_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n35_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR_CC_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR_CC_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n35_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P_CC_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P_CC_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_HMF_P_CC_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P_CC_cluster_p100_n35_bdgraph_data.RData")






################################################################################
#===============================================================================
#HMF Optimal CI AND FDR, alpha = CC, cluster, n=75, p = 100
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

results_HMF <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph'), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  
  data_R <-cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]

  tulos <- HMFGraph_GEM(data_R, print_t=F)
  permutations <- HMFGraph_GEM_permutations(data_R, tulos)
  tulos_var <- HMFGraph_GEM_optimal_CI(tulos,permutations)
  
  
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  tulos_var <- HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=0.2)
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- c(results, admat[ lower.tri(admat,diag=T) ])
  
  tulos_var <-  HMFGraph_GEM_CI(tulos, CI=0.9)
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- c(results, admat[ lower.tri(admat,diag=T) ])
  
  
  
  results
  
}


stopCluster(cl)

results_HMF_Z <-results_HMF[1:((dim(results_HMF)[1])/3),]

results_HMF_Z_CC_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z_CC_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n75_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR_CC_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR_CC_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n75_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P_CC_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P_CC_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_HMF_P_CC_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P_CC_cluster_p100_n75_bdgraph_data.RData")





################################################################################
#===============================================================================
#HMF Optimal CI AND FDR, cluster, n=150, p = 100
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

results_HMF <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph'), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph_GEM(data_R, print_t=F)
  permutations <- HMFGraph_GEM_permutations(data_R, tulos)
  tulos_var <- HMFGraph_GEM_optimal_CI(tulos,permutations)
  
  
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  tulos_var <- HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=0.2)
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- c(results, admat[ lower.tri(admat,diag=T) ])
  
  tulos_var <-  HMFGraph_GEM_CI(tulos, CI=0.9)
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- c(results, admat[ lower.tri(admat,diag=T) ])
  
  
  
  results
  
}


stopCluster(cl)

results_HMF_Z <-results_HMF[1:((dim(results_HMF)[1])/3),]

results_HMF_Z_CC_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z_CC_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n150_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR_CC_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR_CC_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n150_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P_CC_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P_CC_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_HMF_P_CC_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P_CC_cluster_p100_n150_bdgraph_data.RData")


################################################################################
#===============================================================================
#HMF Optimal CI AND FDR, cluster, n=300, p = 100
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

results_HMF <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph'), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph_GEM(data_R, print_t=F)
  permutations <- HMFGraph_GEM_permutations(data_R, tulos)
  tulos_var <- HMFGraph_GEM_optimal_CI(tulos,permutations)
  
  
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  tulos_var <- HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=0.2)
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- c(results, admat[ lower.tri(admat,diag=T) ])
  
  tulos_var <-  HMFGraph_GEM_CI(tulos, CI=0.9)
  
  admat <- tulos_var$adjacency_matrix
  diag(admat) <- 0
  results <- c(results, admat[ lower.tri(admat,diag=T) ])
  
  
  
  results
  
}


stopCluster(cl)

results_HMF_Z <-results_HMF[1:((dim(results_HMF)[1])/3),]

results_HMF_Z_CC_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z_CC_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n300_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR_CC_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR_CC_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR_CC_cluster_p100_n300_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P_CC_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P_CC_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_HMF_P_CC_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P_CC_cluster_p100_n300_bdgraph_data.RData")







