################################################################################
################################################################################
################################################################################
################################################################################
#
# Bdgraph, cluster
# 
# BGGM
#
################################################################################
################################################################################
################################################################################
################################################################################

library(parallel)
library(doSNOW)
library(foreach)
library(progress)
library(BGGM)

source("functions/functions_for_result_handeling.R")



################################################################################
#===============================================================================
#BGGM, n=150, p = 100
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

results_BGGM <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('BGGM'), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  
  res <- BGGM::estimate(data_R)
  E <- BGGM::select(res)
  
  admat <- E$adj
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  
  
  results
  
}


stopCluster(cl)

results_BGGM_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_BGGM,p)

save(results_BGGM_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_BGGM_cluster_p100_n150_bdgraph_data.RData")


################################################################################
#===============================================================================
#BGGM, n=300, p = 100
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

results_BGGM <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('BGGM'), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  res <- BGGM::estimate(data_R)
  E <- BGGM::select(res)
  
  admat <- E$adj
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  
  
  results
  
}


stopCluster(cl)

results_BGGM_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_BGGM,p)

save(results_BGGM_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_BGGM_cluster_p100_n300_bdgraph_data.RData")





