##########################################################################
# 
# FDR-control, Demonstration, with multiple Beta values, fixed alpha values
#
##########################################################################


library(parallel)
library(doSNOW)
library(foreach)
library(progress)

source("functions/functions_for_result_handeling.R")


###########################################################################################
#
# Huge, scale_free, p = 100, n = 300
#
###########################################################################################

load(file = "simulated_data/huge/scale_free_p_100_n_300_huge_data.RData")
load(file = "simulated_data/huge/scale_free_p_100_n_300_huge_adjacency.RData")


n <- dim(scale_free_p_100_n_300_huge_data)[1]
p <- dim(scale_free_p_100_n_300_huge_data)[2]
t1 <- dim(scale_free_p_100_n_300_huge_data)[3]
real_FDRss1 <- matrix(0,ncol=100,nrow=50)



#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.95
#=============================================================================================
#=============================================================================================

n_cores <- parallel::detectCores() - 1

cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_095_hsc <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- scale_free_p_100_n_300_huge_data[,,i]
  adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R,beta = 0.95 ,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}

stopCluster(cl)

#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.9
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_090_hsc <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- scale_free_p_100_n_300_huge_data[,,i]
  adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.9,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)


#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.8
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_080_hsc <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- scale_free_p_100_n_300_huge_data[,,i]
  adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.8,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)

#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.7
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_070_hsc <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- scale_free_p_100_n_300_huge_data[,,i]
  adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta =  0.7,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)


#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.6
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_060_hsc <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- scale_free_p_100_n_300_huge_data[,,i]
  adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta =  0.6,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)




###########################################################################################
#
# Huge, Cluster, p = 100, n = 300
#
###########################################################################################

load(file = "simulated_data/huge/cluster_p_100_n_300_huge_data.RData")
load(file = "simulated_data/huge/cluster_p_100_n_300_huge_adjacency.RData")


n <- dim(cluster_p_100_n_300_huge_data)[1]
p <- dim(cluster_p_100_n_300_huge_data)[2]
t1 <- dim(cluster_p_100_n_300_huge_data)[3]


#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.95
#=============================================================================================
#=============================================================================================

n_cores <- parallel::detectCores() - 1

cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_095_hC <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- cluster_p_100_n_300_huge_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R,beta = 0.95,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}

stopCluster(cl)

#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.90
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_090_hC <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- cluster_p_100_n_300_huge_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.9,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)


#=============================================================================================
#=============================================================================================
#  alpha = 0.9, beta = 0.80
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_080_hC <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- cluster_p_100_n_300_huge_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.80 ,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)

#=============================================================================================
#=============================================================================================
#  alpha = 0.9, beta = 0.70
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_070_hC <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- cluster_p_100_n_300_huge_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.7,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)


#=============================================================================================
#=============================================================================================
#alpha = 0.9, beta = 0.60
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_060_hC <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- cluster_p_100_n_300_huge_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.6,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)

###########################################################################################
#
# Bdgraph, Cluster, p = 100, n = 300
#
###########################################################################################

load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_adjacency.RData")


n <- dim(cluster_p_100_n_300_bdgraph_data)[1]
p <- dim(cluster_p_100_n_300_bdgraph_data)[2]
t1 <- dim(cluster_p_100_n_300_bdgraph_data)[3]


#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.95
#=============================================================================================
#=============================================================================================

n_cores <- parallel::detectCores() - 1

cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_095_bC <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R,beta = 0.95,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}

stopCluster(cl)

#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.90
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_090_bC <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R,beta = 0.90,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)


#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.80
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_080_bC <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.80, print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)

#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.70
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_070_bC <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.70,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)


#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.60
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_060_bC <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.60,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)

###########################################################################################
#
# Bdgraph, scale_free, p = 100, n = 300
#
###########################################################################################

load(file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_adjacency.RData")


n <- dim(scale_free_p_100_n_300_bdgraph_data)[1]
p <- dim(scale_free_p_100_n_300_bdgraph_data)[2]
t1 <- dim(scale_free_p_100_n_300_bdgraph_data)[3]


#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.95
#=============================================================================================
#=============================================================================================

n_cores <- parallel::detectCores() - 1

cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_095_bsc <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- scale_free_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R,beta = 0.95,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}

stopCluster(cl)

#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.90
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_090_bsc <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- scale_free_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R,beta = 0.90,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)


#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.80
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_080_bsc <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- scale_free_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.80,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)

#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.70
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_070_bsc <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- scale_free_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.70,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)


#=============================================================================================
#=============================================================================================
# alpha = 0.9, beta = 0.60
#=============================================================================================
#=============================================================================================


n_cores <- parallel::detectCores() - 1


cl <- parallel::makeCluster( min(n_cores, t1) , type = "SOCK")

doSNOW::registerDoSNOW(cl)

pb <- progress::progress_bar$new(format = " :percent [:bar] :elapsed | eta: :eta",
                                 total = t1 +1 , width = 80)

progress <- function() pb$tick()

opts <- list(progress = progress)

pb$tick()

results_FDR_a_080_b_060_bsc <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('HMFGraph',"rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  data_R <- scale_free_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph::HMFGraph_GEM(alpha=0.80,data_R, beta = 0.60,print_t=F)
  permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
  
  target_FDRs <- seq(0,0.99,by=0.01)
  real_FDRs <- c()
  for(ii in 1:100){
    tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR=target_FDRs[ii])
    
    
    cm <- tarkkuus( adjacentMat(round(adjacency_correct,5)) , adjacentMat(tulos_var$adjacency_matrix))
    
    real_FDRs[ii] <- calculate_scores(cm)$FDR
  }
  is.na(real_FDRs) <- 0
  real_FDRs
}


stopCluster(cl)


#=============================================================================================
#=============================================================================================

#=============================================================================================
#=============================================================================================

#=============================================================================================
#=============================================================================================


save(results_FDR_a_080_b_095_hsc, file="FDR_control/beta/results/results_FDR_a_080_b_095_hsc.RData")
save(results_FDR_a_080_b_090_hsc, file="FDR_control/beta/results/results_FDR_a_080_b_090_hsc.RData")
save(results_FDR_a_080_b_080_hsc, file="FDR_control/beta/results/results_FDR_a_080_b_080_hsc.RData")
save(results_FDR_a_080_b_070_hsc, file="FDR_control/beta/results/results_FDR_a_080_b_070_hsc.RData")
save(results_FDR_a_080_b_060_hsc, file="FDR_control/beta/results/results_FDR_a_080_b_060_hsc.RData")

save(results_FDR_a_080_b_095_hC, file="FDR_control/beta/results/results_FDR_a_080_b_095_hC.RData")
save(results_FDR_a_080_b_090_hC, file="FDR_control/beta/results/results_FDR_a_080_b_090_hC.RData")
save(results_FDR_a_080_b_080_hC, file="FDR_control/beta/results/results_FDR_a_080_b_080_hC.RData")
save(results_FDR_a_080_b_070_hC, file="FDR_control/beta/results/results_FDR_a_080_b_070_hC.RData")
save(results_FDR_a_080_b_060_hC, file="FDR_control/beta/results/results_FDR_a_080_b_060_hC.RData")

save(results_FDR_a_080_b_095_bsc, file="FDR_control/beta/results/results_FDR_a_080_b_095_bsc.RData")
save(results_FDR_a_080_b_090_bsc, file="FDR_control/beta/results/results_FDR_a_080_b_090_bsc.RData")
save(results_FDR_a_080_b_080_bsc, file="FDR_control/beta/results/results_FDR_a_080_b_080_bsc.RData")
save(results_FDR_a_080_b_070_bsc, file="FDR_control/beta/results/results_FDR_a_080_b_070_bsc.RData")
save(results_FDR_a_080_b_060_bsc, file="FDR_control/beta/results/results_FDR_a_080_b_060_bsc.RData")

save(results_FDR_a_080_b_095_bC, file="FDR_control/beta/results/results_FDR_a_080_b_095_bC.RData")
save(results_FDR_a_080_b_090_bC, file="FDR_control/beta/results/results_FDR_a_080_b_090_bC.RData")
save(results_FDR_a_080_b_080_bC, file="FDR_control/beta/results/results_FDR_a_080_b_080_bC.RData")
save(results_FDR_a_080_b_070_bC, file="FDR_control/beta/results/results_FDR_a_080_b_070_bC.RData")
save(results_FDR_a_080_b_060_bC, file="FDR_control/beta/results/results_FDR_a_080_b_060_bC.RData")

