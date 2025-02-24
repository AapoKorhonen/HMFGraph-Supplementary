################################################################################
################################################################################
################################################################################
################################################################################
#
# Results for BDgraph data, cluster
# p = 100, n = 35
#
################################################################################
################################################################################
################################################################################
################################################################################

library(parallel)
library(doSNOW)
library(foreach)
library(progress)
library(devtools)
#devtools::install_github("gleday/beam")
library(beam)
library(PCGII)
library(rags2ridges)
library(flare)
library(thav.glasso)
#devtools::install_github('apatrone2/thav.glasso')
#install.packages(c("MASS", "igraph", "stargazer", "huge", "matrixcalc", "glasso", "scalreg", "genscore", "scio", "ggplot2"))
library(HMFGraph)

source("functions/functions_for_result_handeling.R")
load(file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_35_bdgraph_adjacency.RData")


n <- dim(cluster_p_100_n_35_bdgraph_data)[1]
p <- dim(cluster_p_100_n_35_bdgraph_data)[2]
t1 <- dim(cluster_p_100_n_35_bdgraph_data)[3]




################################################################################
#===============================================================================
#Glasso
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

results_glasso <- foreach(i = 1:t1, .combine = 'cbind',.packages=c("pulsar", "huge", "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  result_glasso <- c()
  data_R <- cluster_p_100_n_35_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_35_bdgraph_adjacency[,,i]
  diag(adjacency_correct) <- 0
  
  
  lmax <- pulsar::getMaxCov(cov(data_R))
  lams <- pulsar::getLamPath(lmax, lmax*.05, len=40)
  bdgraphargs <- list(lambda=lams, verbose=FALSE, method="glasso")
  
  out.p    <- pulsar::pulsar(data_R, fun=huge, fargs=bdgraphargs, rep.num=20,
                             criterion='stars', lb.stars=TRUE, ub.stars=TRUE)
  
  fit.p    <- pulsar::refit(out.p)
  
  admat <-  adjacentMat(as.matrix(fit.p$refit$stars))
  diag(admat) <- 0
  
  result_glasso <- admat[ lower.tri(admat,diag=T) ]
  
  result_glasso
}

stopCluster(cl)


results_glasso_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_glasso, p = p)

save(results_glasso_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_glasso_cluster_p100_n35_bdgraph_data.RData")


################################################################################
#===============================================================================
# Beam
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


results_beam <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('beam', "igraph"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  results <- c()
  data_R <-cluster_p_100_n_35_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_35_bdgraph_adjacency[,,i]
  diag(adjacency_correct) <- 0
  
  
  fit <- beam(X =  data_R, type="conditional")
  sel <- beam.select(fit, method = "BH")
  
  myigraph <- ugraph(sel)
  admat <- as_adjacency_matrix(myigraph, sparse = F)
  
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
} 

stopCluster(cl)


results_beam_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_beam, p = p)

save(results_beam_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_beam_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_beam_cluster_p100_n35_bdgraph_data.RData")

################################################################################
#===============================================================================
# clevel
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


results_clevel <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('PCGII', "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  results <- c()
  data_R <-cluster_p_100_n_35_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_35_bdgraph_adjacency[,,i]
  
  
  CLEVEL.out = clevel(df = data_R)
  
  inference_out = inference(CLEVEL.out, alpha = 0.2)
  
  admat <- adjacentMat(inference_out)
  diag(admat) <- 0
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_clevel_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_clevel, p = p)

save(results_clevel_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_clevel_cluster_p100_n35_bdgraph_data.RData")


################################################################################
#===============================================================================
# clevel, alpha = default
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


results_clevel <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('PCGII', "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  results <- c()
  data_R <-cluster_p_100_n_35_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_35_bdgraph_adjacency[,,i]
  
  
  CLEVEL.out = clevel(df = data_R)
  
  inference_out = inference(CLEVEL.out)
  
  admat <- adjacentMat(inference_out)
  
  diag(admat) <- 0
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_clevel_d_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_clevel, p = p)

save(results_clevel_d_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_clevel_d_cluster_p100_n35_bdgraph_data.RData")

################################################################################
#===============================================================================
#THAV
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


results_THAV <- foreach(i = 1:t1, .combine = 'cbind',.packages=c("thav.glasso","rags2ridges"), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  data_R <-cluster_p_100_n_35_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_35_bdgraph_adjacency[,,i]
  
  # calculate the thAV
  thav.glasso::load_libraries()
  
  thav <- thav.glasso::thAV.estimator(data_R)
  
  thav[abs(thav)>0] <- 1 
  diag(thav) <- 0
  
  results <- thav[ lower.tri(thav,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_THAV_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_THAV,p)


save(results_THAV_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_THAV_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_THAV_cluster_p100_n35_bdgraph_data.RData")



################################################################################
#===============================================================================
# TIGER
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

results_TIGER <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('flare', "rags2ridges"), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  
  data_R <-cluster_p_100_n_35_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_35_bdgraph_adjacency[,,i]
  
  results_tiger <- flare::sugm(data = data_R,
                               method = "tiger",
                               sym = "or")
  admat <-   adjacentMat(as.matrix(results_tiger$path[5][[1]]))
  
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)

results_TIGER_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_TIGER, p = p)

save(results_TIGER_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_TIGER_cluster_p100_n35_bdgraph_data.RData")




################################################################################
#===============================================================================
# G_wishart
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


results_G_wishart <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('BDgraph'), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  data_R <-cluster_p_100_n_35_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_35_bdgraph_adjacency[,,i]
  
  
  bdgraph.obj = bdgraph( data =data_R,n=n,verbose = FALSE )
  
  admat <- bdgraph.obj$p_links
  
  admat[admat >= 0.5] <- 1
  
  admat[admat < 0.5] <- 0
  
  diag(admat) <- 0 
  
  admat <- admat + t(admat)
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)

results_G_wishart_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_G_wishart, p = p)

save(results_G_wishart_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_G_wishart_cluster_p100_n35_bdgraph_data.RData")




################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = p*10/(p*10+n)
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
  
  
  tulos <- HMFGraph_GEM(data_R, print_t=F, alpha = p*10/(p*10+n))
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

results_HMF_Z_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_HMF_Z_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z_cluster_p100_n35_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_HMF_FDR_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR_cluster_p100_n35_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_HMF_P_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P_cluster_p100_n35_bdgraph_data.RData")



################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = p*2/(p*2+n)
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

  
  tulos <- HMFGraph_GEM(data_R, print_t=F, alpha = p*2/(p*2+n))
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

results_HMF_Z2x_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z2x_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_HMF_Z2x_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z2x_cluster_p100_n35_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR2x_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR2x_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_HMF_FDR2x_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR2x_cluster_p100_n35_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P2x_cluster_p100_n35_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P2x_cluster_p100_n35_bdgraph_data, file="results/bdgraph/results_HMF_P2x_cluster_p100_n35_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P2x_cluster_p100_n35_bdgraph_data.RData")


################################################################################
################################################################################
################################################################################
################################################################################
#
# Results for BDgraph data, cluster
# p = 100, n = 75
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

load(file = "simulated_data/bdgraph/cluster_p_100_n_75_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_75_bdgraph_adjacency.RData")


n <- dim(cluster_p_100_n_75_bdgraph_data)[1]
p <- dim(cluster_p_100_n_75_bdgraph_data)[2]
t1 <- dim(cluster_p_100_n_75_bdgraph_data)[3]




################################################################################
#===============================================================================
#Glasso
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

results_glasso <- foreach(i = 1:t1, .combine = 'cbind',.packages=c("pulsar", "huge", "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  result_glasso <- c()
  data_R <- cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]
  diag(adjacency_correct) <- 0
  
  
  lmax <- pulsar::getMaxCov(cov(data_R))
  lams <- pulsar::getLamPath(lmax, lmax*.05, len=40)
  bdgraphargs <- list(lambda=lams, verbose=FALSE, method="glasso")
  
  
  out.p    <- pulsar::pulsar(data_R, fun=huge, fargs=bdgraphargs, rep.num=20,
                             criterion='stars', lb.stars=TRUE, ub.stars=TRUE)
  
  fit.p    <- pulsar::refit(out.p)
  
  admat <-  adjacentMat(as.matrix(fit.p$refit$stars))
  diag(admat) <- 0
  
  result_glasso <- admat[ lower.tri(admat,diag=T) ]
  
  result_glasso
}

stopCluster(cl)


results_glasso_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_glasso, p = p)

save(results_glasso_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_glasso_cluster_p100_n75_bdgraph_data.RData")


################################################################################
#===============================================================================
# Beam
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


results_beam <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('beam', "igraph"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  results <- c()
  data_R <-cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]
  diag(adjacency_correct) <- 0
  
  
  fit <- beam(X =  data_R, type="conditional")
  sel <- beam.select(fit, method = "BH")
  
  myigraph <- ugraph(sel)
  admat <- as_adjacency_matrix(myigraph, sparse = F)
  
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
} 

stopCluster(cl)


results_beam_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_beam, p = p)

save(results_beam_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_beam_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_beam_cluster_p100_n75_bdgraph_data.RData")

################################################################################
#===============================================================================
# clevel
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


results_clevel <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('PCGII', "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  results <- c()
  data_R <-cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]
  
  
  CLEVEL.out = clevel(df = data_R)
  
  inference_out = inference(CLEVEL.out, alpha = 0.2)
  
  admat <- adjacentMat(inference_out)
  diag(admat) <- 0
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_clevel_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_clevel, p = p)

save(results_clevel_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_clevel_cluster_p100_n75_bdgraph_data.RData")


################################################################################
#===============================================================================
# clevel, alpha = default
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


results_clevel <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('PCGII', "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  results <- c()
  data_R <-cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]
  
  
  CLEVEL.out = clevel(df = data_R)
  
  inference_out = inference(CLEVEL.out)
  
  admat <- adjacentMat(inference_out)
  
  diag(admat) <- 0
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_clevel_d_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_clevel, p = p)

save(results_clevel_d_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_clevel_d_cluster_p100_n75_bdgraph_data.RData")

################################################################################
#===============================================================================
#THAV
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


results_THAV <- foreach(i = 1:t1, .combine = 'cbind',.packages=c("thav.glasso","rags2ridges"), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  data_R <-cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]
  
  # calculate the thAV
  thav.glasso::load_libraries()
  
  thav <- thav.glasso::thAV.estimator(data_R)
  
  thav[abs(thav)>0] <- 1 
  diag(thav) <- 0
  
  results <- thav[ lower.tri(thav,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_THAV_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_THAV,p)


save(results_THAV_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_THAV_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_THAV_cluster_p100_n75_bdgraph_data.RData")



################################################################################
#===============================================================================
# TIGER
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

results_TIGER <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('flare', "rags2ridges"), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  
  data_R <-cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]
  
  results_tiger <- flare::sugm(data = data_R,
                               method = "tiger",
                               sym = "or")
  admat <-   adjacentMat(as.matrix(results_tiger$path[5][[1]]))
  
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)

results_TIGER_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_TIGER, p = p)

save(results_TIGER_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_TIGER_cluster_p100_n75_bdgraph_data.RData")




################################################################################
#===============================================================================
# G_wishart
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


results_G_wishart <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('BDgraph'), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  data_R <-cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]
  
  
  bdgraph.obj = bdgraph( data =data_R,n=n,verbose = FALSE )
  
  admat <- bdgraph.obj$p_links
  
  admat[admat >= 0.5] <- 1
  
  admat[admat < 0.5] <- 0
  
  diag(admat) <- 0 
  
  admat <- admat + t(admat)
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)

results_G_wishart_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_G_wishart, p = p)

save(results_G_wishart_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_G_wishart_cluster_p100_n75_bdgraph_data.RData")




################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = p*10/(p*10+n)
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
  
  
  data_R <-cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]
  
  
  tulos <- HMFGraph_GEM(data_R, print_t=F, alpha = p*10/(p*10+n))
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

results_HMF_Z_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_HMF_Z_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z_cluster_p100_n75_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_HMF_FDR_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR_cluster_p100_n75_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_HMF_P_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P_cluster_p100_n75_bdgraph_data.RData")



################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = p*2/(p*2+n)
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
  
  
  data_R <-cluster_p_100_n_75_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_75_bdgraph_adjacency[,,i]
  
  
  tulos <- HMFGraph_GEM(data_R, print_t=F, alpha = p*2/(p*2+n))
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

results_HMF_Z2x_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z2x_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_HMF_Z2x_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z2x_cluster_p100_n75_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR2x_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR2x_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_HMF_FDR2x_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR2x_cluster_p100_n75_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P2x_cluster_p100_n75_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P2x_cluster_p100_n75_bdgraph_data, file="results/bdgraph/results_HMF_P2x_cluster_p100_n75_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P2x_cluster_p100_n75_bdgraph_data.RData")



################################################################################
################################################################################
################################################################################
################################################################################
#
# Results for BDgraph data, cluster
# p = 100, n = 150
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

load(file = "simulated_data/bdgraph/cluster_p_100_n_150_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_150_bdgraph_adjacency.RData")


n <- dim(cluster_p_100_n_150_bdgraph_data)[1]
p <- dim(cluster_p_100_n_150_bdgraph_data)[2]
t1 <- dim(cluster_p_100_n_150_bdgraph_data)[3]




################################################################################
#===============================================================================
#Glasso
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

results_glasso <- foreach(i = 1:t1, .combine = 'cbind',.packages=c("pulsar", "huge", "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  result_glasso <- c()
  data_R <- cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  diag(adjacency_correct) <- 0
  
  
  lmax <- pulsar::getMaxCov(cov(data_R))
  lams <- pulsar::getLamPath(lmax, lmax*.05, len=40)
  bdgraphargs <- list(lambda=lams, verbose=FALSE, method="glasso")
  
  out.p    <- pulsar::pulsar(data_R, fun=huge, fargs=bdgraphargs, rep.num=20,
                             criterion='stars', lb.stars=TRUE, ub.stars=TRUE)
  
  fit.p    <- pulsar::refit(out.p)
  
  admat <-  adjacentMat(as.matrix(fit.p$refit$stars))
  diag(admat) <- 0
  
  result_glasso <- admat[ lower.tri(admat,diag=T) ]
  
  result_glasso
}

stopCluster(cl)


results_glasso_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_glasso, p = p)

save(results_glasso_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_glasso_cluster_p100_n150_bdgraph_data.RData")


################################################################################
#===============================================================================
# Beam
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


results_beam <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('beam', "igraph"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  results <- c()
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  diag(adjacency_correct) <- 0
  
  
  fit <- beam(X =  data_R, type="conditional")
  sel <- beam.select(fit, method = "BH")
  
  myigraph <- ugraph(sel)
  admat <- as_adjacency_matrix(myigraph, sparse = F)
  
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
} 

stopCluster(cl)


results_beam_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_beam, p = p)

save(results_beam_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_beam_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_beam_cluster_p100_n150_bdgraph_data.RData")

################################################################################
#===============================================================================
# clevel
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


results_clevel <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('PCGII', "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  results <- c()
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  
  
  CLEVEL.out = clevel(df = data_R)
  
  inference_out = inference(CLEVEL.out, alpha = 0.2)
  
  admat <- adjacentMat(inference_out)
  diag(admat) <- 0
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_clevel_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_clevel, p = p)

save(results_clevel_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_clevel_cluster_p100_n150_bdgraph_data.RData")


################################################################################
#===============================================================================
# clevel, alpha = default
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


results_clevel <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('PCGII', "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  results <- c()
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  
  
  CLEVEL.out = clevel(df = data_R)
  
  inference_out = inference(CLEVEL.out)
  
  admat <- adjacentMat(inference_out)
  
  diag(admat) <- 0
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_clevel_d_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_clevel, p = p)

save(results_clevel_d_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_clevel_d_cluster_p100_n150_bdgraph_data.RData")

################################################################################
#===============================================================================
#THAV
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


results_THAV <- foreach(i = 1:t1, .combine = 'cbind',.packages=c("thav.glasso","rags2ridges"), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  
  # calculate the thAV
  thav.glasso::load_libraries()
  
  thav <- thav.glasso::thAV.estimator(data_R)
  
  thav[abs(thav)>0] <- 1 
  diag(thav) <- 0
  
  results <- thav[ lower.tri(thav,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_THAV_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_THAV,p)


save(results_THAV_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_THAV_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_THAV_cluster_p100_n150_bdgraph_data.RData")



################################################################################
#===============================================================================
# TIGER
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

results_TIGER <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('flare', "rags2ridges"), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  
  results_tiger <- flare::sugm(data = data_R,
                               method = "tiger",
                               sym = "or")
  admat <-   adjacentMat(as.matrix(results_tiger$path[5][[1]]))
  
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)

results_TIGER_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_TIGER, p = p)

save(results_TIGER_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_TIGER_cluster_p100_n150_bdgraph_data.RData")




################################################################################
#===============================================================================
# G_wishart
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


results_G_wishart <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('BDgraph'), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  
  
  bdgraph.obj = bdgraph( data =data_R,n=n,verbose = FALSE )
  
  admat <- bdgraph.obj$p_links
  
  admat[admat >= 0.5] <- 1
  
  admat[admat < 0.5] <- 0
  
  diag(admat) <- 0 
  
  admat <- admat + t(admat)
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)

results_G_wishart_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_G_wishart, p = p)

save(results_G_wishart_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_G_wishart_cluster_p100_n150_bdgraph_data.RData")




################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = p*10/(p*10+n)
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
  
  
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph_GEM(data_R, print_t=F, alpha = p*10/(p*10+n))
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

results_HMF_Z_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_HMF_Z_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z_cluster_p100_n150_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_HMF_FDR_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR_cluster_p100_n150_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_HMF_P_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P_cluster_p100_n150_bdgraph_data.RData")



################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = p*2/(p*2+n)
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
  
  
  data_R <-cluster_p_100_n_150_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_150_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph_GEM(data_R, print_t=F, alpha = p*2/(p*2+n))
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

results_HMF_Z2x_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z2x_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_HMF_Z2x_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z2x_cluster_p100_n150_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR2x_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR2x_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_HMF_FDR2x_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR2x_cluster_p100_n150_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P2x_cluster_p100_n150_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P2x_cluster_p100_n150_bdgraph_data, file="results/bdgraph/results_HMF_P2x_cluster_p100_n150_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P2x_cluster_p100_n150_bdgraph_data.RData")



################################################################################
################################################################################
################################################################################
################################################################################
#
# Results for BDgraph data, cluster
# p = 100, n = 300
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

load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_adjacency.RData")


n <- dim(cluster_p_100_n_300_bdgraph_data)[1]
p <- dim(cluster_p_100_n_300_bdgraph_data)[2]
t1 <- dim(cluster_p_100_n_300_bdgraph_data)[3]




################################################################################
#===============================================================================
#Glasso
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

results_glasso <- foreach(i = 1:t1, .combine = 'cbind',.packages=c("pulsar", "huge", "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  result_glasso <- c()
  data_R <- cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  diag(adjacency_correct) <- 0
  
  
  lmax <- pulsar::getMaxCov(cov(data_R))
  lams <- pulsar::getLamPath(lmax, lmax*.05, len=40)
  bdgraphargs <- list(lambda=lams, verbose=FALSE, method="glasso")
  
  
  out.p    <- pulsar::pulsar(data_R, fun=huge, fargs=bdgraphargs, rep.num=20,
                             criterion='stars', lb.stars=TRUE, ub.stars=TRUE)
  
  fit.p    <- pulsar::refit(out.p)
  
  admat <-  adjacentMat(as.matrix(fit.p$refit$stars))
  diag(admat) <- 0
  
  result_glasso <- admat[ lower.tri(admat,diag=T) ]
  
  result_glasso
}

stopCluster(cl)


results_glasso_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_glasso, p = p)

save(results_glasso_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_glasso_cluster_p100_n300_bdgraph_data.RData")


################################################################################
#===============================================================================
# Beam
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


results_beam <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('beam', "igraph"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  
  results <- c()
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  diag(adjacency_correct) <- 0
  
  
  fit <- beam(X =  data_R, type="conditional")
  sel <- beam.select(fit, method = "BH")
  
  myigraph <- ugraph(sel)
  admat <- as_adjacency_matrix(myigraph, sparse = F)
  
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
} 

stopCluster(cl)


results_beam_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_beam, p = p)

save(results_beam_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_beam_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_beam_cluster_p100_n300_bdgraph_data.RData")

################################################################################
#===============================================================================
# clevel
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


results_clevel <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('PCGII', "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  results <- c()
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  
  CLEVEL.out = clevel(df = data_R)
  
  inference_out = inference(CLEVEL.out, alpha = 0.2)
  
  admat <- adjacentMat(inference_out)
  diag(admat) <- 0
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_clevel_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_clevel, p = p)

save(results_clevel_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_clevel_cluster_p100_n300_bdgraph_data.RData")


################################################################################
#===============================================================================
# clevel, alpha = default
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


results_clevel <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('PCGII', "rags2ridges"), .options.snow = opts) %dopar% {
  set.seed(i)
  
  results <- c()
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  
  CLEVEL.out = clevel(df = data_R)
  
  inference_out = inference(CLEVEL.out)
  
  admat <- adjacentMat(inference_out)
  
  diag(admat) <- 0
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_clevel_d_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_clevel, p = p)

save(results_clevel_d_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_clevel_d_cluster_p100_n300_bdgraph_data.RData")

################################################################################
#===============================================================================
#THAV
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


results_THAV <- foreach(i = 1:t1, .combine = 'cbind',.packages=c("thav.glasso","rags2ridges"), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  # calculate the thAV
  thav.glasso::load_libraries()
  
  thav <- thav.glasso::thAV.estimator(data_R)
  
  thav[abs(thav)>0] <- 1 
  diag(thav) <- 0
  
  results <- thav[ lower.tri(thav,diag=T) ]
  
  results
  
}

stopCluster(cl)


results_THAV_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_THAV,p)


save(results_THAV_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_THAV_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_THAV_cluster_p100_n300_bdgraph_data.RData")



################################################################################
#===============================================================================
# TIGER
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

results_TIGER <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('flare', "rags2ridges"), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  results_tiger <- flare::sugm(data = data_R,
                               method = "tiger",
                               sym = "or")
  admat <-   adjacentMat(as.matrix(results_tiger$path[5][[1]]))
  
  diag(admat) <- 0
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)

results_TIGER_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_TIGER, p = p)

save(results_TIGER_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_TIGER_cluster_p100_n300_bdgraph_data.RData")




################################################################################
#===============================================================================
# G_wishart
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


results_G_wishart <- foreach(i = 1:t1, .combine = 'cbind',.packages=c('BDgraph'), .options.snow = opts) %dopar% {
  
  set.seed(i)
  
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  
  bdgraph.obj = bdgraph( data =data_R,n=n,verbose = FALSE )
  
  admat <- bdgraph.obj$p_links
  
  admat[admat >= 0.5] <- 1
  
  admat[admat < 0.5] <- 0
  
  diag(admat) <- 0 
  
  admat <- admat + t(admat)
  
  results <- admat[ lower.tri(admat,diag=T) ]
  
  results
  
}

stopCluster(cl)

results_G_wishart_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_G_wishart, p = p)

save(results_G_wishart_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_G_wishart_cluster_p100_n300_bdgraph_data.RData")




################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = p*10/(p*10+n)
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
  
  
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph_GEM(data_R, print_t=F, alpha = p*10/(p*10+n))
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

results_HMF_Z_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_HMF_Z_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z_cluster_p100_n300_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_HMF_FDR_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR_cluster_p100_n300_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_HMF_P_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P_cluster_p100_n300_bdgraph_data.RData")



################################################################################
#===============================================================================
#HMF Optimal Z AND FDR, alpha = p*2/(p*2+n)
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
  
  
  data_R <-cluster_p_100_n_300_bdgraph_data[,,i]
  adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,i]
  
  tulos <- HMFGraph_GEM(data_R, print_t=F, alpha = p*2/(p*2+n))
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

results_HMF_Z2x_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_HMF_Z,p)


save(results_HMF_Z2x_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_HMF_Z2x_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_Z2x_cluster_p100_n300_bdgraph_data.RData")


results_HMF_FDR <-results_HMF[(dim(results_HMF)[1]/3 + 1):(dim(results_HMF)[1]*(2/3)) ,]


results_HMF_FDR2x_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_HMF_FDR,p)

save(results_HMF_FDR2x_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_HMF_FDR2x_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_FDR2x_cluster_p100_n300_bdgraph_data.RData")



results_HMF_P <-results_HMF[ (dim(results_HMF)[1]*(2/3) + 1):(dim(results_HMF)[1]), ]


results_HMF_P2x_cluster_p100_n300_bdgraph_data  <- vector_to_array(results_HMF_P,p)

save(results_HMF_P2x_cluster_p100_n300_bdgraph_data, file="results/bdgraph/results_HMF_P2x_cluster_p100_n300_bdgraph_data.RData")

load(file="results/bdgraph/results_HMF_P2x_cluster_p100_n300_bdgraph_data.RData")

