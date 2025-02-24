
library(BGGM)
library(BDgraph)
library(ggplot2)

library(MCMCpack)
library("qgraph")
library("MPsychoR")


library(philentropy)
library(saturnin)
library(beam)
library(PRROC)
library(PCGII)


source("libraries/libraries.r")

source("functions/conf_matrix_scores.R")

source("functions/confusion_matrix.R")

source("functions/Data_generator.R")

source("functions/adjacency_matrix.R")

load(file = "simulated_data/huge/scale_free_p_100_n_300_huge_data.RData")
load(file = "simulated_data/huge/scale_free_p_100_n_300_huge_adjacency.RData")

load(file = "simulated_data/huge/cluster_p_100_n_300_huge_data.RData")
load(file = "simulated_data/huge/cluster_p_100_n_300_huge_adjacency.RData")

load(file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_adjacency.RData")

load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_adjacency.RData")


par(mfrow=c(1,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
plot_adjacency_matrix(adjacency_correct,title="Huge, Scale-free")
data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
plot_adjacency_matrix(adjacency_correct,title="Huge, Cluster")
data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
plot_adjacency_matrix(adjacency_correct,title="bdgraph, Scale-free")
data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
plot_adjacency_matrix(adjacency_correct,title="bdgraph, Cluster")

n <- dim(scale_free_p_100_n_300_huge_data)[1]
p <- dim(scale_free_p_100_n_300_huge_data)[2]

sum(cluster_p_100_n_300_huge_adjacency[,,1])/2

sum(cluster_p_100_n_300_bdgraph_adjacency[,,1])/2

#========================
# GWISHART
#========================

par(mfrow=c(1,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]

bdgraph.obj = bdgraph( data =data_R,n=n,verbose = FALSE )
admat <- bdgraph.obj$p_links
admat[admat >= 0.5] <- 1
admat[admat < 0.5] <- 0
diag(admat) <- 0 
admat <- admat + t(admat)
plot_adjacency_matrix(admat,title="G-WISHART")

data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]

bdgraph.obj = bdgraph( data =data_R,n=n,verbose = FALSE )
admat <- bdgraph.obj$p_links
admat[admat >= 0.5] <- 1
admat[admat < 0.5] <- 0
diag(admat) <- 0 
admat <- admat + t(admat)
plot_adjacency_matrix(admat,title="G-WISHART")

data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]

bdgraph.obj = bdgraph( data =data_R,n=n,verbose = FALSE )
admat <- bdgraph.obj$p_links
admat[admat >= 0.5] <- 1
admat[admat < 0.5] <- 0
diag(admat) <- 0 
admat <- admat + t(admat)
plot_adjacency_matrix(admat,title="G-WISHART")

data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]

bdgraph.obj = bdgraph( data =data_R,n=n,verbose = FALSE )
admat <- bdgraph.obj$p_links
admat[admat >= 0.5] <- 1
admat[admat < 0.5] <- 0
diag(admat) <- 0 
admat <- admat + t(admat)
plot_adjacency_matrix(admat,title="G-WISHART")


#========================
# TIGER
#========================
library(flare)
library(rags2ridges)

par(mfrow=c(1,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]


results_tiger <- flare::sugm(data = data_R,
                             method = "tiger",
                             sym = "or")
admat <-   adjacentMat(as.matrix(results_tiger$path[5][[1]]))
diag(admat) <- 0
plot_adjacency_matrix(admat,title="TIGER")



data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]

results_tiger <- flare::sugm(data = data_R,
                             method = "tiger",
                             sym = "or")
admat <-   adjacentMat(as.matrix(results_tiger$path[5][[1]]))
diag(admat) <- 0
plot_adjacency_matrix(admat,title="TIGER")

data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]

results_tiger <- flare::sugm(data = data_R,
                             method = "tiger",
                             sym = "or")
admat <-   adjacentMat(as.matrix(results_tiger$path[5][[1]]))
diag(admat) <- 0
plot_adjacency_matrix(admat,title="TIGER")

data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]

results_tiger <- flare::sugm(data = data_R,
                             method = "tiger",
                             sym = "or")
admat <-   adjacentMat(as.matrix(results_tiger$path[5][[1]]))
diag(admat) <- 0
plot_adjacency_matrix(admat,title="TIGER")


#========================
# Glasso
#========================

par(mfrow=c(1,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]

lmax <- pulsar::getMaxCov(cov(data_R))
lams <- pulsar::getLamPath(lmax, lmax*.05, len=40)
hugeargs <- list(lambda=lams, verbose=FALSE, method="glasso")
out.p    <- pulsar::pulsar(data_R, fun=huge, fargs=hugeargs, rep.num=20,
                           criterion='stars', lb.stars=TRUE, ub.stars=TRUE)
fit.p    <- pulsar::refit(out.p)
admat <-  adjacentMat(as.matrix(fit.p$refit$stars))
diag(admat) <- 0
plot_adjacency_matrix(admat,title="Glasso, Stars")



data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]

lmax <- pulsar::getMaxCov(cov(data_R))
lams <- pulsar::getLamPath(lmax, lmax*.05, len=40)
hugeargs <- list(lambda=lams, verbose=FALSE, method="glasso")
out.p    <- pulsar::pulsar(data_R, fun=huge, fargs=hugeargs, rep.num=20,
                           criterion='stars', lb.stars=TRUE, ub.stars=TRUE)
fit.p    <- pulsar::refit(out.p)
admat <-  adjacentMat(as.matrix(fit.p$refit$stars))
diag(admat) <- 0
plot_adjacency_matrix(admat,title="Glasso, Stars")

data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]

lmax <- pulsar::getMaxCov(cov(data_R))
lams <- pulsar::getLamPath(lmax, lmax*.05, len=40)
hugeargs <- list(lambda=lams, verbose=FALSE, method="glasso")
out.p    <- pulsar::pulsar(data_R, fun=huge, fargs=hugeargs, rep.num=20,
                           criterion='stars', lb.stars=TRUE, ub.stars=TRUE)
fit.p    <- pulsar::refit(out.p)
admat <-  adjacentMat(as.matrix(fit.p$refit$stars))
diag(admat) <- 0
plot_adjacency_matrix(admat,title="Glasso, Stars")

data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]

lmax <- pulsar::getMaxCov(cov(data_R))
lams <- pulsar::getLamPath(lmax, lmax*.05, len=40)
hugeargs <- list(lambda=lams, verbose=FALSE, method="glasso")
out.p    <- pulsar::pulsar(data_R, fun=huge, fargs=hugeargs, rep.num=20,
                           criterion='stars', lb.stars=TRUE, ub.stars=TRUE)
fit.p    <- pulsar::refit(out.p)
admat <-  adjacentMat(as.matrix(fit.p$refit$stars))
diag(admat) <- 0
plot_adjacency_matrix(admat,title="Glasso, Stars")


#========================
# THAV
#========================

par(mfrow=c(1,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]

thav.glasso::load_libraries()
thav <- thav.glasso::thAV.estimator(data_R)
thav[abs(thav)>0] <- 1 
diag(thav) <- 0

plot_adjacency_matrix(thav,title="thav")



data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]

thav.glasso::load_libraries()
thav <- thav.glasso::thAV.estimator(data_R)
thav[abs(thav)>0] <- 1 
diag(thav) <- 0

plot_adjacency_matrix(thav,title="thav")


data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]

thav.glasso::load_libraries()
thav <- thav.glasso::thAV.estimator(data_R)
thav[abs(thav)>0] <- 1 
diag(thav) <- 0

plot_adjacency_matrix(thav,title="thav")


data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]

thav.glasso::load_libraries()
thav <- thav.glasso::thAV.estimator(data_R)
thav[abs(thav)>0] <- 1 
diag(thav) <- 0

plot_adjacency_matrix(thav,title="thav")


#========================
# CLEVEL
#========================

par(mfrow=c(1,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]

CLEVEL.out = clevel(df = data_R)
inference_out = inference(CLEVEL.out)
admat <- adjacentMat(inference_out)
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="CLEVEL")


data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]

CLEVEL.out = clevel(df = data_R)
inference_out = inference(CLEVEL.out)
admat <- adjacentMat(inference_out)
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="CLEVEL")


data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]

CLEVEL.out = clevel(df = data_R)
inference_out = inference(CLEVEL.out)
admat <- adjacentMat(inference_out)
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="CLEVEL")


data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]

CLEVEL.out = clevel(df = data_R)
inference_out = inference(CLEVEL.out)
admat <- adjacentMat(inference_out)
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="CLEVEL")



#========================
# BEAM
#========================

par(mfrow=c(1,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]

fit <- beam(X =  data_R, type="conditional")
sel <- beam.select(fit, method = "BH")
summary(fit)
myigraph <- ugraph(sel)
admat10 <- as_adjacency_matrix(myigraph, sparse = F)
plot_adjacency_matrix(adjacentMat(admat10),title="beam")


data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]

fit <- beam(X =  data_R, type="conditional")
sel <- beam.select(fit, method = "BH")
summary(fit)
myigraph <- ugraph(sel)
admat10 <- as_adjacency_matrix(myigraph, sparse = F)
plot_adjacency_matrix(adjacentMat(admat10),title="beam")

data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]

fit <- beam(X =  data_R, type="conditional")
sel <- beam.select(fit, method = "BH")
summary(fit)
myigraph <- ugraph(sel)
admat10 <- as_adjacency_matrix(myigraph, sparse = F)
plot_adjacency_matrix(adjacentMat(admat10),title="beam")

data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]

fit <- beam(X =  data_R, type="conditional")
sel <- beam.select(fit, method = "BH")
summary(fit)
myigraph <- ugraph(sel)
admat10 <- as_adjacency_matrix(myigraph, sparse = F)
plot_adjacency_matrix(adjacentMat(admat10),title="beam")



#========================
# HMFGraph, alpha = CC
#========================
library(HMFGraph)
par(mfrow=c(1,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]

est <-  nlshrink::linshrink_cov(data_R)
kappa_max <- max(eigen(est)$values)/min(eigen(est)$values)
alpha <- alpha_selector_eigen(data_R,0.2, kappa_max,beta=0.9)
tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =alpha, epsilon1=0, epsilon2=0,
                      B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")


data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]

est <-  nlshrink::linshrink_cov(data_R)
kappa_max <- max(eigen(est)$values)/min(eigen(est)$values)
alpha <- alpha_selector_eigen(data_R,0.2, kappa_max,beta=0.9)
tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =alpha, epsilon1=0, epsilon2=0,
                      B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")

data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]

est <-  nlshrink::linshrink_cov(data_R)
kappa_max <- max(eigen(est)$values)/min(eigen(est)$values)
alpha <- alpha_selector_eigen(data_R,0.2, kappa_max,beta=0.9)
tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =alpha, epsilon1=0, epsilon2=0,
                      B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")

data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]

est <-  nlshrink::linshrink_cov(data_R)
kappa_max <- max(eigen(est)$values)/min(eigen(est)$values)
alpha <- alpha_selector_eigen(data_R,0.2, kappa_max,beta=0.9)
tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =alpha, epsilon1=0, epsilon2=0,
                      B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")




#========================
# HMFGraph, alpha = 10*p/(10*p+n)
#========================
library(HMFGraph)
par(mfrow=c(1,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]

tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =10*p/(10*p+n), epsilon1=0, epsilon2=0,
                                B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")


data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]


tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =10*p/(10*p+n), epsilon1=0, epsilon2=0,
                                B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")

data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]


tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =10*p/(10*p+n), epsilon1=0, epsilon2=0,
                                B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")

data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]


tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =10*p/(10*p+n), epsilon1=0, epsilon2=0,
                                B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")

#========================
# HMFGraph, alpha = CC
#========================
library(HMFGraph)
par(mfrow=c(1,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]

est <-  nlshrink::linshrink_cov(data_R)
kappa_max <- max(eigen(est)$values)/min(eigen(est)$values)
alpha <- alpha_selector_eigen(data_R,0.2, kappa_max,beta=0.9)
tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =alpha, epsilon1=0, epsilon2=0,
                                B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")


data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]

est <-  nlshrink::linshrink_cov(data_R)
kappa_max <- max(eigen(est)$values)/min(eigen(est)$values)
alpha <- alpha_selector_eigen(data_R,0.2, kappa_max,beta=0.9)
tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =alpha, epsilon1=0, epsilon2=0,
                                B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")

data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]

est <-  nlshrink::linshrink_cov(data_R)
kappa_max <- max(eigen(est)$values)/min(eigen(est)$values)
alpha <- alpha_selector_eigen(data_R,0.2, kappa_max,beta=0.9)
tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =alpha, epsilon1=0, epsilon2=0,
                                B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")

data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]

est <-  nlshrink::linshrink_cov(data_R)
kappa_max <- max(eigen(est)$values)/min(eigen(est)$values)
alpha <- alpha_selector_eigen(data_R,0.2, kappa_max,beta=0.9)
tulos <- HMFGraph::HMFGraph_GEM(data_R, max_iters=10000,stop_criterion=10^-6,n= n,p=p,fixed_B=F,beta=0.9, alpha =alpha, epsilon1=0, epsilon2=0,
                                B=diag(p), inter = 100, print_t=F)
permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=F, number_of_permutations = 50)
tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0)
admat <- tulos_var$adjacency_matrix
diag(admat) <- 0
plot_adjacency_matrix(adjacentMat(admat),title="HMFGraph")

