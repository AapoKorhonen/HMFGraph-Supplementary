############################################################
# Comparisons of networks received from GEM algortihm and Gibbs sampler.
# Networks are selected based on credible intervals. 
# 99 %, 95%, 90% and 80% CIs are used
############################################################



library(huge)
library(HMFGraph)
library(qgraph)

set.seed(42)
n <-100 # number of samples
p <-20 # number of variables
graph_data <- huge.generator(n = n, d =p, vis = TRUE, graph ="scale-free")
data_R <-graph_data$data
real_admat <-round(graph_data$omega,5)
diag(real_admat) <- 0
real_admat[abs(real_admat) > 0] <- 1

alpha <- HMFGraph::alpha_binary_search(data_R) # An optimal alpha value

results_GEM <- HMFGraph::HMFGraph_GEM(data_R, alpha=alpha)

start_time <- Sys.time()

results_gibbs <- HMFGraph::HMFGraph_gibbs_sampler(data_R, alpha=alpha)

end_time <- Sys.time(); end_time - start_time



#====================================
# True network
#====================================

setEPS()
postscript("CI_GEM_GIBBS_network.eps", width = 5, height = 15)   # koko oletuksena tuumina

#par(mfrow=c(1,1))
layout(matrix(c(1,1,1,1,2,3,4,5,6,7,8,9),6,2,byrow=TRUE))


qgraph::qgraph(real_admat, layout="circle", title="Scale-free network generated with huge R-package")




#par(mfrow=c(4,2))



#====================================
# 99 % CI
#====================================
gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.99)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="GEM-algorithm, 99 % CI")

gibbs_network <-HMFGraph::HMFGraph_gibbs_CI(results_gibbs, CI=0.99)
qgraph::qgraph(gibbs_network$adjacency_matrix, layout="circle", title="Gibbs-sampler, 99 % CI")


#====================================
# 95 % CI
#====================================
gem_network <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.95)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="GEM-algorithm, 95 % CI")

gibbs_network <-HMFGraph::HMFGraph_gibbs_CI(results_gibbs, CI=0.95)
qgraph::qgraph(gibbs_network$adjacency_matrix, layout="circle", title="Gibbs-sampler, 95 % CI")


#====================================
# 90 % CI
#====================================
gem_network <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="GEM-algorithm, 90 % CI")

gibbs_network <-HMFGraph::HMFGraph_gibbs_CI(results_gibbs, CI=0.90)
qgraph::qgraph(gibbs_network$adjacency_matrix, layout="circle", title="Gibbs-sampler, 90 % CI")


#====================================
# 80 % CI
#====================================
gem_network <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.80)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="GEM-algorithm, 80 % CI")

gibbs_network <-HMFGraph::HMFGraph_gibbs_CI(results_gibbs, CI=0.80)
qgraph::qgraph(gibbs_network$adjacency_matrix, layout="circle", title="Gibbs-sampler, 80 % CI")

dev.off()
