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


#====================================
# True network
#====================================

setEPS()
postscript("FigS7.eps", width = 5, height = 15)   # koko oletuksena tuumina

#par(mfrow=c(1,1))
layout(matrix(c(1,2,3,4,5,6,7,8,9,10),5,2,byrow=TRUE))


#qgraph::qgraph(real_admat, layout="circle", title="Scale-free network generated with huge R-package")




#par(mfrow=c(4,2))



#====================================
# beta = 0.95
#====================================

results_GEM <- HMFGraph::HMFGraph_GEM(data_R, alpha=0.80, beta=0.95)

gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="alpha=0.80, beta=0.95")

results_GEM <- HMFGraph::HMFGraph_GEM(data_R, beta=0.95)
gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="CC-method, beta=0.95")


#====================================
# beta=0.90
#====================================
results_GEM <- HMFGraph::HMFGraph_GEM(data_R, alpha=0.80, beta=0.90)

gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="alpha=0.80, beta=0.90")

results_GEM <- HMFGraph::HMFGraph_GEM(data_R, beta=0.90)
gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="CC-method, beta=0.90")


#====================================
# beta=0.80
#====================================
results_GEM <- HMFGraph::HMFGraph_GEM(data_R, alpha=0.80, beta=0.80)

gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="alpha=0.80, beta=0.80")

results_GEM <- HMFGraph::HMFGraph_GEM(data_R, beta=0.80)
gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="CC-method, beta=0.80")


#====================================
# beta=0.70
#====================================
results_GEM <- HMFGraph::HMFGraph_GEM(data_R, alpha=0.80, beta=0.70)

gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="alpha=0.80, beta=0.70")

results_GEM <- HMFGraph::HMFGraph_GEM(data_R, beta=0.70)
gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="CC-method, beta=0.70")

#====================================
# beta=0.60
#====================================

results_GEM <- HMFGraph::HMFGraph_GEM(data_R, alpha=0.80, beta=0.60)

gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="alpha=0.80, beta=0.60")

results_GEM <- HMFGraph::HMFGraph_GEM(data_R, beta=0.60)
gem_network  <- HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)
qgraph::qgraph(gem_network$adjacency_matrix, layout="circle", title="CC-method, beta=0.60")


dev.off()
