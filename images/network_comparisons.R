
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

names <- c("accuracy", "bal-accuracy", "MCC", "F1", "TPR", "TNR", "PPV","NPV", "FNR",
           "FPR","FOR", "LRp", "LRn", "FDR", "ACC", "NMI")

Value <- 4 # F1 
round_value <- 2

source("functions/functions_for_result_handeling.R")

load(file = "simulated_data/huge/scale_free_p_100_n_300_huge_data.RData")
load(file = "simulated_data/huge/scale_free_p_100_n_300_huge_adjacency.RData")

load(file = "simulated_data/huge/cluster_p_100_n_300_huge_data.RData")
load(file = "simulated_data/huge/cluster_p_100_n_300_huge_adjacency.RData")

load(file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/scale_free_p_100_n_300_bdgraph_adjacency.RData")

load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_data.RData")
load(file = "simulated_data/bdgraph/cluster_p_100_n_300_bdgraph_adjacency.RData")

setEPS()
postscript("all_networks.eps", width = 12, height = 3*8)   


par(mfrow=c(8,4))
data_R <-scale_free_p_100_n_300_huge_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
qgraph::qgraph(adjacency_correct,title="huge, Scale-free")
data_R <-cluster_p_100_n_300_huge_data[,,1]; adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
qgraph::qgraph(adjacency_correct,title="huge, Cluster")
data_R <-scale_free_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
qgraph::qgraph(adjacency_correct,title="BDgraph, Scale-free")
data_R <-cluster_p_100_n_300_bdgraph_data[,,1]; adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
qgraph::qgraph(adjacency_correct,title="BDgraph, Cluster")

n <- dim(scale_free_p_100_n_300_huge_data)[1]
p <- dim(scale_free_p_100_n_300_huge_data)[2]

sum(cluster_p_100_n_300_huge_adjacency[,,1])/2

sum(cluster_p_100_n_300_bdgraph_adjacency[,,1])/2


#========================
# HMFGraph, alpha = CC
#========================
# setEPS()
# postscript("HMFGraph_CC_networks.eps", width = 12, height = 3)   
# 
# 
# par(mfrow=c(1,4))
adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_HMF_Z_CC_scale_free_p100_n300_huge_data.RData")
admat <- results_HMF_Z_CC_scale_free_p100_n300_huge_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("HMFGraph, CC, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_HMF_Z_CC_cluster_p100_n300_huge_data.RData")
admat <- results_HMF_Z_CC_cluster_p100_n300_huge_data[,,1] 
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("HMFGraph, CC, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n300_bdgraph_data.RData")
admat <-results_HMF_Z_CC_scale_free_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("HMFGraph, CC, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n300_bdgraph_data.RData")
admat <- results_HMF_Z_CC_cluster_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("HMFGraph, CC, ",names[Value]," = ",round(V,round_value)))


# dev.off()
#========================
# GWISHART
#========================
# setEPS()
# postscript("G_wishart_networks.eps", width = 12, height = 3)  
# 
# 
# par(mfrow=c(1,4))
adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_G_wishart_scale_free_p100_n300_huge_data.RData")
admat <- results_G_wishart_scale_free_p100_n300_huge_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("G-WISHART, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_G_wishart_cluster_p100_n300_huge_data.RData")
admat <- results_G_wishart_cluster_p100_n300_huge_data[,,1] 
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("G-WISHART, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_G_wishart_scale_free_p100_n300_bdgraph_data.RData")
admat <-results_G_wishart_scale_free_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("G-WISHART, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_G_wishart_cluster_p100_n300_bdgraph_data.RData")
admat <- results_G_wishart_cluster_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("G-WISHART, ",names[Value]," = ",round(V,round_value)))

# dev.off()
#========================
# TIGER
#========================
# setEPS()
# postscript("TIGER_networks.eps", width = 12, height = 3)   
# 
# 
# par(mfrow=c(1,4))
adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_TIGER_scale_free_p100_n300_huge_data.RData")
admat <- results_TIGER_scale_free_p100_n300_huge_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("TIGER, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_TIGER_cluster_p100_n300_huge_data.RData")
admat <- results_TIGER_cluster_p100_n300_huge_data[,,1] 
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("TIGER, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_TIGER_scale_free_p100_n300_bdgraph_data.RData")
admat <-results_TIGER_scale_free_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("TIGER, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_TIGER_cluster_p100_n300_bdgraph_data.RData")
admat <- results_TIGER_cluster_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("TIGER, ",names[Value]," = ",round(V,round_value)))

# dev.off()
#========================
# Glasso
#========================
# setEPS()
# postscript("Glasso_networks.eps", width = 12, height = 3)  
# 
# 
# par(mfrow=c(1,4))
adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_glasso_scale_free_p100_n300_huge_data.RData")
admat <- results_glasso_scale_free_p100_n300_huge_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("glasso, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_glasso_cluster_p100_n300_huge_data.RData")
admat <- results_glasso_cluster_p100_n300_huge_data[,,1] 
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("glasso, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_glasso_scale_free_p100_n300_bdgraph_data.RData")
admat <-results_glasso_scale_free_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("glasso, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_glasso_cluster_p100_n300_bdgraph_data.RData")
admat <- results_glasso_cluster_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("glasso, ",names[Value]," = ",round(V,round_value)))

# dev.off()
#========================
# THAV
#========================
# setEPS()
# postscript("THAV_networks.eps", width = 12, height = 3)   
# 
# 
# par(mfrow=c(1,4))
adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_THAV_scale_free_p100_n300_huge_data.RData")
admat <- results_THAV_scale_free_p100_n300_huge_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("THAV, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_THAV_cluster_p100_n300_huge_data.RData")
admat <- results_THAV_cluster_p100_n300_huge_data[,,1] 
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("THAV, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_THAV_scale_free_p100_n300_bdgraph_data.RData")
admat <-results_THAV_scale_free_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("THAV, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_THAV_cluster_p100_n300_bdgraph_data.RData")
admat <- results_THAV_cluster_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("THAV, ",names[Value]," = ",round(V,round_value)))

# # dev.off()
# #========================
# # CLEVEL
# #========================
# # setEPS()
# # postscript("Clevel_networks.eps", width = 12, height = 3)   
# # 
# # 
# # par(mfrow=c(1,4))
# adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
# load(file="results/huge/results_clevel_scale_free_p100_n300_huge_data.RData")
# admat <- results_clevel_scale_free_p100_n300_huge_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("clevel, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
# load(file="results/huge/results_clevel_cluster_p100_n300_huge_data.RData")
# admat <- results_clevel_cluster_p100_n300_huge_data[,,1] 
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("clevel, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
# load(file="results/bdgraph/results_clevel_scale_free_p100_n300_bdgraph_data.RData")
# admat <-results_clevel_scale_free_p100_n300_bdgraph_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("clevel, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
# load(file="results/bdgraph/results_clevel_cluster_p100_n300_bdgraph_data.RData")
# admat <- results_clevel_cluster_p100_n300_bdgraph_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("clevel, ",names[Value]," = ",round(V,round_value)))

# dev.off()
#========================
# CLEVEL, default alpha
#========================
# setEPS()
# postscript("clevel_D_networks.eps", width = 12, height = 3)   
# 
# 
# par(mfrow=c(1,4))
adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_clevel_d_scale_free_p100_n300_huge_data.RData")
admat <- results_clevel_d_scale_free_p100_n300_huge_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("clevel, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_clevel_d_cluster_p100_n300_huge_data.RData")
admat <- results_clevel_d_cluster_p100_n300_huge_data[,,1] 
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("clevel, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_clevel_d_scale_free_p100_n300_bdgraph_data.RData")
admat <-results_clevel_d_scale_free_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("clevel, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_clevel_d_cluster_p100_n300_bdgraph_data.RData")
admat <- results_clevel_d_cluster_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("clevel, ",names[Value]," = ",round(V,round_value)))

# dev.off()
#========================
# BEAM
#========================
# setEPS()
# postscript("beam_networks.eps", width = 12, height = 3)   
# 
# 
# par(mfrow=c(1,4))
adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_beam_scale_free_p100_n300_huge_data.RData")
admat <- results_beam_scale_free_p100_n300_huge_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("beam, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
load(file="results/huge/results_beam_cluster_p100_n300_huge_data.RData")
admat <- results_beam_cluster_p100_n300_huge_data[,,1] 
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("beam, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_beam_scale_free_p100_n300_bdgraph_data.RData")
admat <-results_beam_scale_free_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("beam, ",names[Value]," = ",round(V,round_value)))

adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
load(file="results/bdgraph/results_beam_cluster_p100_n300_bdgraph_data.RData")
admat <- results_beam_cluster_p100_n300_bdgraph_data[,,1]
V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
qgraph::qgraph(admat,title=paste0("beam, ",names[Value]," = ",round(V,round_value)))


# dev.off()
# #========================
# # HMFGraph, alpha = CC
# #========================
# # setEPS()
# # postscript("HMFGraph_CC_networks.eps", width = 12, height = 3)   
# # 
# # 
# # par(mfrow=c(1,4))
# adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
# load(file="results/huge/results_HMF_Z_CC_scale_free_p100_n300_huge_data.RData")
# admat <- results_HMF_Z_CC_scale_free_p100_n300_huge_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, CC, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
# load(file="results/huge/results_HMF_Z_CC_cluster_p100_n300_huge_data.RData")
# admat <- results_HMF_Z_CC_cluster_p100_n300_huge_data[,,1] 
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, CC, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
# load(file="results/bdgraph/results_HMF_Z_CC_scale_free_p100_n300_bdgraph_data.RData")
# admat <-results_HMF_Z_CC_scale_free_p100_n300_bdgraph_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, CC, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
# load(file="results/bdgraph/results_HMF_Z_CC_cluster_p100_n300_bdgraph_data.RData")
# admat <- results_HMF_Z_CC_cluster_p100_n300_bdgraph_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, CC, ",names[Value]," = ",round(V,round_value)))

# 
# # dev.off()
# #========================
# # HMFGraph, alpha = 10*p/(10*p+n)
# #========================
# # setEPS()
# # postscript("HMFGraph_10x_networks.eps", width = 12, height = 3)  
# # 
# # 
# # par(mfrow=c(1,4))
# adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
# load(file="results/huge/results_HMF_Z_scale_free_p100_n300_huge_data.RData")
# admat <- results_HMF_Z_scale_free_p100_n300_huge_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, 10x, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
# load(file="results/huge/results_HMF_Z_cluster_p100_n300_huge_data.RData")
# admat <- results_HMF_Z_cluster_p100_n300_huge_data[,,1] 
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, 10x, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
# load(file="results/bdgraph/results_HMF_Z_scale_free_p100_n300_bdgraph_data.RData")
# admat <-results_HMF_Z_scale_free_p100_n300_bdgraph_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, 10x, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
# load(file="results/bdgraph/results_HMF_Z_cluster_p100_n300_bdgraph_data.RData")
# admat <- results_HMF_Z_cluster_p100_n300_bdgraph_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, 10x, ",names[Value]," = ",round(V,round_value)))
# 
# # dev.off()
# #========================
# # HMFGraph, alpha = 2*p/(2*p+n)
# #========================
# # setEPS()
# # postscript("HMFGraph_2x_networks.eps", width = 12, height = 3)  
# # 
# # 
# # par(mfrow=c(1,4))
# adjacency_correct <- scale_free_p_100_n_300_huge_adjacency[,,1]
# load(file="results/huge/results_HMF_Z2x_scale_free_p100_n300_huge_data.RData")
# admat <- results_HMF_Z2x_scale_free_p100_n300_huge_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, 2x, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- cluster_p_100_n_300_huge_adjacency[,,1]
# load(file="results/huge/results_HMF_Z2x_cluster_p100_n300_huge_data.RData")
# admat <- results_HMF_Z2x_cluster_p100_n300_huge_data[,,1] 
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, 2x, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- scale_free_p_100_n_300_bdgraph_adjacency[,,1]
# load(file="results/bdgraph/results_HMF_Z2x_scale_free_p100_n300_bdgraph_data.RData")
# admat <-results_HMF_Z2x_scale_free_p100_n300_bdgraph_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, 2x, ",names[Value]," = ",round(V,round_value)))
# 
# adjacency_correct <- cluster_p_100_n_300_bdgraph_adjacency[,,1]
# load(file="results/bdgraph/results_HMF_Z2x_cluster_p100_n300_bdgraph_data.RData")
# admat <- results_HMF_Z2x_cluster_p100_n300_bdgraph_data[,,1]
# V <- calculate_scores(tarkkuus(adjacency_correct, admat))[Value]
# qgraph::qgraph(admat,title=paste0("HMFGraph, 2x, ",names[Value]," = ",round(V,round_value)))


dev.off()