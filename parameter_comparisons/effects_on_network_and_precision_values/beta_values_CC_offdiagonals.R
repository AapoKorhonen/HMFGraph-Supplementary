############################################################
# Comparisons of values of off diagonal elements of estimated precision matrix with multiple different beta values
# For all, alpha selected using CC-method
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

colors <- brewer.pal(5, "Dark2")  

#====================================
# True network
#====================================

setEPS()
postscript("FigS6.eps", width = 10, height = 11)   # koko oletuksena tuumina

par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,1,1,1,2,2),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.5,0.5,0.05))


#====================================
# beta = 0.95
#====================================

results_GEM <- HMFGraph::HMFGraph_GEM(data_R, beta=0.95)

plot(density(results_GEM$omega[lower.tri(graph_data$omega)])  , col=colors[1], lty =1, xlim=c(-0.3,0.6)
     , cex.lab = 1.2,cex.main=2,cex.axis=1.5, lwd = 2
     , ylab = "density", xlab="values of off-diagonal elements of estimated precision matrix", main= "Distribution of off-diagonal elements in the precision matrix with multiple beta values")


#====================================
# beta=0.90
#====================================
results_GEM <- HMFGraph::HMFGraph_GEM(data_R, beta=0.90)

lines(density(results_GEM$omega[lower.tri(graph_data$omega)])  , col=colors[2], lty =2, lwd = 2,cex.lab = 1.2,)


#====================================
# beta=0.80
#====================================
results_GEM <- HMFGraph::HMFGraph_GEM(data_R, beta=0.80)

lines(density(results_GEM$omega[lower.tri(graph_data$omega)])  , col=colors[3], lty =3, lwd = 2,cex.lab = 1.2,)


#====================================
# beta=0.70
#====================================
results_GEM <- HMFGraph::HMFGraph_GEM(data_R, beta=0.70)

lines(density(results_GEM$omega[lower.tri(graph_data$omega)]) , col=colors[4], lty =4, lwd = 2,cex.lab = 1.2,)

#====================================
# beta=0.60
#====================================
results_GEM <- HMFGraph::HMFGraph_GEM(data_R, beta=0.60)

lines(density(results_GEM$omega[lower.tri(graph_data$omega)]) , col=colors[5], lty =5, lwd = 2,cex.lab = 1.2,)

par(mar = c(1, 1, 1, 1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c(colors[1],colors[2], colors[3], colors[4], colors[5])

legend(x = "top",inset = 0,
       legend = c("beta = 0.95", "beta = 0.90", "beta = 0.80","beta = 0.70", "beta = 0.60"), 
       col=plot_colors, lwd=2, cex=1.2, xpd = TRUE, horiz = TRUE, lty =c(1:5))

dev.off()
