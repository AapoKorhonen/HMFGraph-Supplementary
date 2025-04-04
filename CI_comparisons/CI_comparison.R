############################################################
# Comparisons of credible intervals received from GEM algortihm and Gibbs sampler.
# 90% CIs are used
############################################################

library(huge)
library(HMFGraph)

colors <- c("blue","red") 
set.seed(42)
n <-100
p <-20
graph_data <- huge::huge.generator(n = n, d =p, vis = TRUE, graph ="scale-free")
data_R <-graph_data$data
n <- dim(data_R)[1]
p <- dim(data_R)[2]

alpha <- HMFGraph::alpha_binary_search(data_R)

results_gibbs <- HMFGraph::HMFGraph_gibbs_sampler(data_R, alpha=alpha)

results_gibbs_map <-HMFGraph::HMFGraph_gibbs_CI(results_gibbs, CI=0.90)

results_gibbs_map$lower_CI

results_gibbs_map$upper_CI

results_gibbs_map$median

results_GEM <- HMFGraph::HMFGraph_GEM(data_R, alpha=alpha, stop_criterion = 0)

results_GEM_map <-HMFGraph::HMFGraph_GEM_CI(results_GEM, CI=0.90)

results_GEM_map$lower_CI

results_GEM_map$upper_CI

results_GEM_map$MAP_estimate

set.seed(123)
ala_mat1 <- results_gibbs_map$lower_CI[ lower.tri(results_gibbs_map$lower_CI) ]  
yla_mat1 <- results_gibbs_map$upper_CI[ lower.tri(results_gibbs_map$lower_CI) ]  
ala_mat2 <- results_GEM_map$lower_CI[ lower.tri(results_gibbs_map$lower_CI) ]  
yla_mat2 <- results_GEM_map$upper_CI[ lower.tri(results_gibbs_map$lower_CI) ] 
map1 <- results_gibbs_map$median[ lower.tri(results_gibbs_map$lower_CI) ]  
map2 <- results_GEM_map$MAP_estimate[ lower.tri(results_gibbs_map$lower_CI) ]  

map1  <- map1[!(map1==0)]
ala_mat1 <- ala_mat1[!(ala_mat1==0)]
yla_mat1 <- yla_mat1[!(yla_mat1==0)]
map2  <- map2[!(map2==0)]
ala_mat2 <- ala_mat2[!(ala_mat2==0)]
yla_mat2 <- yla_mat2[!(yla_mat2==0)]

order_map <- order(map1)  
ala_mat1 <- ala_mat1[order_map]
yla_mat1 <- yla_mat1[order_map]
ala_mat2 <- ala_mat2[order_map]
yla_mat2 <- yla_mat2[order_map]
map1 <- map1[order_map]
map2 <- map2[order_map]


x <- seq_along(ala_mat1)
par(mfrow=c(1,1),mgp=c(2,1,0))
setEPS()
cairo_ps("CI_GEM_GIBBS_CI.eps", width = 20, height = 10, fallback_resolution = 1000)   


plot(x, ala_mat1, type = "n", ylim = range(c(ala_mat1, yla_mat1, ala_mat2, yla_mat2)), 
     xlab = "Edges", ylab = "Credible Interval",
     main = "90 % posterior credible intervals")


for (i in seq_along(x)) {
  segments(x[i], ala_mat1[i], x[i], yla_mat1[i], col = colors[1], lwd = 1.5)
}
polygon(c(x, rev(x)), c(ala_mat1, rev(yla_mat1)),  col = scales::alpha(col=colors[1], 0.3) , border = NA)

for (i in seq_along(x)) {
  segments(x[i], ala_mat2[i], x[i], yla_mat2[i], col =colors[2], lwd = 1.5)
}
polygon(c(x, rev(x)), c(ala_mat2, rev(yla_mat2)),col = scales::alpha(col=colors[2], 0.3) , border = NA)


points(x, map1, col = colors[1], pch = 16, cex=0.7)  
points(x, map2, col = colors[2], pch = 16, cex=0.7)   

legend("bottomright", legend = c("Gibbs sampler", "GEM algorithm"),
       col = c(colors[1],colors[2]),
       pch = c(16, 15),
       lty = c(1, 1),
       lwd = c(2, 2),
       title = "Estimation method")

abline(h=0)

dev.off()

