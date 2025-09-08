
################################################################################################################################################################
#==============================================================================================================================================================
# This file is for plotting out the FDR control results for multiple alpha values. alpha selected using the CC-method
#==============================================================================================================================================================
################################################################################################################################################################


library(RColorBrewer)

colors <- brewer.pal(5, "Dark2")  

################################################################################################################################################################


load(file="FDR_control/beta/results/results_FDR_CC_b_095_hsc.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_090_hsc.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_080_hsc.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_070_hsc.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_060_hsc.RData")

load(file="FDR_control/beta/results/results_FDR_CC_b_095_hC.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_090_hC.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_080_hC.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_070_hC.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_060_hC.RData")

load(file="FDR_control/beta/results/results_FDR_CC_b_095_bsc.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_090_bsc.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_080_bsc.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_070_bsc.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_060_bsc.RData")

load(file="FDR_control/beta/results/results_FDR_CC_b_095_bC.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_090_bC.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_080_bC.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_070_bC.RData")
load(file="FDR_control/beta/results/results_FDR_CC_b_060_bC.RData")

setEPS()
postscript("FigS9.eps", width = 12, height = 14)   

par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.5,0.5,0.05))

target_FDRs <- seq(0,0.99,by=0.01)

par(pty="s")
plot(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_095_hsc)) , 
     type = "l", main ="huge, scale-free", 
     xlim=c(0,1),ylim=c(0,1), lwd = 2, col = colors[1],
     xlab="Target FDR", ylab = "Real FDR",cex.lab = 1.2,cex.main=2,cex.axis=1.5)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_090_hsc)), col=colors[2], lwd = 2,cex.lab = 1.2, lty=2)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_080_hsc)), col=colors[3], lwd = 2,cex.lab = 1.2, lty=3)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_070_hsc)), col=colors[4], lwd = 2,cex.lab = 1.2, lty=4)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_060_hsc)), col=colors[5], lwd = 2,cex.lab = 1.2, lty=5)
abline(a = 0, b = 1, lty = 2) 

par(pty="s")
plot(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_095_hC)) ,
     type = "l", main ="huge, cluster", 
     xlim=c(0,1),ylim=c(0,1), lwd = 2, col = colors[1],
     xlab="Target FDR", ylab = "Real FDR",cex.lab = 1.2,cex.main=2,cex.axis=1.5)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_090_hC)), col=colors[2], lwd = 2,cex.lab = 1.2, lty=2)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_080_hC)), col=colors[3], lwd = 2,cex.lab = 1.2, lty=3)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_070_hC)), col=colors[4], lwd = 2,cex.lab = 1.2, lty=4)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_060_hC)), col=colors[5], lwd = 2,cex.lab = 1.2, lty=5)
abline(a = 0, b = 1, lty = 2) 

par(pty="s")
plot(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_095_bsc)) ,
     type = "l", main ="Bdgraph, scale-free", 
     xlim=c(0,1),ylim=c(0,1), lwd = 2, col = colors[1],
     xlab="Target FDR", ylab = "Real FDR",cex.lab = 1.2,cex.main=2,cex.axis=1.5)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_090_bsc)), col=colors[2], lwd = 2,cex.lab = 1.2, lty=2,)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_080_bsc)), col=colors[3], lwd = 2,cex.lab = 1.2, lty=3)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_070_bsc)), col=colors[4], lwd = 2,cex.lab = 1.2, lty=4)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_060_bsc)), col=colors[5], lwd = 2,cex.lab = 1.2, lty=5)
abline(a = 0, b = 1, lty = 2) 

par(pty="s")
plot(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_095_bC)) ,
     type = "l", main ="Bdgraph, cluster", 
     xlim=c(0,1),ylim=c(0,1), lwd = 2, col = colors[1],
     xlab="Target FDR", ylab = "Real FDR", cex.lab = 1.2,cex.main=2,cex.axis=1.5)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_090_bC)), col=colors[2], lwd = 2,cex.lab = 1.2, lty=2)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_080_bC)), col=colors[3], lwd = 2,cex.lab = 1.2, lty=3)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_070_bC)), col=colors[4], lwd = 2,cex.lab = 1.2, lty=4)
lines(x=target_FDRs,y=colMeans(t(results_FDR_CC_b_060_bC)), col=colors[5], lwd = 2,cex.lab = 1.2, lty=5)
abline(a = 0, b = 1, lty = 2) 


par(mar = c(1, 1, 1, 1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c(colors[1],colors[2], colors[3], colors[4], colors[5])
legend(x = "top",inset = 0,
       legend = c("beta = 0.95", "beta = 0.90", "beta = 0.80","beta = 0.70", "beta = 0.60"), 
       col=plot_colors, lwd=2, cex=1.2, xpd = TRUE, horiz = TRUE, lty =c(1:5))


dev.off()
