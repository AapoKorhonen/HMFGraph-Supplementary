
################################################################################################################################################################
#==============================================================================================================================================================
# This file is for plotting out the FDR control results for multiple alpha values
#==============================================================================================================================================================
################################################################################################################################################################


library(RColorBrewer)

colors <- brewer.pal(3, "Dark2")  

################################################################################################################################################################


load(file="selected_alpha_values/results/alphas_hsc_35.RData")
load(file="selected_alpha_values/results/alphas_hsc_75.RData")
load(file="selected_alpha_values/results/alphas_hsc_150.RData")
load(file="selected_alpha_values/results/alphas_hsc_300.RData")

alphas_hsc <- c(mean(alphas_hsc_35),
                mean(alphas_hsc_75),
                mean(alphas_hsc_150),
                mean(alphas_hsc_300))

load(file="selected_alpha_values/results/alphas_hC_35.RData")
load(file="selected_alpha_values/results/alphas_hC_75.RData")
load(file="selected_alpha_values/results/alphas_hC_150.RData")
load(file="selected_alpha_values/results/alphas_hC_300.RData")


alphas_hC <- c(mean(alphas_hC_35),
                mean(alphas_hC_75),
                mean(alphas_hC_150),
                mean(alphas_hC_300))


load(file="selected_alpha_values/results/alphas_bsc_35.RData")
load(file="selected_alpha_values/results/alphas_bsc_75.RData")
load(file="selected_alpha_values/results/alphas_bsc_150.RData")
load(file="selected_alpha_values/results/alphas_bsc_300.RData")


alphas_bsc <- c(mean(alphas_bsc_35),
                mean(alphas_bsc_75),
                mean(alphas_bsc_150),
                mean(alphas_bsc_300))


load(file="selected_alpha_values/results/alphas_bC_35.RData")
load(file="selected_alpha_values/results/alphas_bC_75.RData")
load(file="selected_alpha_values/results/alphas_bC_150.RData")
load(file="selected_alpha_values/results/alphas_bC_300.RData")


alphas_bC <- c(mean(alphas_bC_35),
              mean(alphas_bC_75),
              mean(alphas_bC_150),
              mean(alphas_bC_300))


p <- 100
n <- c(35,75,150,300)

alpha_p10 <- 10*p/(10*p+n)
alpha_p2 <- 2*p/(2*p+n)

setEPS()
postscript("selected_alpha_values.eps", width = 12, height = 14)   

par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,2,3,4,5,5),nrow = 3,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.5,0.5,0.05))

target_FDRs <- seq(0,0.99,by=0.01)

plot(x=n,y=colMeans(t(alphas_hsc)),
     type = "l", main ="huge, scale-free",ylim=c(0,1), lwd = 2, col = colors[1],
     xlab="Sample size (n)", ylab="alpha values",cex.lab = 1.2,cex.main=2,cex.axis=1.5)
lines(x=n,y=alpha_p10, col=colors[2], lwd = 2,cex.lab = 1.2, lty=2)
lines(x=n,y=alpha_p2, col=colors[3], lwd = 2,cex.lab = 1.2, lty=3)
abline(a = 0, b = 1, lty = 2) 

plot(x=n,y=colMeans(t(alphas_hC)) ,
     type = "l", main ="huge, cluster", ylim=c(0,1), lwd = 2, col = colors[1],
     xlab="Sample size (n)", ylab="alpha values",cex.lab = 1.2,cex.main=2,cex.axis=1.5)
lines(x=n,y=alpha_p10, col=colors[2], lwd = 2,cex.lab = 1.2, lty=2)
lines(x=n,y=alpha_p2, col=colors[3], lwd = 2,cex.lab = 1.2, lty=3)
abline(a = 0, b = 1, lty = 2) 

plot(x=n,y=colMeans(t(alphas_bsc)) ,
     type = "l", main ="Bdgraph, scale-free", ylim=c(0,1), lwd = 2, col = colors[1],
     xlab="Sample size (n)", ylab="alpha values",cex.lab = 1.2,cex.main=2,cex.axis=1.5)
lines(x=n,y=alpha_p10, col=colors[2], lwd = 2,cex.lab = 1.2, lty=2,)
lines(x=n,y=alpha_p2, col=colors[3], lwd = 2,cex.lab = 1.2, lty=3)
abline(a = 0, b = 1, lty = 2) 

plot(x=n,y=colMeans(t(alphas_bC)) ,
     type = "l", main ="Bdgraph, cluster", ylim=c(0,1), lwd = 2, col = colors[1],
     xlab="Sample size (n)", ylab="alpha values", cex.lab = 1.2,cex.main=2,cex.axis=1.5)
lines(x=n,y=alpha_p10, col=colors[2], lwd = 2,cex.lab = 1.2, lty=2)
lines(x=n,y=alpha_p2, col=colors[3], lwd = 2,cex.lab = 1.2, lty=3)
abline(a = 0, b = 1, lty = 2) 


par(mar = c(1, 1, 1, 1))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colors <- c(colors[1],colors[2], colors[3])
legend(x = "top",inset = 0,
       legend = c("CC-method", "alpha = 10*p/(10*p+n)", "alpha = 2*p/(2*p+n)"), 
       col=plot_colors, lwd=2, cex=1.2, xpd = TRUE, horiz = TRUE, lty =c(1:3))


dev.off()