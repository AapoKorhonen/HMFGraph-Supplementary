#========================
# Riboflavin data 
#========================
library("hdi")
data("riboflavin")
dim(riboflavin)
dim(riboflavin$x)
library(RColorBrewer)
library(grDevices)
library(viridis)
vars <- var(riboflavin$x)
vars <- diag((vars))
vars_ord <- order(vars,decreasing = T )

data_R <- riboflavin$x[,c(sort(vars_ord[c(1:100)],decreasing = F )) ]
data_R <- cbind(riboflavin$y, data_R) # including the riboflavin production 

data_R <- huge::huge.npn(data_R)

n <- dim(data_R)[1]
p <- dim(data_R)[2]

list_of_c_genes <- c( "YXLE_at","PCKA_at", "YTGD_at", 
                      "AHPC_at", "YHZA_at","YTCF_at",
                      "NDK_at", "GAP_at", "RPSG_at", 
                      "YTGB_at",  "XHLA_at")

list_num <- 2:(length(list_of_c_genes)+1)
indices <- list()
indices[[1]] <- 1

for(j in 1:(length(list_of_c_genes))){
  
  indices[[j+1]] <- (1:p)[colnames(data_R) == list_of_c_genes[j]]
}

indices

indices[[13]] <- (2:p)[(colnames(data_R)[-1] %in% list_of_c_genes) == F]

(1:p)[colnames(data_R) %in% list_of_c_genes]
(1:p)[colnames(data_R) == "YXLD_at"]




indices <- list()
indices[[1]] <- 1


indices[[1]] <- 1
indices[[2]] <- (2:p)[(colnames(data_R)[-1] %in% c("YXLE_at")) == T] # FDR 0.1
indices[[3]] <- (2:p)[(colnames(data_R)[-1] %in% c("PCKA_at")) == T] # FDR 0.2
indices[[4]] <- (2:p)[(colnames(data_R)[-1] %in% c("YTGD_at")) == T] # FDR 0.3
indices[[5]] <- (2:p)[(colnames(data_R)[-1] %in% c("AHPC_at", "YTCF_at")) == T] # FDR 0.4
indices[[6]] <- (2:p)[(colnames(data_R)[-1] %in% c("NDK_at", "YHZA_at")) == T] # FDR 0.5
indices[[7]] <- (2:p)[(colnames(data_R)[-1] %in% c("GAP_at", "RPSG_at", "YTGB_at", "XHLA_at")) == T] # FDR 0.7
indices[[8]] <- (2:p)[(colnames(data_R)[-1] %in% list_of_c_genes) == F]

#========================
# alpha = CC
#========================
set.seed(42)

tulos <- HMFGraph::HMFGraph_GEM(data_R,print_binary_search = T)
tulos$alpha
max(eigen(tulos$omega)$values)/min(eigen(tulos$omega)$values) # Condition number

permutations <- HMFGraph::HMFGraph_GEM_permutations(data_R, tulos, parallel=T)
tulos_var <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.2)

qgraph::qgraph(tulos_var$adjacency_matrix)

setEPS()
postscript("Fig6.eps", width = 14, height = 12)  

colfunc <- colorRampPalette(c(viridis(6)))
COLS <- colfunc(6)

colors <- c()
par(mfrow=c(1,1))
colors[1] <- "red"
colors[2:7] <- COLS
colors[8] <- "white"

par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,2,3,3),nrow = 2,ncol = 2,byrow = TRUE)
layout(mat = m,heights = c(0.5,0.07,0.05),width = c(3,0.4))

par(mar = c(0.01, 0.01, 0.01, 0.01))
label_color <- rep("black", 101)
label_color[c(32,89,97)] <- "white"
qgraph::qgraph(tulos_var$adjacency_matrix,diag=F,usePCH=T,vsize= 2.7,
               color = colors, label.color=label_color,
               groups = indices, legend=F,label.cex=1.5, labels=T
)

par(mar = c(0.01, 0.01, 0.01, 0.01))


list_of_FDR <- c("0.7", "0.5", "0.4", "0.3", "0.2", "0.1")
legend_image <- as.raster(matrix(rev(colfunc(20)), ncol=1))
plot(c(0,2),c(0.1,0.7),type = 'n', axes = F,xlab = '', ylab = '',ylim = rev(c(0.1,0.7)))
title("Target FDR ", line = -1.2,cex.main = 1.7)
text(x=1.3, y = c(0.7,0.5,0.4,0.3,0.2,0.1), labels = c(0.7,0.5,0.4,0.3,0.2,0.1),cex = 1.2)

n <- 50  
y_vals <- seq(0.1, 0.7, length.out = n + 1)
cols <- colorRampPalette(c(viridis(6)))(n)

for (i in 1:n) {
  rect(0.5,y_vals[i], 1,y_vals[i + 1], col = cols[i], border = NA)
}

plot(1, type = "n", axes=FALSE, xlab="", ylab="")

list_of_c_genes <- c("ribo_p (1)", "YXLE_at (97)"
                     ,"PCKA_at (32)", "YTGD_at (89)" 
                     ,"AHPC_at (8)", "YHZA_at (62)"
                     ,"YTCF_at (85)", "NDK_at (31)"
                     ,"GAP_at (18)", "RPSG_at (40)"
                     ,"YTGB_at (87)", "XHLA_at (44)")

colors_gene <- c()
colors_gene[1] <- colors[1]; colors_gene[2] <- colors[2]; colors_gene[3] <- colors[3]; colors_gene[4] <- colors[4]
colors_gene[c(5,6)] <- colors[5]; colors_gene[c(7,8)] <- colors[6]; colors_gene[c(9,10,11,12)] <- colors[7]

legend(x = "top",inset = 0,
       legend = list_of_c_genes,ncol=6,border=NA,
       col=rep("black", length(list_of_c_genes)), pt.bg=colors_gene, cex=1.4, xpd = TRUE,pch=21)

dev.off()

#==============================================================================================================================
# All target FDR networks
#==============================================================================================================================
par(mfrow=c(4,2))

label_color <- rep("black", 101)
label_color[c(32,89,97)] <- "white"

setEPS()
postscript("Figs14.eps", width = 25, height = 50)  
par(mar = c(5.1, 4.1, 4.1, 2.1))
m <- matrix(c(1,2,3,4,5,6,7,8,9,9,10,10),nrow = 6,ncol =2,byrow = TRUE)
layout(mat = m,heights = c(0.5,0.5,0.5,0.5,0.1,0.15))


tulos_var1 <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0)
qgraph::qgraph(tulos_var1$adjacency_matrix,diag=F,usePCH=T,vsize= 3, label.color=label_color,
               color = colors, groups = indices,title="Target FDR = 0.0", legend=F,title.cex=4,label.cex=1.5, labels=T
)
sum(tulos_var$adjacency_matrix)/2

tulos_var2 <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.1)
qgraph::qgraph(tulos_var2$adjacency_matrix,diag=F,usePCH=T,vsize= 3, label.color=label_color,
               color = colors, groups = indices,title="Target FDR = 0.1", legend=F,title.cex=4,label.cex=1.5, labels=T
)
sum(tulos_var$adjacency_matrix)/2

tulos_var3 <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.20)
qgraph::qgraph(tulos_var3$adjacency_matrix,diag=F,usePCH=T,vsize= 3, label.color=label_color,
               color = colors, groups = indices,title="Target FDR = 0.2", legend=F,title.cex=4,label.cex=1.5, labels=T
)
sum(tulos_var$adjacency_matrix)/2

tulos_var4 <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.30)
qgraph::qgraph(tulos_var4$adjacency_matrix,diag=F,usePCH=T,vsize= 3, label.color=label_color,
               color = colors, groups = indices,title="Target FDR = 0.3", legend=F,title.cex=4,label.cex=1.5, labels=T
)
sum(tulos_var$adjacency_matrix)/2

tulos_var5 <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.4)
qgraph::qgraph(tulos_var5$adjacency_matrix,diag=F,usePCH=T,vsize= 3, label.color=label_color,
               color = colors, groups = indices,title="Target FDR = 0.4", legend=F,title.cex=4,label.cex=1.5, labels=T
)
sum(tulos_var$adjacency_matrix)/2

tulos_var6 <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.5)
qgraph::qgraph(tulos_var6$adjacency_matrix,diag=F,usePCH=T,vsize= 3, label.color=label_color,
               color = colors, groups = indices,title="Target FDR = 0.5", legend=F,title.cex=4,label.cex=1.5, labels=T
)
sum(tulos_var$adjacency_matrix)/2

tulos_var7 <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.6)
qgraph::qgraph(tulos_var7$adjacency_matrix,diag=F,usePCH=T,vsize= 3, label.color=label_color,
               color = colors, groups = indices,title="Target FDR = 0.6", legend=F,title.cex=4,label.cex=1.5, labels=T
)
sum(tulos_var$adjacency_matrix)/2


tulos_var8 <- HMFGraph::HMFGraph_GEM_FDR_control(tulos,permutations, target_FDR = 0.7)
qgraph::qgraph(tulos_var8$adjacency_matrix,diag=F,usePCH=T,vsize= 3, label.color=label_color,
               color = colors, groups = indices,title="Target FDR = 0.7", legend=F,title.cex=4,label.cex=1.5, labels=T
)
sum(tulos_var$adjacency_matrix)/2



par(mar = c(0.01, 0.01, 0.01, 0.01))
list_of_FDR <- c("0.7", "0.5", "0.4", "0.3", "0.2", "0.1")
legend_image <- as.raster(matrix(rev(colfunc(20)), ncol=1))
plot(c(0.1,0.7),c(-2,1),type = 'n', axes = F,xlab = '', ylab = '',ylim = rev(c(0.1,0.7)))
title("Target FDR ",cex.main =4, line = -8.5)
text(y=0.5, x = c(0.7,0.5,0.4,0.3,0.2,0.1), labels = c(0.7,0.5,0.4,0.3,0.2,0.1),cex = 4)


n <- 50  
x_vals <- seq(0.1, 0.7, length.out = n + 1)
cols <- colorRampPalette(c(viridis(6)))(n)

for (i in 1:n) {
  rect(x_vals[i],0.6,x_vals[i + 1],1, col = cols[i], border = NA)
}

par(mar = c(0.01, 0.01, 0.01, 0.01))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")

list_of_c_genes <- c("ribo_p (1)", "YXLE_at (97)"
                     ,"PCKA_at (32)", "YTGD_at (89)" 
                     ,"AHPC_at (8)", "YHZA_at (62)"
                     ,"YTCF_at (85)", "NDK_at (31)"
                     ,"GAP_at (18)", "RPSG_at (40)"
                     ,"YTGB_at (87)", "XHLA_at (44)")

colors_gene <- c()
colors_gene[1] <- colors[1]; colors_gene[2] <- colors[2]; colors_gene[3] <- colors[3]; colors_gene[4] <- colors[4]
colors_gene[c(5,6)] <- colors[5]; colors_gene[c(7,8)] <- colors[6]; colors_gene[c(9,10,11,12)] <- colors[7]

legend(x = "top",inset = 0,
       legend = list_of_c_genes,ncol=6,border=NA,
       col=rep("black", length(list_of_c_genes)), pt.bg=colors_gene, cex=4.5, xpd = TRUE,pch=21)

colnames(data_R)[tulos_var1$adjacency_matrix[,1]==1]
colnames(data_R)[tulos_var2$adjacency_matrix[,1]==1]
colnames(data_R)[tulos_var3$adjacency_matrix[,1]==1]
colnames(data_R)[tulos_var4$adjacency_matrix[,1]==1]
colnames(data_R)[tulos_var5$adjacency_matrix[,1]==1]
colnames(data_R)[tulos_var6$adjacency_matrix[,1]==1]
colnames(data_R)[tulos_var7$adjacency_matrix[,1]==1]
colnames(data_R)[tulos_var8$adjacency_matrix[,1]==1]

dev.off()


#==============================================================================================================================
# Here is a demonstration of how the expected number of connections impact the recovered correlation structure
# The result doesn't change significantly even with high number of expected connections.
#==============================================================================================================================
par(mfrow=c(4,2))


tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = 0)
qgraph::qgraph(tulos_var$adjacency_matrix)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = round(p/3,0) )
qgraph::qgraph(tulos_var$adjacency_matrix)
round(p/3,0)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = round(p/2,0))
qgraph::qgraph(tulos_var$adjacency_matrix)
round(p/2,0)
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p)
qgraph::qgraph(tulos_var$adjacency_matrix)
p
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*2)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*2
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*5)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*5
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*10)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*10
sum(tulos_var$adjacency_matrix)/2

tulos_var <- HMFGraph::HMFGraph_GEM_optimal_CI(tulos,permutations, expected_connections = p*100)
qgraph::qgraph(tulos_var$adjacency_matrix)
p*100
sum(tulos_var$adjacency_matrix)/2

