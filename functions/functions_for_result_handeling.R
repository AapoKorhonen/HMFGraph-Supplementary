

vector_to_array <- function(vector_lower_diagonal, p){
  
  t1 <- dim(vector_lower_diagonal)[2]
  
  array_of_matrices <- array(0, c(p,p,t1))
  
  for(i in 1:t1){
    
    new_matrix <- diag(p)
    
    new_matrix[lower.tri(new_matrix,diag=T)] <- vector_lower_diagonal[,i]
    
    new_matrix <- new_matrix + t(new_matrix)
    
    diag(new_matrix) <- 0
    array_of_matrices[,,i] <- new_matrix
  }
  return(array_of_matrices)
}


calculate_scores <- function(cm) {
  tp <- cm[1,1]
  tn <- cm[2,2]
  fp <- cm[2,1]
  fn <- cm[1,2]
  tpr <- tp / (tp + fn)
  tnr <- tn / (tn + fp)
  ppv <- tp / (tp + fp)
  npv <- tn / (tn + fn)
  fnr <- 1 - tpr
  fpr <- 1 - tnr
  fdr <- 1 - ppv
  FOR <- 1 - npv
  lr_plus <- tpr / fpr
  lr_neg <- fnr / tnr
  pt <- sqrt(fpr) / (sqrt(tpr) + sqrt(fpr))
  ts <- tp / (tp + fn + fp)
  acc <- (tp ) / (tp+ fp)
  bal_acc <- (tpr + tnr) / 2
  F1_score <- 2 * (ppv * tpr) / (ppv + tpr)
  mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  
  
  results <- data.frame(ACC = acc, ACC_bal = bal_acc, MCC = mcc, F1 = F1_score,
                        TPR = tpr, TNR = tnr, PPV = ppv, NPV = npv, FNR = fnr,
                        FPR = fpr, FOR = FOR, LRp = lr_plus, LRn = lr_neg,FDR=fdr)
  return(results)
}

tarkkuus <- function(matrix1, matrix2){
  
  samat <- matrix1*matrix2
  
  fpos <- matrix2 - samat 
  fneg <- matrix1- samat
  
  
  conf_matrix <- matrix(1, 2,2)
  
  sum1 <- max(sum(matrix1[lower.tri(matrix1, diag = FALSE)]), sum(matrix1[upper.tri(matrix1, diag = FALSE)]))
  
  neg <- (((dim(matrix1)[1]^2)-dim(matrix1)[1] )  /2 ) -  sum1
  
  sum2 <- max(sum(matrix2[lower.tri(matrix2, diag = FALSE)]), sum(matrix2[upper.tri(matrix2, diag = FALSE)]))
  
  sum_samat <- max(sum(samat[lower.tri(samat, diag = FALSE)]), sum(samat[upper.tri(samat, diag = FALSE)]))
  
  sum_fp <- max(sum(fpos[lower.tri(fpos, diag = FALSE)]), sum(fpos[upper.tri(fpos, diag = FALSE)]))
  
  sum_fn <- min(sum(fneg[lower.tri(fneg, diag = FALSE)]), sum(fneg[upper.tri(fneg, diag = FALSE)]))
  
  
  conf_matrix[1,1] <- sum_samat
  conf_matrix[2,1] <- sum_fp
  conf_matrix[1,2] <- sum_fn
  conf_matrix[2,2] <- neg - sum_fp
  
  colnames(conf_matrix) <- c("Est Pos", "Est Neg") 
  rownames(conf_matrix) <- c("True Pos", "True Neg")
  
  koko <- sum1 + neg 
  oikeat <- sum1
  
  
  tulos <- matrix(c(sum_samat,  sum_fp ,oikeat  , koko) ,2 ,2)

  return(conf_matrix)
}




calculate_results_from_file <- function(results, true_values){
  t1 <- dim(results)[3]  
  results_values <- matrix(0, nrow= t1, ncol=16)
  
  for(i in 1:t1){
    cm <- tarkkuus( true_values[,,i], results[,,i])
    object_cm <- calculate_scores(cm)
    object_cm[is.na(object_cm)] <- 0
    

    global_est <- igraph::transitivity(igraph::graph_from_adjacency_matrix(results[,,i], mode = "undirected"), type = "average")

    average_clustering_coefficent <- global_est

    community <- igraph::cluster_louvain( igraph::graph_from_adjacency_matrix(results[,,i], mode = "undirected"))

    predicted_clusters <- igraph::membership(community) 
    g <- igraph::graph_from_adjacency_matrix(true_values[,,i], mode = "undirected")
    components <- igraph::components(g)
    true_clusters <- components$membership
    nmi_score <- aricode::NMI(as.vector(true_clusters), as.vector(predicted_clusters))

    
    if(is.na(average_clustering_coefficent)) average_clustering_coefficent <- 1
    measurements <- c(object_cm$ACC, object_cm$ACC_bal, object_cm$MCC, object_cm$F1,
                      object_cm$TPR, object_cm$TNR, object_cm$PPV, object_cm$NPV,
                      object_cm$FNR, object_cm$FPR, object_cm$FOR, object_cm$LRp,
                      object_cm$LRn, object_cm$FDR, average_clustering_coefficent, nmi_score )
    
    results_values[i,] <- measurements
    
    
  }
  return(results_values)
}





print_results <- function(files, comparison_files,values = 1, round1 = 1, round2 = 2){
  
  
  
  print_text <- c("n = 35",
                  "n = 75",
                  "n = 150",
                  "n = 300")
  
  
  names <- c("accuracy", "bal-accuracy", "MCC", "F1", "TPR", "TNR", "PPV","NPV", "FNR",
             "FPR","FOR", "LRp", "LRn", "FDR", "ACC", "NMI")
  cat( "\n    ") 
  
  for(j in 1:length(values)){
    
    cat(paste0( " | ", names[values[j]] ))
  }
  
  cat("\n")
  
  for(i in 1:4){
    
    cat(paste0(print_text[i], " ") )  
    
    scores_all <- calculate_results_from_file(files[[i]], comparison_files[[i]])
    
    
    
    
    for (j in 1:length(values)){
      
      
      
      cat(paste0(" | " ,round(mean(scores_all[,values[j]]),round1), " ", "(", round(sd(scores_all[,values[j]]),round2), ")", " " ))  
      
      
    }
    
    cat("\n")  
    
    
    
  }
  
}
