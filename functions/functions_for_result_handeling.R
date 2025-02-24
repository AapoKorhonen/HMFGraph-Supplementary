

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


calculate_results_from_file <- function(results, true_values){
  t1 <- dim(results)[3]  
  results_values <- matrix(0, nrow= t1, ncol=16)
  
  for(i in 1:t1){
    cm <- tarkkuus( true_values[,,i], results[,,i])
    object_cm <- calculate_scores(cm)
    object_cm[is.na(object_cm)] <- 0
    
    
    #global_true <- igraph::transitivity(igraph::graph_from_adjacency_matrix(true_values[,,i], mode = "undirected"), type = "global")

    
    global_est <- igraph::transitivity(igraph::graph_from_adjacency_matrix(results[,,i], mode = "undirected"), type = "average")

    cc_difference <- global_est

    community <- igraph::cluster_louvain( igraph::graph_from_adjacency_matrix(results[,,i], mode = "undirected"))

    predicted_clusters <- igraph::membership(community) 
    g <- igraph::graph_from_adjacency_matrix(true_values[,,i], mode = "undirected")
    components <- igraph::components(g)
    true_clusters <- components$membership
    nmi_score <- aricode::NMI(as.vector(true_clusters), as.vector(predicted_clusters))

    
    if(is.na(cc_difference)) cc_difference <- 1
    measurements <- c(object_cm$ACC, object_cm$ACC_bal, object_cm$MCC, object_cm$F1,
                      object_cm$TPR, object_cm$TNR, object_cm$PPV, object_cm$NPV,
                      object_cm$FNR, object_cm$FPR, object_cm$FOR, object_cm$LRp,
                      object_cm$LRn, object_cm$FDR, cc_difference, nmi_score )
    
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
