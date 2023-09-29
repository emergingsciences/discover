# Use the results of clustCombi to combine clusters. clustCombi doesn't
# come with a function to operationalize the cluster combinations.
# This function uses the results of the combiM object in the clustCombi()
# results to perform the re-classification.
#
# Parameters:
# mod - Model output from Mclust
# combiM - The combiM object from clustCombi()
# num_clust - The number of clusters you want to end up with
#
# Returns a modified mod$classification with clusters combined
reassignClusters <- function(mod, combiM, num_clust) {
  # Create a copy of the original classifications
  new_classifications <- mod$classification
  
  # Determine the starting index based on the number of clusters
  start_index <- length(combiM)
  end_index <- num_clust
  
  # Iterate through combiM matrices, starting from the last and moving backward
  for (i in start_index:end_index) {
    print(i)
    # Identify the diagonal (unchanged) elements (1) and the off-diagonal (reassigned) elements (1)
    # unchanged_elements <- which(diag(comb[[i]]) == 1)
    reassigned_elements <- which(combiM[[i]] == 1 & row(combiM[[i]]) != col(combiM[[i]]))
    
    # Update the cluster assignments in new_classifications based on off-diagonal elements
    for (reassigned_elements in reassigned_elements) {
      # print(paste("i =", i, ", j =", j))
      row_idx <- row(combiM[[i]])[reassigned_elements]
      col_idx <- col(combiM[[i]])[reassigned_elements]
      
      # Find values in mod$classification that match the col_idx and update them to row_idx
      new_classifications[new_classifications == col_idx] <- row_idx
    }
  }
  
  return(new_classifications)
}