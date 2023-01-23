

Comp_priors <- function(train_labels) {
  #' Compute the priors of each class label 
  #' 
  #' @param train_labels a vector of labels with length equal to n
  #' @return a probability vector of length K = 10
  
  
  K <- 10
  pi_vec <- rep(0, K)
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
  # prior is the proportion of each label in the dataset
  
  zero = sum(train_labels == 0)/length(train_labels)
  one = sum(train_labels == 1)/length(train_labels)
  two = sum(train_labels == 2)/length(train_labels)
  three = sum(train_labels == 3)/length(train_labels)
  four = sum(train_labels == 4)/length(train_labels)
  five = sum(train_labels == 5)/length(train_labels)
  six = sum(train_labels == 6)/length(train_labels)
  seven = sum(train_labels == 7)/length(train_labels)
  eight = sum(train_labels == 8)/length(train_labels)
  nine = sum(train_labels == 9)/length(train_labels)
  
  pi_vec = c(zero, one, two, three, four, five, six, seven, eight, nine)
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(pi_vec)
}
  
Vector_means <- function(){
  
  
}

Comp_cond_means <- function(train_data, train_labels) {
  #' Compute the conditional means of each class 
  #' 
  #' @param train_data a n by p matrix containing p features of n training points
  #' @param train_labels a vector of labels with length equal to n
  #' 
  #' @return a p by 10 matrix, each .
  
  K <- 10
  p <- ncol(train_data)
  mean_mat <- matrix(0, p, K)
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
  # each row is a class, each column is for a predictor 
  # supposed to be average, not proportion
  # subset the data by labels 
  
  index = 1:K
  new = list()
  
  for (i in index) {
    indexes = which(train_labels == index[i]-1)
    #indexes = which(train_labels == index[[1]][i]-1)
    if (length(indexes) != 0){
      new = append(new, list(indexes))
    }
    else {
      new = append(new, list(0))
    }
  }
  
  
  for (i in index){
    mean_mat[,i] = colMeans(train_data[new[[i]],])
  }
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(mean_mat)
}



Comp_cond_covs <- function(train_data, train_labels, cov_equal = FALSE) {
  #' Compute the conditional covariance matrix of each class
  #' 
  #' @param train_data a n by p matrix containing p features of n training points
  #' @param train_labels a vector of labels with length equal to n
  #' @param cov_equal TRUE if all conditional covariance matrices are equal, 
  #'   otherwise, FALSE 
  #' 
  #' @return 
  #'  if \code{cov_equal} is FALSE, return an array with dimension (p, p, K),
  #'    containing p by p covariance matrices of each class;
  #'  else, return a p by p covariance matrix. 
  
  K <- 10
  p <- ncol(train_data)
  
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
  
  cov_arr <- array(0, c(p, p, K))
  library(abind)
  
  
  if (cov_equal == FALSE){
    index = 1:K
    new = list()
    
    p <- ncol(data)
    #mean_mat <- matrix(0, p, K)
    
    for (i in index) {
      indexes = which(train_labels == index[i]-1)
      if (length(indexes) != 0){ #special case when there's a missing label in the dataset
        new = append(new, list(indexes))
      }
      else {
        new = append(new, list(0))
      }
    }
    for (i in index){
      #cov_arr = abind(cov_arr, cov(train_data[new[[i]],]))
      cov_arr[,,i] = cov(train_data[new[[i]],])
    }}
  
  else {
    cov_arr = cov(train_data)
  }
  
  
  return(cov_arr)
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
}




Predict_posterior <- function(test_data, priors, means, covs, cov_equal) {
  
  #' Predict the posterior probabilities of each class 
  #'
  #' @param test_data a n_test by p feature matrix 
  #' @param priors a vector of prior probabilities with length equal to K
  #' @param means a p by K matrix containing conditional means given each class
  #' @param covs covariance matrices of each class, depending on \code{cov_equal}
  #' @param cov_equal TRUE if all conditional covariance matrices are equal; 
  #'   otherwise FALSE.
  #'   
  #' @return a n_test by K matrix: each row contains the posterior probabilities 
  #'   of each class.
  
  n_test <- nrow(test_data)
  K <- length(priors)
  posteriors <- matrix(0, n_test, K)
  test_data = data.frame(test_data)
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
 
  p = ncol(test_data)
  denom = 0  
  
  if (cov_equal == FALSE){
    for (i in 1:K){
      for (j in 1:n_test){
        density = dmvnorm(x = test_data[j,],means[,i], covs[,,i])
        num = density * priors[i]
        denom = 0
        for (k in 1:K){
          denom = denom + dmvnorm(x = test_data[j,],means[,k], covs[,,k]) * priors[k]
        }
       posteriors[j, i] = num/denom 
      }
    }
  }
  else{
    for (i in 1:K){
      for (j in 1:n_test){
        density = dmvnorm(x = test_data[j,],means[,i], covs)
        num = density * priors[i]
        denom = 0
        for (k in 1:K){
          denom = denom + dmvnorm(x = test_data[j,],means[,k], covs) * priors[k]
        }
        posteriors[j, i] = num/denom 
      }
    }
  }
  
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(posteriors)
}




Predict_labels <- function(posteriors) {
  
  #' Predict labels based on the posterior probabilities over K classes
  #' 
  #' @param posteriors A n by K posterior probabilities
  #' 
  #' @return A vector of predicted labels with length equal to n
  
  n_test <- nrow(posteriors)
  pred_labels <- rep(NA, n_test)
  
  #####################################################################
  #  TODO                                                             #
  #####################################################################
  
  pred_labels = max.col(posteriors)-1
  
  #####################################################################
  #                       END OF YOUR CODE                            #
  #####################################################################
  
  return(pred_labels)
}




