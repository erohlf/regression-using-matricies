#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import data.table
#'
#' @export
#'
ridge_regression <- function(dat, response, lambda) {
  
  y <- dat %>%  pull({{response}})
  x <- dat %>% select(-{{response}})
  
  x <- scale(as.matrix(x))
  y <- as.matrix(y)

  results <- data.frame()

  for(i in lambda){
    ridge <- diag(i)
    betas <- cbind(data.frame(t(solve((t(x) %*% x) + ridge) %*% t(x) %*% y)), i)
    results <- rbind(results, betas)
    
  }  
  return(results)
  
}

#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#' @import data.table
#'
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambdas) {
  
  train <- ridge_regression(train_dat, response, lambdas)
  
  train <- train %>% select(-lambda)
  
  y <- test_dat %>% pull({{response}})
  
  lambda_errors <- data.frame()
  
  for(i in 1:nrow(train)){
    predictvals <- predict_from_coefs(test_dat, response, train[i,])
    
    error <- sum((predicvals[,2] - y)^2)
    
    nextrow <- cbind(lambda = lambda[i], error = error)
    
    lambda_errors <- rbind(lambda_errors, nextrow)
  }
  return(lambda_errors)
#lambda_errors is a data frame with lambda and its corresponding SSE
}