#' Computes predicted values given coefficients
#'
#' This function takes a data frame of coefficients in the form outputted by
#' functions like \code{multiple_linear_regression} or \code{ridge_regression}.
#'
#' It calculates the predicted values of a response variable from these coefficients.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param coefs A data frame of coefficient estimates
#'
#' @return A data frame of true and predicted values
#'
#' @import dplyr
#' @import data.table
#'
#' @export
predict_from_coefs <- function(dat, response, coefs){
  
  dt <- data.table(dat)
  y <- dt %>%  pull({{response}})
  x <- dt %>% select(-{{response}})
  
  #add column of 1's for intercept 
  x <- cbind(1, x)
  x <- as.matrix(x)
  
  coefs <- as.matrix(coefs)
  predictions <- x %*% t(coefs)
  
  results <- data.frame(cbind(actual_y = y, predicted_y = predictions))
  
  return(results)
}