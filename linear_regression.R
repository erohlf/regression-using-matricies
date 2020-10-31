#' Implements simple linear regression by hand
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import data.table
#'
#' @export
simple_linear_regression <- function(dat, response, explanatory){
  
  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})
  
  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()
  
  x_bar <- mean(x)
  y_bar <- mean(y)
  
  
  sd_x <- sqrt((sum(x - x_bar)^2)/length(x))
  sd_y <- sqrt((sum(y - y_bar)^2)/length(y))
  
  x <- cbind(1, x)
  betas <- solve(t(x) %*% x) %*% t(x) %*% y
  
  beta_0 <- betas[1]
  beta_1 <- betas[2]
  
  
  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )
  
  names(results)[2] <- explan_name
  
  return(results)
  
}



#' Implements linear regression with many predictors by hand
#'
#' This function computes coefficients for multiple regression by hand.
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import data.table
#'
#' @export

multiple_linear_regression <- function(dat, response) {
  
  y <- dat %>% pull({{response}})
  x <- dat %>% pull(-{{response}})
  
  x <- as.matrix(x)
  y <- as.matrix(y)
  
  betas <- solve(t(x) %*% x) %*% t(x) %*% y
  
  
  results <- data.table(t(betas))
  
  return(results)
}