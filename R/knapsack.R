# 1.1.2 Brute force search

#' Brute force search
#' @param x knapsack objects - data frame
#' @param W capacity of knapsack - integer
#' @param parallel parallelize - logical
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame(
#'   w=sample(1:4000, size = n, replace = TRUE),
#'   v=runif(n = n, 0, 10000)
#' )
#' 
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
#' brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
#' @export
#' @import foreach doParallel parallel

brute_force_knapsack <- function(x, W, parallel = FALSE){
  stopifnot(is.data.frame(x) && all(x > 0) && W > 0)
  total_value <- 0
  elements <- vector()
  if(parallel == FALSE){
    for(i in 1:(2^nrow(x)-1)){
      ids <- which(intToBits(i) == 01)
      if(sum(x$w[ids]) <= W && sum(x$v[ids]) > total_value){
        total_value <- sum(x$v[ids])
        elements <- ids
      }
    }
  }else{
    cl <- makeCluster(4)
    registerDoParallel(cl)
    foreach(i = 1:(2^nrow(x)-1)) %do% {
      ids <- which(intToBits(i) == 01)
      if(sum(x$w[ids]) <= W && sum(x$v[ids]) > total_value){
        total_value <- sum(x$v[ids])
        elements <- ids
      }
    }
    stopCluster(cl)
  }
  result <- list(value = total_value, elements = elements)
  return(result)
}


# 1.1.3 Dynamic programming

#' Dynamic programming
#' @param x knapsack objects - data frame
#' @param W capacity of knapsack - integer
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame(
#'   w=sample(1:4000, size = n, replace = TRUE),
#'   v=runif(n = n, 0, 10000)
#' )
#' 
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
#' knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)
#' @export

knapsack_dynamic <- function(x, W){
  stopifnot(is.data.frame(x) && all(x > 0) && W > 0)
  n <- nrow(x)
  dynamic_matrix <- matrix(0, nrow = n + 1, ncol = W+1)
  w <- x$w
  v <- x$v
  total_value <- 0
  elements <- vector()
  for(i in 1 : n+1){
    for(j in 1 : W+1){
      if(j-1 < w[i-1]){
        dynamic_matrix[i, j] <- dynamic_matrix[i-1, j]
      }else{
        pri_value <- 
          dynamic_matrix[i, j] <- max(dynamic_matrix[i-1, j], dynamic_matrix[i-1, j-w[i-1]] + v[i-1])
      }
    }
  }
  temp_value <- max(dynamic_matrix)
  while(temp_value > 0){
    n <- which(dynamic_matrix == temp_value, arr.ind = TRUE)[1,1]
    elements <- c(as.numeric(n-1), elements)
    temp_value <- temp_value - v[n-1]
  }
  result <- list(value = max(dynamic_matrix), elements = elements)
  return(result)
}


# 1.1.4 Greedy heuristic

#' Greedy heuristic
#' @param x knapsack objects - data frame
#' @param W capacity of knapsack - integer
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <- data.frame(
#'   w=sample(1:4000, size = n, replace = TRUE),
#'   v=runif(n = n, 0, 10000)
#' )
#' 
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
#' @export
#' 
greedy_knapsack <- function(x, W){
  stopifnot(is.data.frame(x) && all(x > 0) && W > 0)
  # ratio <- x$v/x$w
  x <- x[order(x$v/x$w, decreasing = TRUE),]
  i <- 1
  while(sum(x$w[1:i]) <= W) {
    i = i + 1
  }
  total_value <- sum(x$v[1:(i-1)])
  elements <- as.numeric( rownames(x[1:(i-1),]))
  result <- list(value = total_value, elements = elements)
  return(result)
}
