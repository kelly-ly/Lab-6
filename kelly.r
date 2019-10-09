RNGkind(sample.kind = "Rounding")
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

# 1.1.2 Brute force search
brute_force_knapsack <- function(x, W){
  stopifnot(all(x > 0) && W > 0)
  num_knapsack <- nrow(x)
  comb_list <- lapply(seq_len(num_knapsack), FUN = function(i) combn(1:num_knapsack, i))
  total_value <- 0
  elements <- vector()
  for (i in comb_list){
    for(j in 1:ncol(i)){
      if(sum(x[i[,j],1]) <= W && sum(x[i[,j],2]) > total_value){
        total_value <- sum(x[i[,j],2])
        elements <- i[,j]
      }
    }
  }
  result <- list(value = total_value, elements = elements)
  return(result)
}

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)

# 1.1.3 Dynamic programming
knapsack_dynamic <- function(x, W){
  stopifnot(all(x > 0) && W > 0)
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
        dynamic_matrix[i, j] <- max(dynamic_matrix[i-1, j],dynamic_matrix[i-1, j - w[i-1]] + v[i-1])
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

knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)


# 1.1.4 Greedy heuristic
greedy_knapsack <- function(x, W){
  x["r"] <- x$v / x$w
  total_value <- 0
  total_weight<- 0
  elements <- vector()
  flag <- TRUE
  while(W > 0 && nrow(x) != 0){
    n <- which.max(x$r)
    # cat("n: ", n, "lenght: ", nrow(x), "\n")
    if(W >= x$w[n]){
      elements <- c(elements, as.numeric(row.names(x[n,])))
      total_value <- total_value + x$v[n]
      total_weight <- total_weight + x$w[n]
      W <- W - x$w[n]
    }
    x <- x[-n, ]
  }
  result <- list(value = total_value, weight = total_weight, elements = elements)
  return(result)
}

greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

# list <- greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)$elements
# a <- knapsack_objects[1:1200,][list,]
# print(nrow(a))
# print(a)
# print(sum(a$w))
# print(sum(a$v))

