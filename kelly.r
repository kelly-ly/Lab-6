set.seed(42)
n <- 2000
knapsack_objets <- data.frame(
  w = sample(1:4000, size = n, replace = TRUE),
  v = runif(n = n, 0, 10000)
)

brute_force_knapsack = function(x, W){
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
  result <- list(value = total_value, elements= elements)
  return(result)
}

brute_force_knapsack(x = knapsack_objets[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objets[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objets[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objets[1:12,], W = 2000)
