set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

brute_force_knapsack1 = function(x, W){
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


brute_force_knapsack1(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack1(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack1(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack1(x = knapsack_objects[1:12,], W = 2000)


brute_force_knapsack = function(x, W){
  stopifnot(all(x > 0) && W > 0)
  num_knapsack <- nrow(x)
  comb_list <- combn(seq_len(num_knapsack), 2)
  
  total_value <- 0
  elements <- vector()
  for (i in 1:ncol(comb_list)){
    if(sum(x[comb_list[,i],1]) <= W && sum(x[comb_list[,i],2]) > total_value){
      total_value <- sum(x[comb_list[,i],2])
      elements <- comb_list[,i]
    }
  }
  result <- list(value = total_value, elements= elements)
  return(result)
}

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)


print(knapsack_objects[1:8,])
