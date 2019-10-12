library(lineprof)
source("knapsack.r")

l <- lineprof(brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE))
l

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500, parallel = TRUE)
