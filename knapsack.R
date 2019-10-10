set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
knapsack_objects

brute_force_knapsack<-function(x,W){
  if(ncol(x)!=2)stop("exceed two variable")
  if(W<=0)stop("not postive value")
  max_value=0
  max_items<-c()
  for(i in 1:2^nrow(x)-1){
    a<-intToBits(i)
    number_of_items<-c()
    value_of_items<-0
    weight_of_items<-0
    for(j in 1:length(a)){
      if(a[j]==1)
      {
        number_of_items<-c(number_of_items,j)
        print(number_of_items)
        weight_of_items<-weight_of_items+x$w[j]
        value_of_items<-value_of_items+x$v[j]
      }
    }
    if(weight_of_items<W){
      if(value_of_items>max_value){
        max_value=value_of_items
        max_items=number_of_items
      }
    }
  }
  return(list(value=max_value,element=max_items))
}

# brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
# brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
# brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
# brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)


knapsack_dynamic<-function(x,W){
  v<-x$v
  w<-x$w
  n<-nrow(x)
  m<-matrix(0,nrow=n+1,ncol = W+1)
  max<-0
  max_pos<-c()
  
  for(i in 1:n+1){
    for(j in 1:W+1){
      if(j-1<w[i-1]){
        m[i,j]=m[i-1,j]
      }
      else{
        m[i,j]=max(m[i-1,j],m[i-1,j-w[i-1]]+v[i-1])
        if(m[i,j]>max){
          max=m[i,j]
          max_pos<-c(i,j)
        }
      }
    }
  }
  
  max_element<-c(max_pos[1]-1)
  
  total_weight<-max_pos[2]-1
  
  max_element<-c(which(w==(total_weight-w[max_element])),max_element)
  
  
  return(list(value=max,elements=max_element))
}
# knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
# knapsack_dynamic(x = knapsack_objects[1:12,], W = 3500)
# knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
# knapsack_dynamic(x = knapsack_objects[1:12,], W = 2000)


greedy_knapsack<-function(x,W){
  new_x<-x[order(x$v,decreasing = TRUE),]
  v<-new_x$v

  w<-new_x$w

  n<-nrow(new_x)
  
  greedy_w<-W
  max_value<-0
  max_pos<-c()
  for(i in 1:n){
    if(w[i]>W){
      next
    }
    else{
      greedy_w<-w[i]
      temp_value<-v[i]
      temp_pos<-c(i)
    }
    for(j in (i+1):n){
      if(j==n+1) break
      if(greedy_w+w[j]<W){
        greedy_w<-greedy_w+w[j]
        temp_pos<-c(temp_pos,j)
        temp_value<-temp_value+v[j]
      }
    }

    if(temp_value>max_value){
      max_value<-temp_value
      max_pos<-temp_pos
    }
  }
  
  return(list(new_x,max_value,max_pos))
  
}
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
knapsack_objects[1:8,]
# 
# knapsack_objects[1:8,]
# 
# v<-c(1,6,18,22,28)
# w<-c(1,2,5,6,7)
# n<-5
# W<-11
# m<-matrix(0,nrow=n+1,ncol=W+1)
# max<-0
# max_pos<-c()
# 
# for(i in 1:n+1){
#   for(j in 1:W+1){
#     if(j-1<w[i-1]){
#       m[i,j]=m[i-1,j]
#     }
#     else{
#       m[i,j]=max(m[i-1,j],m[i-1,j-w[i-1]]+v[i-1])
#       if(m[i,j]>max){
#         max=m[i,j]
#         max_pos<-c(i,j)
#         }
#     }
#   }
# }
# m
# max_pos[1]-1
# 
# for(i in 2:n+1){
#   for(j in 1:W+1){
#     if(w[i-1]>j){
#       m[i-1,j]=m[i-2,j]
#     }
#     else{
#       m[i-1,j]=max(m[i-2,j],v[i-1]+m[i-2,j-w[i-1]])
#     }
#   }
# }
# 
# m
# 
# 