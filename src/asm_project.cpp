set.seed(1)
  
# create 3D cities
  n <- 25
  cities <- data.frame(
      x = runif(n),
      y = runif(n),
      z = runif(n)
  )
    
# distance
    dist3d <- function(a,b){
      sqrt(sum((a-b)^2))
    }
    
    total_dist <- function(route){
      sum(sapply(1:(length(route)-1), function(i){
        dist3d(cities[route[i],], cities[route[i+1],])
      })) +
        dist3d(cities[route[length(route)],], cities[route[1],])
    }
    
# SA
    current <- sample(1:n)
      best <- current
    init <- current
    
    T <- 1
    cooling <- 0.995
    
    for(iter in 1:5000){
      
      i <- sample(1:n,1)
      j <- sample(setdiff(1:n,i),1)
      
      new <- current
      a <- min(i,j)
      b <- max(i,j)
      new[a:b] <- rev(new[a:b])
      
      d_old <- total_dist(current)
      d_new <- total_dist(new)
      
      if(d_new < d_old || runif(1) < exp((d_old - d_new)/T)){
        current <- new
      }
      
      if(total_dist(current) < total_dist(best)){
        best <- current
      }
      
      T <- T * cooling
    }