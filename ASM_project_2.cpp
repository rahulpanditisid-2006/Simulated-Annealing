library(Rcpp)
  
  cppFunction('
                NumericVector two_opt_cpp(NumericMatrix cities, int iters){
                  
                  int n = cities.nrow();
                  
                  // distance function
                  auto dist = [&](int i, int j){
                    double dx = cities(i,0) - cities(j,0);
                    double dy = cities(i,1) - cities(j,1);
                    return sqrt(dx*dx + dy*dy);
                  };
                  
                  // compute total route length
                  auto route_dist = [&](IntegerVector route){
                    double d = 0.0;
                    for(int i=0; i<n-1; i++){
                      d += dist(route[i]-1, route[i+1]-1);
                    }
                    d += dist(route[n-1]-1, route[0]-1);
                    return d;
                  };
                  
                  // initial random route
                  IntegerVector current = Rcpp::sample(n, n, false);
                  
                  double current_cost = route_dist(current);
                  
                  NumericVector costs(iters);
                  
                  for(int iter=0; iter<iters; iter++){
                    
                    // pick two indices
                    int i = floor(R::runif(0,n));
                    int j = floor(R::runif(0,n));
                    
                    if(i == j){
                      costs[iter] = current_cost;
                      continue;
                    }
                    
                    int a = std::min(i,j);
                    int b = std::max(i,j);
                    
                    // create new route by reversing segment
                    IntegerVector new_route = clone(current);
                    
                    while(a < b){
                      int temp = new_route[a];
                      new_route[a] = new_route[b];
                      new_route[b] = temp;
                      a++; b--;
                    }
                    
                    double new_cost = route_dist(new_route);
                    
                    // accept only if improvement (pure 2-opt)
                    if(new_cost < current_cost){
                      current = new_route;
                      current_cost = new_cost;
                    }
                    
                    costs[iter] = current_cost;
                  }
                  
                  return costs;
                }
                ')
  grid_size <- 100

x <- rep(1:grid_size, each=grid_size)
  y <- rep(1:grid_size, grid_size)
  
  cities <- cbind(x, y)
  set.seed(1)
  
  costs_2opt <- two_opt_cpp(cities, 2000)
  plot(costs_2opt, type="l", col="purple", lwd=2,
       xlab="Iterations", ylab="Tour Length",
       main="2-opt Convergence (2000 iterations)")
  
  

  
  
  library(Rcpp)
  
  cppFunction('
                NumericVector SA_cpp(NumericMatrix cities, int iters){
                  
                  int n = cities.nrow();
                  
                  // distance function
                  auto dist = [&](int i, int j){
                    double dx = cities(i,0) - cities(j,0);
                    double dy = cities(i,1) - cities(j,1);
                    return sqrt(dx*dx + dy*dy);
                  };
                  
                  // compute route length
                  auto route_dist = [&](IntegerVector route){
                    double d = 0.0;
                    for(int i=0; i<n-1; i++){
                      d += dist(route[i]-1, route[i+1]-1);
                    }
                    d += dist(route[n-1]-1, route[0]-1);
                    return d;
                  };
                  
                  // initial route
                  IntegerVector current = Rcpp::sample(n, n, false);
                  double current_cost = route_dist(current);
                  
                  // temperature parameters
                  double T = 1.0;
                  double cooling = 0.995;
                  
                  NumericVector costs(iters);
                  
                  for(int iter=0; iter<iters; iter++){
                    
                    // pick two indices
                    int i = floor(R::runif(0,n));
                    int j = floor(R::runif(0,n));
                    
                    if(i == j){
                      costs[iter] = current_cost;
                      continue;
                    }
                    
                    int a = std::min(i,j);
                    int b = std::max(i,j);
                    
                    // create new route (2-opt move)
                    IntegerVector new_route = clone(current);
                    
                    while(a < b){
                      int temp = new_route[a];
                      new_route[a] = new_route[b];
                      new_route[b] = temp;
                      a++; b--;
                    }
                    
                    double new_cost = route_dist(new_route);
                    
                    double delta = new_cost - current_cost;
                    
                    // acceptance rule
                    if(delta < 0 || R::runif(0,1) < exp(-delta/T)){
                      current = new_route;
                      current_cost = new_cost;
                    }
                    
                    costs[iter] = current_cost;
                    
                    // cool down
                    T = T * cooling;
                  }
                  
                  return costs;
                }
                ')
  grid_size <- 100

x <- rep(1:grid_size, each=grid_size)
  y <- rep(1:grid_size, grid_size)
  
  cities <- cbind(x, y)
  grid_size <- 100

x <- rep(1:grid_size, each=grid_size)
  y <- rep(1:grid_size, grid_size)
  
  cities <- cbind(x, y)
  set.seed(1)
  
  costs_SA <- SA_cpp(cities, 2000)
  plot(costs_SA, type="l", col="red", lwd=2,
       xlab="Iterations", ylab="Tour Length",
       main="Simulated Annealing (2000 iterations)")

  
  
  
  
  library(Rcpp)
  
  cppFunction('
                NumericVector ACO_cpp(NumericMatrix cities, int iters, int n_ants){
                  
                  int n = cities.nrow();
                  
                  // distance function
                  auto dist = [&](int i, int j){
                    double dx = cities(i,0) - cities(j,0);
                    double dy = cities(i,1) - cities(j,1);
                    return sqrt(dx*dx + dy*dy) + 1e-6; // avoid zero
                  };
                  
                  // initialize pheromone matrix
                  NumericMatrix tau(n,n);
                  for(int i=0;i<n;i++){
                    for(int j=0;j<n;j++){
                      tau(i,j) = 1.0;
                    }
                  }
                  
                  double alpha = 1.0;   // pheromone importance
                  double beta = 2.0;    // distance importance
                  double rho = 0.5;     // evaporation
                  
                  NumericVector best_costs(iters);
                  double global_best = 1e9;
                  
                  for(int iter=0; iter<iters; iter++){
                    
                    List ant_routes(n_ants);
                    NumericVector ant_costs(n_ants);
                    
                    for(int k=0; k<n_ants; k++){
                      
                      LogicalVector visited(n,false);
                      IntegerVector route(n);
                      
                      int current = floor(R::runif(0,n));
                      route[0] = current+1;
                      visited[current] = true;
                      
                      // build route
                      for(int step=1; step<n; step++){
                        
                        NumericVector probs(n);
                        double sum_prob = 0.0;
                        
                        for(int j=0;j<n;j++){
                          if(!visited[j]){
                            double p = pow(tau(current,j),alpha) * pow(1.0/dist(current,j),beta);
                            probs[j] = p;
                            sum_prob += p;
                          }
                        }
                        
                        // roulette wheel selection
                        double r = R::runif(0, sum_prob);
                        double cumulative = 0.0;
                        
                        int next_city = -1;
                        
                        for(int j=0;j<n;j++){
                          if(!visited[j]){
                            cumulative += probs[j];
                            if(cumulative >= r){
                              next_city = j;
                              break;
                            }
                          }
                        }
                        
                        route[step] = next_city+1;
                        visited[next_city] = true;
                        current = next_city;
                      }
                      
                      // compute cost
                      double d = 0.0;
                      for(int i=0;i<n-1;i++){
                        d += dist(route[i]-1, route[i+1]-1);
                      }
                      d += dist(route[n-1]-1, route[0]-1);
                      
                      ant_routes[k] = route;
                      ant_costs[k] = d;
                      
                      if(d < global_best){
                        global_best = d;
                      }
                    }
                    
                    // evaporation
                    for(int i=0;i<n;i++){
                      for(int j=0;j<n;j++){
                        tau(i,j) *= (1.0 - rho);
                      }
                    }
                    
                    // pheromone update
                    for(int k=0; k<n_ants; k++){
                      IntegerVector route = ant_routes[k];
                      double d = ant_costs[k];
                      
                      for(int i=0;i<n-1;i++){
                        int a = route[i]-1;
                        int b = route[i+1]-1;
                        tau(a,b) += 1.0/d;
                        tau(b,a) += 1.0/d;
                      }
                      
                      // closing edge
                      int a = route[n-1]-1;
                      int b = route[0]-1;
                      tau(a,b) += 1.0/d;
                      tau(b,a) += 1.0/d;
                    }
                    
                    best_costs[iter] = global_best;
                  }
                  
                  return best_costs;
                }
                ')
  grid_size <- 100

x <- rep(1:grid_size, each=grid_size)
  y <- rep(1:grid_size, grid_size)
  
  cities <- cbind(x, y)
  set.seed(1)
  
  costs_ACO <- ACO_cpp(cities, iters=2000, n_ants=30)
  plot(costs_ACO, type="l", col="darkgreen", lwd=2,
       xlab="Iterations", ylab="Tour Length",
       main="Ant Colony Optimization (2000 iterations)")
  

  
  
  
  
  
  plot(costs_SA, type="l", col="red", lwd=2,
       xlab="Iterations", ylab="Tour Length",
       main="All Algorithms Comparison",
       ylim=range(c(costs_SA, costs_2opt, costs_GA, costs_ACO)))
  
  lines(costs_2opt, col="purple", lwd=2)
  lines(costs_GA, col="blue", lwd=2)
  lines(costs_ACO, col="darkgreen", lwd=2)
  
  legend("topright",
         legend=c("Simulated Annealing",
                  "2-opt",
                  "Genetic Algorithm",
                  "ACO"),
                  col=c("red","purple","blue","darkgreen"),
                  lwd=2)
  
  
  
  
  
  
  
  
  
  grid_size <- 100

x <- rep(1:grid_size, each=grid_size)
  y <- rep(1:grid_size, grid_size)
  
  cities <- cbind(x, y)
  set.seed(1)
  
# IMPORTANT: these functions must already exist
# SA_cpp, two_opt_cpp, GA_grid_cpp, ACO_cpp
  
  costs_SA   <- SA_cpp(cities, 2000)
    costs_2opt <- two_opt_cpp(cities, 2000)
    costs_GA   <- GA_grid_cpp(cities, 2000, 50)
    costs_ACO  <- ACO_cpp(cities, 2000, 30)
    costs_SA   <- cummin(costs_SA)
    costs_2opt <- cummin(costs_2opt)
    costs_GA   <- cummin(costs_GA)
    costs_ACO  <- cummin(costs_ACO)
    plot(costs_SA, type="l", col="red", lwd=2,
         xlab="Iterations", ylab="Best Tour Length",
         main="TSP Grid: Algorithm Comparison",
         ylim=range(c(costs_SA, costs_2opt, costs_GA, costs_ACO)))
    
    lines(costs_2opt, col="purple", lwd=2)
    lines(costs_GA, col="blue", lwd=2)
    lines(costs_ACO, col="darkgreen", lwd=2)
    
    legend("topright",
           legend=c("Simulated Annealing",
                    "2-opt",
                    "Genetic Algorithm",
                    "Ant Colony Optimization"),
                    col=c("red","purple","blue","darkgreen"),
                    lwd=2)
    
    

    
    set.seed(1)
    
    n <- 40
  theta <- seq(0, 2*pi, length.out = n+1)[-1]
  
  cities <- cbind(cos(theta), sin(theta))
    iters <- 2000
  
  costs_SA   <- SA_cpp(cities, iters)
    costs_2opt <- two_opt_cpp(cities, iters)
    costs_GA   <- GA_grid_cpp(cities, iters, 50)
    costs_ACO  <- ACO_cpp(cities, iters, 30)
    
    costs_SA   <- cummin(costs_SA)
    costs_2opt <- cummin(costs_2opt)
    costs_GA   <- cummin(costs_GA)
    costs_ACO  <- cummin(costs_ACO)
    
    plot(costs_SA, type="l", col="red", lwd=2,
         xlab="Iterations", ylab="Tour Length",
         main="Cerný Circle: Algorithm Comparison",
         ylim=range(c(costs_SA, costs_2opt, costs_GA, costs_ACO)))
    
    lines(costs_2opt, col="purple", lwd=2)
    lines(costs_GA, col="blue", lwd=2)
    lines(costs_ACO, col="darkgreen", lwd=2)
    
    legend("topright",
           legend=c("Simulated Annealing",
                    "2-opt",
                    "Genetic Algorithm",
                    "Ant Colony Optimization"),
                    col=c("red","purple","blue","darkgreen"),
                    lwd=2)

