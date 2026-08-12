library(Rcpp)
  
  cppFunction('
                NumericVector GA_grid_cpp(NumericMatrix cities, int iters, int pop_size){
                  
                  int n = cities.nrow();
                  
                  // distance function inline
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
                  
                  // initialize population
                  List population(pop_size);
                  for(int i=0; i<pop_size; i++){
                    population[i] = Rcpp::sample(n, n, false);
                  }
                  
                  NumericVector best_costs(iters);
                  
                  for(int iter=0; iter<iters; iter++){
                    
                    // compute fitness
                    NumericVector fitness(pop_size);
                    for(int i=0; i<pop_size; i++){
                      IntegerVector route = population[i];
                      fitness[i] = route_dist(route);
                    }
                    
                    // sort population by fitness
                    IntegerVector idx = seq(0, pop_size-1);
                    std::sort(idx.begin(), idx.end(), [&](int a, int b){
                      return fitness[a] < fitness[b];
                    });
                    
                    // keep best half
                    int half = pop_size/2;
                    List new_pop(pop_size);
                    
                    for(int i=0; i<half; i++){
                      new_pop[i] = population[idx[i]];
                    }
                    
                    // crossover + mutation
                    for(int i=half; i<pop_size; i++){
                      
                      // parents
                      IntegerVector p1 = new_pop[floor(R::runif(0,half))];
                      IntegerVector p2 = new_pop[floor(R::runif(0,half))];
                      
                      IntegerVector child(n);
                      
                      // crossover (partial copy from p1)
                      int cut1 = floor(R::runif(0,n));
                      int cut2 = floor(R::runif(0,n));
                      if(cut1 > cut2){
                        int temp = cut1;
                        cut1 = cut2;
                        cut2 = temp;
                      }
                      
                      LogicalVector used(n+1, false);
                      
                      // copy segment from p1
                      for(int j=cut1; j<=cut2; j++){
                        child[j] = p1[j];
                        used[p1[j]] = true;
                      }
                      
                      // fill remaining from p2
                      int pos = 0;
                      for(int j=0; j<n; j++){
                        if(!used[p2[j]]){
                          
                          // find next empty spot
                          while(pos>=cut1 && pos<=cut2){
                            pos++;
                          }
                          
                          if(pos < n){
                            child[pos] = p2[j];
                            pos++;
                          }
                        }
                      }
                      
                      // mutation (swap)
                      if(R::runif(0,1) < 0.3){
                        int a = floor(R::runif(0,n));
                        int b = floor(R::runif(0,n));
                        
                        int temp = child[a];
                        child[a] = child[b];
                        child[b] = temp;
                      }
                      
                      new_pop[i] = child;
                    }
                    
                    population = new_pop;
                    
                    // store best cost
                    best_costs[iter] = fitness[idx[0]];
                  }
                  
                  return best_costs;
                }
                ')
  
  grid_size <- 100

x <- rep(1:grid_size, each=grid_size)
  y <- rep(1:grid_size, grid_size)
  
  cities <- cbind(x,y)
  
  set.seed(1)
  
  costs <- GA_grid_cpp(cities, iters=2000, pop_size=50)
  plot(costs, type="l", col="red",
       xlab="Iterations", ylab="Cost",
       main="Genetic Algorithm (Grid TSP)")