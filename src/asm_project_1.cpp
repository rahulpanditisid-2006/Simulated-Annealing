library(Rcpp)
  
  cppFunction('
                List optimize_all_cpp(int iters,
                                      double xmin,
                                      double xmax,
                                      int pop_size,
                                      int n_ants){
                  
                  // objective
                  auto f = [&](double x){
                    return x*x + 10.0 * sin(5.0*x);
                  };
                  
                  NumericVector SA_cost(iters);
                  NumericVector LS_cost(iters);
                  NumericVector GA_cost(iters);
                  NumericVector ACO_cost(iters);
                  
                  // =========================
                  // 1) Simulated Annealing
                  // =========================
                  {
                    double x = xmin + (xmax-xmin)*R::runif(0,1);
                    double T = 2.0;
                    double cooling = 0.995;
                    double step0 = 0.5*(xmax-xmin);
                    
                    for(int i=0;i<iters;i++){
                      double step = step0 * (T/2.0);
                      double xn = x + R::runif(-step, step);
                      if(xn < xmin) xn = xmin;
                      if(xn > xmax) xn = xmax;
                      
                      double d = f(xn) - f(x);
                      if(d < 0 || R::runif(0,1) < exp(-d/T)){
                        x = xn;
                      }
                      
                      SA_cost[i] = f(x);
                      T *= cooling;
                    }
                  }
                  
                  // =========================
                  // 2) Local Search (2-opt analogue)
                  // =========================
                  {
                    double x = xmin + (xmax-xmin)*R::runif(0,1);
                    double step = 0.1*(xmax-xmin);
                    
                    for(int i=0;i<iters;i++){
                      double xn = x + R::runif(-step, step);
                      if(xn < xmin) xn = xmin;
                      if(xn > xmax) xn = xmax;
                      
                      if(f(xn) < f(x)){
                        x = xn;
                      }
                      
                      LS_cost[i] = f(x);
                    }
                  }
                  
                  // =========================
                  // 3) Genetic Algorithm
                  // =========================
                  {
                    NumericVector pop(pop_size);
                    for(int i=0;i<pop_size;i++){
                      pop[i] = xmin + (xmax-xmin)*R::runif(0,1);
                    }
                    
                    for(int iter=0; iter<iters; iter++){
                      
                      NumericVector fitness(pop_size);
                      for(int i=0;i<pop_size;i++){
                        fitness[i] = f(pop[i]);
                      }
                      
                      IntegerVector idx = seq(0,pop_size-1);
                      std::sort(idx.begin(), idx.end(),
                                [&](int a,int b){ return fitness[a] < fitness[b]; });
                      
                      int half = pop_size/2;
                      NumericVector new_pop(pop_size);
                      
                      for(int i=0;i<half;i++){
                        new_pop[i] = pop[idx[i]];
                      }
                      
                      for(int i=half;i<pop_size;i++){
                        double p1 = new_pop[(int)floor(R::runif(0,half))];
                        double p2 = new_pop[(int)floor(R::runif(0,half))];
                        
                        double child = 0.5*(p1 + p2);
                        child += R::rnorm(0, 0.1*(xmax-xmin)); // mutation
                        
                        if(child < xmin) child = xmin;
                        if(child > xmax) child = xmax;
                        
                        new_pop[i] = child;
                      }
                      
                      pop = new_pop;
                      
                      // best
                      double best = f(pop[0]);
                      for(int i=1;i<pop_size;i++){
                        double val = f(pop[i]);
                        if(val < best) best = val;
                      }
                      
                      GA_cost[iter] = best;
                    }
                  }
                  
                  // =========================
                  // 4) Ant Colony Optimization (continuous bins)
                  // =========================
                  {
                    int bins = 50;
                    NumericVector tau(bins, 1.0);
                    double alpha = 1.0, beta = 2.0, rho = 0.5;
                    
                    double best = 1e9;
                    
                    for(int iter=0; iter<iters; iter++){
                      
                      for(int k=0;k<n_ants;k++){
                        
                        NumericVector prob(bins);
                        double sum = 0;
                        
                        for(int i=0;i<bins;i++){
                          double x = xmin + (xmax-xmin)*(i/(double)bins);
                          double val = pow(tau[i],alpha) * pow(1.0/(1.0+fabs(f(x))), beta);
                          prob[i] = val;
                          sum += val;
                        }
                        
                        double r = R::runif(0,sum);
                        double cum = 0;
                        int chosen = 0;
                        
                        for(int i=0;i<bins;i++){
                          cum += prob[i];
                          if(cum >= r){
                            chosen = i;
                            break;
                          }
                        }
                        
                        double x = xmin + (xmax-xmin)*(chosen/(double)bins);
                        double val = f(x);
                        
                        if(val < best) best = val;
                        
                        tau[chosen] += 1.0/(1.0+val);
                      }
                      
                      // evaporation
                      for(int i=0;i<bins;i++){
                        tau[i] *= (1.0 - rho);
                      }
                      
                      ACO_cost[iter] = best;
                    }
                  }
                  
                  return List::create(
                    Named("SA") = SA_cost,
                    Named("LS") = LS_cost,
                    Named("GA") = GA_cost,
                    Named("ACO") = ACO_cost
                  );
                }
                ')
  
  res <- optimize_all_cpp(
      iters = 2000,
      xmin  = -5,
      xmax  =  5,
      pop_size = 50,
      n_ants   = 30
  )
  
  sa  <- cummin(res$SA)
  ls  <- cummin(res$LS)
  ga  <- cummin(res$GA)
  aco <- cummin(res$ACO)
  
  plot(sa, type="l", col="red", lwd=2,
       ylim=range(c(sa, ls, ga, aco)),
       xlab="Iterations", ylab="f(x)",
       main="Optimization Comparison")
  
  lines(ls,  col="purple", lwd=2)
  lines(ga,  col="blue", lwd=2)
  lines(aco, col="darkgreen", lwd=2)
  
  legend("topright",
         legend=c("SA","Local Search","GA","ACO"),
         col=c("red","purple","blue","darkgreen"),
         lwd=2)
