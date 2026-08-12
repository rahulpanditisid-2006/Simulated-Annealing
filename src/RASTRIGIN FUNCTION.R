# ============================================================
# RASTRIGIN FUNCTION ANALYSIS
# ============================================================

rm(list = ls())

library(plotly)

# ============================================================
# RASTRIGIN FUNCTION
# ============================================================

rastrigin <- function(x){
  
  20 +
    x[1]^2 +
    x[2]^2 -
    10*cos(2*pi*x[1]) -
    10*cos(2*pi*x[2])
}

# ============================================================
# GRID FOR VISUALIZATION
# ============================================================

x_vals <- seq(-5.12,5.12,length.out=250)

y_vals <- seq(-5.12,5.12,length.out=250)

z_matrix <- outer(
  x_vals,
  y_vals,
  Vectorize(function(x,y)
    rastrigin(c(x,y)))
)

# ============================================================
# 1. 3D SURFACE PLOT
# ============================================================

fig <- plot_ly(
  x=x_vals,
  y=y_vals,
  z=z_matrix
) %>%
  add_surface()

fig

# ============================================================
# 2. CONTOUR PLOT
# ============================================================

filled.contour(
  x_vals,
  y_vals,
  z_matrix,
  xlab="x",
  ylab="y",
  main="Rastrigin Function Contour"
)

# ============================================================
# PARAMETERS
# ============================================================

iterations <- 5000

start <- c(4,4)

# ============================================================
# 3. SIMULATED ANNEALING
# ============================================================

set.seed(123)

x_sa <- start

T <- 10

cooling <- 0.995

sa_values <- numeric(iterations)

trajectory_sa <- matrix(
  0,
  nrow=iterations,
  ncol=2
)

accepted <- numeric(iterations)

temperatures <- numeric(iterations)

for(i in 1:iterations){
  
  current <- rastrigin(x_sa)
  
  proposal <- x_sa + rnorm(2,sd=0.5)
  
  proposal_value <- rastrigin(proposal)
  
  delta <- proposal_value-current
  
  if(delta < 0 ||
     runif(1) < exp(-delta/T)){
    
    x_sa <- proposal
    
    accepted[i] <- 1
  }
  
  trajectory_sa[i,] <- x_sa
  
  sa_values[i] <- rastrigin(x_sa)
  
  temperatures[i] <- T
  
  T <- T*cooling
}

# ============================================================
# 4. GRADIENT DESCENT
# ============================================================

grad_rastrigin <- function(x){
  
  dx <- 2*x[1] +
    20*pi*sin(2*pi*x[1])
  
  dy <- 2*x[2] +
    20*pi*sin(2*pi*x[2])
  
  c(dx,dy)
}

x_gd <- start

learning_rate <- 0.001

gd_values <- numeric(iterations)

for(i in 1:iterations){
  
  gradient <- grad_rastrigin(x_gd)
  
  x_gd <- x_gd -
    learning_rate*gradient
  
  gd_values[i] <- rastrigin(x_gd)
}

# ============================================================
# 5. HILL CLIMBING
# ============================================================

x_hc <- start

hc_values <- numeric(iterations)

for(i in 1:iterations){
  
  current <- rastrigin(x_hc)
  
  proposal <- x_hc + rnorm(2,sd=0.2)
  
  proposal_value <- rastrigin(proposal)
  
  if(proposal_value < current){
    
    x_hc <- proposal
  }
  
  hc_values[i] <- rastrigin(x_hc)
}

# ============================================================
# 6. TRAJECTORY PLOT
# ============================================================

filled.contour(
  x_vals,
  y_vals,
  z_matrix,
  xlab="x",
  ylab="y",
  main="SA Trajectory on Rastrigin Function",
  plot.axes={
    
    axis(1)
    axis(2)
    
    lines(
      trajectory_sa[,1],
      trajectory_sa[,2],
      col="red",
      lwd=2
    )
    
    points(
      start[1],
      start[2],
      pch=19,
      col="blue"
    )
    
    points(
      0,0,
      pch=19,
      col="darkgreen"
    )
  }
)

# ============================================================
# 7. CONVERGENCE COMPARISON
# ============================================================

sa_plot <- log10(sa_values+1e-8)

gd_plot <- log10(gd_values+1e-8)

hc_plot <- log10(hc_values+1e-8)

plot(
  sa_plot,
  type="l",
  col="red",
  lwd=3,
  ylim=range(
    c(sa_plot,gd_plot,hc_plot)
  ),
  xlab="Iteration",
  ylab="log10(Objective Value)",
  main="Algorithm Comparison on Rastrigin Function",
  cex.main=1.5,
  cex.lab=1.3
)

lines(gd_plot,col="blue",lwd=3)

lines(hc_plot,col="darkgreen",lwd=3)

grid()

legend(
  "topright",
  legend=c(
    "Simulated Annealing",
    "Gradient Descent",
    "Hill Climbing"
  ),
  col=c(
    "red",
    "blue",
    "darkgreen"
  ),
  lwd=3,
  bg="white"
)

# ============================================================
# 8. ACCEPTANCE RATIO
# ============================================================

window_size <- 100

acceptance_ratio <- numeric(
  iterations-window_size
)

for(i in 1:(iterations-window_size)){
  
  acceptance_ratio[i] <- mean(
    accepted[i:(i+window_size)]
  )
}

plot(
  acceptance_ratio,
  type="l",
  lwd=3,
  xlab="Iteration",
  ylab="Acceptance Ratio",
  main="Acceptance Ratio During Optimization"
)

# ============================================================
# 9. TEMPERATURE DECAY
# ============================================================

plot(
  temperatures,
  type="l",
  lwd=3,
  xlab="Iteration",
  ylab="Temperature",
  main="Temperature Decay"
)

# ============================================================
# 10. MULTIPLE RUNS
# ============================================================

runs <- 20

final_values <- numeric(runs)

for(r in 1:runs){
  
  x <- c(runif(1,-5,5),
         runif(1,-5,5))
  
  T <- 10
  
  for(i in 1:3000){
    
    current <- rastrigin(x)
    
    proposal <- x +
      rnorm(2,sd=0.25)
    
    proposal_value <- rastrigin(proposal)
    
    delta <- proposal_value-current
    
    if(delta < 0 ||
       runif(1) < exp(-delta/T)){
      
      x <- proposal
    }
    
    T <- T*0.995
  }
  
  final_values[r] <- rastrigin(x)
}

boxplot(
  final_values,
  main="Distribution of Final Objective Values",
  ylab="Final Objective Value"
)

# ============================================================
# FINAL RESULTS
# ============================================================

cat("\n====================================\n")

cat("FINAL OBJECTIVE VALUES\n")

cat("====================================\n")

cat("\nSimulated Annealing:\n")
print(tail(sa_values,1))

cat("\nGradient Descent:\n")
print(tail(gd_values,1))

cat("\nHill Climbing:\n")
print(tail(hc_values,1))