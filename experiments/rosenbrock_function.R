# ============================================================
# ROSENBROCK FUNCTION : COMPLETE ANALYSIS CODE
# Generates:
# 1. 3D Surface Plot
# 2. Contour Plot
# 3. SA Optimization Trajectory
# 4. Convergence Curve
# 5. Temperature Decay Plot
# 6. Acceptance Ratio Plot
# 7. Multiple Run Statistics
# ============================================================

rm(list = ls())

library(plotly)

# ============================================================
# ROSENBROCK FUNCTION
# ============================================================

rosenbrock <- function(x, y){
  (1 - x)^2 + 100*(y - x^2)^2
}

# ============================================================
# GRID FOR VISUALIZATION
# ============================================================

x_vals <- seq(-2, 2, length.out = 200)
y_vals <- seq(-1, 3, length.out = 200)

z_matrix <- outer(
  x_vals,
  y_vals,
  Vectorize(rosenbrock)
)

# ============================================================
# 1. 3D SURFACE PLOT
# ============================================================

fig1 <- plot_ly(
  x = x_vals,
  y = y_vals,
  z = z_matrix
) %>%
  add_surface()

fig1

# saveWidget(fig1, "rosenbrock_surface.html")

# ============================================================
# 2. CONTOUR PLOT
# ============================================================

png("rosenbrock_contour.png", width = 900, height = 700)

contour(
  x_vals,
  y_vals,
  z_matrix,
  nlevels = 40,
  xlab = "x",
  ylab = "y",
  main = "Rosenbrock Function Contour"
)

points(1,1,col="red",pch=19,cex=1.5)

dev.off()

# ============================================================
# 3. SIMULATED ANNEALING IMPLEMENTATION
# ============================================================

set.seed(123)

iterations <- 5000

x <- c(-1.5, 2)

T <- 5
cooling <- 0.995

trajectory <- matrix(0, nrow=iterations, ncol=2)

values <- numeric(iterations)

temperatures <- numeric(iterations)

accepted <- numeric(iterations)

for(i in 1:iterations){
  
  current_value <- rosenbrock(x[1], x[2])
  
  proposal <- x + rnorm(2, sd = 0.15)
  
  proposal_value <- rosenbrock(proposal[1], proposal[2])
  
  delta <- proposal_value - current_value
  
  if(delta < 0 || runif(1) < exp(-delta/T)){
    
    x <- proposal
    accepted[i] <- 1
    
  }
  
  trajectory[i,] <- x
  
  values[i] <- rosenbrock(x[1], x[2])
  
  temperatures[i] <- T
  
  T <- T * cooling
}

# ============================================================
# 4. TRAJECTORY ON CONTOUR PLOT
# ============================================================

png("rosenbrock_trajectory.png", width = 900, height = 700)

contour(
  x_vals,
  y_vals,
  z_matrix,
  nlevels = 40,
  xlab = "x",
  ylab = "y",
  main = "SA Optimization Trajectory"
)

lines(
  trajectory[,1],
  trajectory[,2],
  col = "red",
  lwd = 2
)

points(
  trajectory[1,1],
  trajectory[1,2],
  col="blue",
  pch=19,
  cex=1.5
)

points(
  1,1,
  col="darkgreen",
  pch=19,
  cex=1.5
)

legend(
  "topright",
  legend=c("Trajectory","Start","Global Minimum"),
  col=c("red","blue","darkgreen"),
  lty=c(1,NA,NA),
  pch=c(NA,19,19)
)

dev.off()

# ============================================================
# 5. CONVERGENCE CURVE
# ============================================================

png("rosenbrock_convergence.png", width = 900, height = 700)

plot(
  values,
  type="l",
  lwd=2,
  xlab="Iteration",
  ylab="Objective Value",
  main="Convergence Curve of Simulated Annealing"
)

dev.off()

# ============================================================
# 6. TEMPERATURE DECAY PLOT
# ============================================================

png("temperature_decay.png", width = 900, height = 700)

plot(
  temperatures,
  type="l",
  lwd=2,
  xlab="Iteration",
  ylab="Temperature",
  main="Temperature Decay"
)

dev.off()

# ============================================================
# 7. ACCEPTANCE RATIO ANALYSIS
# ============================================================

window_size <- 100

acceptance_ratio <- numeric(iterations-window_size)

for(i in 1:(iterations-window_size)){
  
  acceptance_ratio[i] <- mean(
    accepted[i:(i+window_size)]
  )
}

png("acceptance_ratio.png", width = 900, height = 700)

plot(
  acceptance_ratio,
  type="l",
  lwd=2,
  xlab="Iteration",
  ylab="Acceptance Ratio",
  main="Acceptance Ratio During Optimization"
)

dev.off()

# ============================================================
# 8. MULTIPLE RUN STATISTICS
# ============================================================

runs <- 20

final_values <- numeric(runs)

for(r in 1:runs){
  
  x <- c(runif(1,-2,2), runif(1,-1,3))
  
  T <- 5
  
  for(i in 1:3000){
    
    current_value <- rosenbrock(x[1],x[2])
    
    proposal <- x + rnorm(2, sd=0.15)
    
    proposal_value <- rosenbrock(
      proposal[1],
      proposal[2]
    )
    
    delta <- proposal_value-current_value
    
    if(delta < 0 || runif(1) < exp(-delta/T)){
      
      x <- proposal
    }
    
    T <- T*0.995
  }
  
  final_values[r] <- rosenbrock(x[1],x[2])
}

# ============================================================
# 9. BOXPLOT OF MULTIPLE RUNS
# ============================================================

png("multiple_runs_boxplot.png", width = 900, height = 700)

boxplot(
  final_values,
  main="Distribution of Final Objective Values",
  ylab="Final Function Value"
)

dev.off()

# ============================================================
# 10. SUMMARY STATISTICS
# ============================================================

cat("====================================\n")
cat("MULTIPLE RUN STATISTICS\n")
cat("====================================\n")

cat("Mean Final Value:\n")
print(mean(final_values))

cat("\nStandard Deviation:\n")
print(sd(final_values))

cat("\nBest Run:\n")
print(min(final_values))

cat("\nWorst Run:\n")
print(max(final_values))




#-------------------------------------------------

# ============================================================
# COMPARISON OF:
# 1. Simulated Annealing (SA)
# 2. Gradient Descent (GD)
# 3. Hill Climbing (HC)
# ON ROSENBROCK FUNCTION
# ============================================================

rm(list = ls())

# ============================================================
# ROSENBROCK FUNCTION
# ============================================================

rosenbrock <- function(x){
  
  (1 - x[1])^2 +
    100*(x[2] - x[1]^2)^2
}

# ============================================================
# ROSENBROCK GRADIENT
# ============================================================

grad_rosenbrock <- function(x){
  
  dx <- -2*(1 - x[1]) -
    400*x[1]*(x[2] - x[1]^2)
  
  dy <- 200*(x[2] - x[1]^2)
  
  c(dx,dy)
}

# ============================================================
# PARAMETERS
# ============================================================

iterations <- 5000

start <- c(-1.5,2)

# ============================================================
# 1. SIMULATED ANNEALING
# ============================================================

set.seed(123)

x_sa <- start

T <- 5

cooling <- 0.995

sa_values <- numeric(iterations)

for(i in 1:iterations){
  
  current <- rosenbrock(x_sa)
  
  proposal <- x_sa + rnorm(2, sd=0.15)
  
  proposal_value <- rosenbrock(proposal)
  
  delta <- proposal_value - current
  
  if(delta < 0 ||
     runif(1) < exp(-delta/T)){
    
    x_sa <- proposal
  }
  
  sa_values[i] <- rosenbrock(x_sa)
  
  T <- T*cooling
}

# ============================================================
# 2. GRADIENT DESCENT
# ============================================================

x_gd <- start

learning_rate <- 0.001

gd_values <- numeric(iterations)

for(i in 1:iterations){
  
  gradient <- grad_rosenbrock(x_gd)
  
  x_gd <- x_gd -
    learning_rate*gradient
  
  gd_values[i] <- rosenbrock(x_gd)
}

# ============================================================
# 3. HILL CLIMBING
# ============================================================

set.seed(123)

x_hc <- start

hc_values <- numeric(iterations)

for(i in 1:iterations){
  
  current <- rosenbrock(x_hc)
  
  proposal <- x_hc + rnorm(2, sd=0.05)
  
  proposal_value <- rosenbrock(proposal)
  
  if(proposal_value < current){
    
    x_hc <- proposal
  }
  
  hc_values[i] <- rosenbrock(x_hc)
}

# ============================================================
# LOG SCALE FOR BETTER VISIBILITY
# ============================================================

sa_plot <- log10(sa_values + 1e-8)

gd_plot <- log10(gd_values + 1e-8)

hc_plot <- log10(hc_values + 1e-8)

# ============================================================
# COMPARISON PLOT
# ============================================================

png(
  "algorithm_comparison_rosenbrock.png",
  width = 1000,
  height = 750
)

plot(
  sa_plot,
  type="l",
  col="red",
  lwd=2,
  ylim=range(
    c(sa_plot,gd_plot,hc_plot)
  ),
  xlab="Iteration",
  ylab="log10(Objective Value)",
  main="Algorithm Comparison on Rosenbrock Function"
)

lines(
  gd_plot,
  col="blue",
  lwd=2
)

lines(
  hc_plot,
  col="darkgreen",
  lwd=2
)

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
  lwd=2
)

dev.off()

# ============================================================
# DISPLAY PLOT
# ============================================================

plot(
  sa_plot,
  type="l",
  col="red",
  lwd=2,
  ylim=range(
    c(sa_plot,gd_plot,hc_plot)
  ),
  xlab="Iteration",
  ylab="log10(Objective Value)",
  main="Algorithm Comparison on Rosenbrock Function"
)

lines(
  gd_plot,
  col="blue",
  lwd=2
)

lines(
  hc_plot,
  col="darkgreen",
  lwd=2
)

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
  lwd=2
)

# ============================================================
# FINAL VALUES
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
