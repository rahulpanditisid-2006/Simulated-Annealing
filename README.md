# Simulated Annealing

This project is a study and implementation of the **Simulated Annealing (SA)** optimization algorithm. The main aim was to understand how SA works both mathematically and in practice, especially for optimization problems where local search methods can get stuck in local minima.

The project was done as part of an Advanced Statistical Methods course project at the **Indian Statistical Institute, Delhi**.

## What is Simulated Annealing?

Simulated Annealing is a probabilistic optimization method inspired by the annealing process in metallurgy. Instead of always accepting a better solution, it can also accept a worse solution with a probability that depends on the current temperature.

At high temperatures, the algorithm explores the search space more freely. As the temperature decreases, the algorithm becomes more selective and focuses on improving the current solution.

For an uphill move, the acceptance probability is

$$
P(\text{accept}) =
\exp\left(-\frac{\Delta f}{T}\right)
$$

where $\Delta f$ is the increase in the objective function and $T$ is the current temperature.

## What I studied

The project has both a theoretical and an experimental component.

### Theoretical Analysis

The theoretical part studies Simulated Annealing as a time-inhomogeneous Markov chain. It covers:

- State space and neighbourhood structure
- Metropolis acceptance rule
- Gibbs (Boltzmann) distribution
- Detailed balance and reversibility
- Convergence as temperature approaches zero
- Hajek's convergence theorem
- Energy barriers and escape probabilities
- Spectral gap and relaxation time
- Effect of cooling schedules on convergence

A major focus was understanding how the cooling schedule controls the balance between exploration and exploitation.

## Experiments

The behaviour of Simulated Annealing was studied on different types of optimization problems.

### 1. Rosenbrock Function

The Rosenbrock function was used as an example of a smooth but non-convex optimization problem.

Simulated Annealing was compared with Gradient Descent and Hill Climbing to study how different optimization methods behave on a narrow, curved optimization landscape.

### 2. Rastrigin Function

The Rastrigin function was used to study a highly multimodal optimization landscape with many local minima.

Simulated Annealing performed substantially better than the local optimization methods in this setting, as its probabilistic acceptance of worse solutions allowed it to escape local minima.

### 3. Metropolis Algorithm vs Simulated Annealing

The project also compares the Metropolis algorithm with Simulated Annealing.

Both methods use the same basic probabilistic acceptance mechanism. The main difference is that the Metropolis algorithm operates at a fixed temperature, while Simulated Annealing gradually decreases the temperature.

This allows Simulated Annealing to move from broad exploration in the early stages to more local refinement in the later stages.

### 4. Traveling Salesman Problem

Simulated Annealing was applied to the Traveling Salesman Problem using 2-opt neighbourhood moves.

It was compared with:

- 2-opt Local Search
- Genetic Algorithm
- Ant Colony Optimization

Simulated Annealing was able to improve initial tours and escape poor local solutions. However, specialized methods such as 2-opt and Ant Colony Optimization performed better on the tested instances.

### 5. Machine Learning Hyperparameter Tuning

Simulated Annealing was also used for tuning the hyperparameters of a Random Forest classifier on the Breast Cancer Wisconsin dataset.

It was compared with:

- Random Search
- Bayesian Optimization using Optuna

Simulated Annealing performed better than Random Search, while Bayesian Optimization achieved better sample efficiency and overall performance.

## Main Observations

The experiments showed that the performance of Simulated Annealing depends strongly on the structure of the optimization problem.

- Simulated Annealing is useful for highly multimodal problems where local methods can become trapped.
- Accepting worse solutions helps the algorithm escape local minima.
- The cooling schedule has a major effect on the final result.
- Fast cooling can lead to premature convergence.
- Slow cooling improves exploration but increases computational cost.
- Simulated Annealing is not always better than specialized optimization methods.
- Gradient-based methods can be more effective on smooth optimization problems.
- Specialized methods such as 2-opt and Ant Colony Optimization can perform better on TSP.
- Bayesian Optimization can be more efficient for machine learning hyperparameter tuning.

Overall, the project showed that there is no single optimization method that works best for every problem. Simulated Annealing is particularly useful when the search space is difficult, multimodal, or when gradient information is unavailable or unreliable.

## Repository Structure

```text
Simulated-Annealing/
│
├── README.md
│
├── src/
│   └── C++ implementations
│
├── experiments/
│   ├── rastrigin_function.R
│   ├── metropolis_vs_simulated_annealing.R
│   └── hyperparameter_tuning_comparison.R
│
├── results/
│   └── figures and experiment outputs
│
└── report/
    └── simulated_annealing_report.pdf

```

## Authors 

- Rahul Pandit
- Kanishk Bhuradiya
- Bhagavath Chukka
- Harsh Maurya
