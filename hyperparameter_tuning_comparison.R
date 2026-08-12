# ============================================================
# HYPERPARAMETER TUNING COMPARISON
# Simulated Annealing vs Random Search vs Optuna (TPE)
# ============================================================

# ============================================================
# INSTALL REQUIRED LIBRARIES
# ============================================================

# pip install optuna

# ============================================================
# IMPORTS
# ============================================================

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import optuna
import time

from sklearn.datasets import load_breast_cancer

from sklearn.model_selection import (
  cross_val_score,
  StratifiedKFold
)

from sklearn.ensemble import (
  RandomForestClassifier,
  GradientBoostingClassifier
)

from sklearn.svm import SVC

from scipy.stats import (
  kruskal,
  wilcoxon
)

# ============================================================
# DATASET
# ============================================================

data = load_breast_cancer()

X = data.data
y = data.target

cv = StratifiedKFold(
  n_splits=5,
  shuffle=True,
  random_state=42
)

# ============================================================
# OBJECTIVE FUNCTIONS
# ============================================================

def evaluate_rf(params):
  
  model = RandomForestClassifier(
    n_estimators=params["n_estimators"],
    max_depth=params["max_depth"],
    min_samples_split=params["min_samples_split"],
    random_state=42
  )

score = cross_val_score(
  model,
  X,
  y,
  cv=cv,
  scoring="accuracy"
).mean()

return score


def evaluate_svm(params):
  
  model = SVC(
    C=params["C"],
    gamma=params["gamma"]
  )

score = cross_val_score(
  model,
  X,
  y,
  cv=cv,
  scoring="accuracy"
).mean()

return score


def evaluate_gb(params):
  
  model = GradientBoostingClassifier(
    n_estimators=params["n_estimators"],
    learning_rate=params["learning_rate"],
    max_depth=params["max_depth"],
    random_state=42
  )

score = cross_val_score(
  model,
  X,
  y,
  cv=cv,
  scoring="accuracy"
).mean()

return score

# ============================================================
# RANDOM SEARCH
# ============================================================

def random_search(
  objective,
  n_iter=50
):
  
  best_scores = []

best = -np.inf

start_time = time.time()

for i in range(n_iter):
  
  params = {
    "n_estimators":
      np.random.randint(10,301),
    
    "max_depth":
      np.random.randint(1,21),
    
    "min_samples_split":
      np.random.randint(2,11)
  }

score = objective(params)

best = max(best, score)

best_scores.append(best)

runtime = time.time() - start_time

return best_scores, runtime

# ============================================================
# SIMULATED ANNEALING
# ============================================================

def simulated_annealing(
  objective,
  n_iter=50
):
  
  current = {
    "n_estimators":100,
    "max_depth":10,
    "min_samples_split":2
  }

current_score = objective(current)

best = current_score

best_scores = []

T = 1.0

cooling = 0.95

start_time = time.time()

for i in range(n_iter):
  
  proposal = {
    
    "n_estimators":
      int(np.clip(
        current["n_estimators"]
        +
          np.random.randint(-20,21),
        10,
        300
      )),
    
    "max_depth":
      int(np.clip(
        current["max_depth"]
        +
          np.random.randint(-3,4),
        1,
        20
      )),
    
    "min_samples_split":
      int(np.clip(
        current["min_samples_split"]
        +
          np.random.randint(-2,3),
        2,
        10
      ))
  }

proposal_score = objective(proposal)

delta = proposal_score - current_score

if delta > 0 or np.random.rand() < np.exp(delta/T):
  
  current = proposal
current_score = proposal_score

best = max(best, current_score)

best_scores.append(best)

T *= cooling

runtime = time.time() - start_time

return best_scores, runtime

# ============================================================
# OPTUNA (TPE)
# ============================================================

def optuna_search(
  objective,
  n_iter=50
):
  
  scores = []

start_time = time.time()

def objective_optuna(trial):
  
  params = {
    
    "n_estimators":
      trial.suggest_int(
        "n_estimators",
        10,
        300
      ),
    
    "max_depth":
      trial.suggest_int(
        "max_depth",
        1,
        20
      ),
    
    "min_samples_split":
      trial.suggest_int(
        "min_samples_split",
        2,
        10
      )
  }

score = objective(params)

scores.append(score)

return score

study = optuna.create_study(
  direction="maximize"
)

study.optimize(
  objective_optuna,
  n_trials=n_iter
)

best_scores = np.maximum.accumulate(scores)

runtime = time.time() - start_time

return best_scores, runtime

# ============================================================
# MULTIPLE RUNS
# ============================================================

runs = 20

iterations = 50

sa_final = []
rs_final = []
optuna_final = []

sa_runtime = []
rs_runtime = []
optuna_runtime = []

sa_curves = []
rs_curves = []
optuna_curves = []

for r in range(runs):
  
  print("Run:", r+1)

sa_scores, sa_time = simulated_annealing(
  evaluate_rf,
  iterations
)

rs_scores, rs_time = random_search(
  evaluate_rf,
  iterations
)

opt_scores, opt_time = optuna_search(
  evaluate_rf,
  iterations
)

sa_curves.append(sa_scores)
rs_curves.append(rs_scores)
optuna_curves.append(opt_scores)

sa_final.append(sa_scores[-1])
rs_final.append(rs_scores[-1])
optuna_final.append(opt_scores[-1])

sa_runtime.append(sa_time)
rs_runtime.append(rs_time)
optuna_runtime.append(opt_time)

# ============================================================
# MEAN CONVERGENCE CURVES
# ============================================================

sa_mean = np.mean(sa_curves, axis=0)
rs_mean = np.mean(rs_curves, axis=0)
opt_mean = np.mean(optuna_curves, axis=0)

# ============================================================
# CONVERGENCE PLOT
# ============================================================

plt.figure(figsize=(10,6))

plt.plot(
  sa_mean,
  label="Simulated Annealing",
  linewidth=3
)

plt.plot(
  rs_mean,
  label="Random Search",
  linewidth=3
)

plt.plot(
  opt_mean,
  label="Optuna (TPE)",
  linewidth=3
)

plt.xlabel("Iteration")
plt.ylabel("Best Accuracy")
plt.title("Convergence Comparison")

plt.legend()

plt.grid(True)

plt.savefig(
  "convergence_model.png",
  dpi=300,
  bbox_inches="tight"
)

plt.show()

# ============================================================
# BOXPLOT
# ============================================================

plt.figure(figsize=(8,6))

plt.boxplot([
  sa_final,
  rs_final,
  optuna_final
])

plt.xticks(
  [1,2,3],
  [
    "SA",
    "Random Search",
    "Optuna"
  ]
)

plt.ylabel("Final Accuracy")

plt.title(
  "Distribution of Final Accuracy"
)

plt.grid(True)

plt.savefig(
  "boxplot_model.png",
  dpi=300,
  bbox_inches="tight"
)

plt.show()

# ============================================================
# RUNTIME COMPARISON
# ============================================================

runtime_df = pd.DataFrame({
  
  "Method":[
    "SA",
    "Random Search",
    "Optuna"
  ],
  
  "Mean Runtime":[
    np.mean(sa_runtime),
    np.mean(rs_runtime),
    np.mean(optuna_runtime)
  ]
})

print(runtime_df)

# ============================================================
# STATISTICAL TESTS
# ============================================================

# Kruskal-Wallis

H, p = kruskal(
  sa_final,
  rs_final,
  optuna_final
)

print("\nKruskal-Wallis Test")
print("H statistic:", H)
print("p-value:", p)

# Pairwise Wilcoxon

print("\nWilcoxon Tests")

print(
  "SA vs RS:",
  wilcoxon(sa_final, rs_final).pvalue
)

print(
  "SA vs Optuna:",
  wilcoxon(sa_final, optuna_final).pvalue
)

print(
  "Optuna vs RS:",
  wilcoxon(optuna_final, rs_final).pvalue
)

# ============================================================
# SUMMARY TABLE
# ============================================================

summary = pd.DataFrame({
  
  "Method":[
    "Simulated Annealing",
    "Random Search",
    "Optuna"
  ],
  
  "Mean Accuracy":[
    np.mean(sa_final),
    np.mean(rs_final),
    np.mean(optuna_final)
  ],
  
  "Std Dev":[
    np.std(sa_final),
    np.std(rs_final),
    np.std(optuna_final)
  ],
  
  "Mean Runtime":[
    np.mean(sa_runtime),
    np.mean(rs_runtime),
    np.mean(optuna_runtime)
  ]
})

print("\nSummary Table")
print(summary)

summary.to_csv(
  "summary_results.csv",
  index=False
)
