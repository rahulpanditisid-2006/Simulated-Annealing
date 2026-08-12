# ============================================================
# METROPOLIS vs SIMULATED ANNEALING
# Visual distinction between the two algorithms
# ============================================================

import numpy as np
import matplotlib.pyplot as plt

# ============================================================
# MULTI-MODAL FUNCTION
# ============================================================

def f(x):
  return x**2 + 10*np.sin(5*x)

# ============================================================
# DOMAIN
# ============================================================

X = np.linspace(-6, 6, 1000)
Y = f(X)

# ============================================================
# INITIAL POINT
# ============================================================

np.random.seed(1)

x_metro = 5.0
x_sa = 5.0

# temperature only for SA
T = 5
cooling = 0.995

# paths
path_metro = [x_metro]
path_sa = [x_sa]

# ============================================================
# METROPOLIS ALGORITHM
# (constant temperature)
# ============================================================

for i in range(300):
  
  new = x_metro + np.random.normal(0, 0.5)

delta = f(new) - f(x_metro)

# constant temperature
T_metro = 1

if delta < 0 or np.random.rand() < np.exp(-delta / T_metro):
  x_metro = new

path_metro.append(x_metro)

# ============================================================
# SIMULATED ANNEALING
# (cooling schedule)
# ============================================================

for i in range(300):
  
  new = x_sa + np.random.normal(0, 0.5)

delta = f(new) - f(x_sa)

if delta < 0 or np.random.rand() < np.exp(-delta / T):
  x_sa = new

path_sa.append(x_sa)

# cooling
T *= cooling

# ============================================================
# PLOT
# ============================================================

fig, ax = plt.subplots(1,2, figsize=(14,5))

# ------------------------------------------------------------
# METROPOLIS
# ------------------------------------------------------------

ax[0].plot(X, Y)

ax[0].scatter(
  path_metro,
  [f(x) for x in path_metro],
  s=15
)

ax[0].set_title("Metropolis Algorithm")

ax[0].set_xlabel("x")
ax[0].set_ylabel("f(x)")

# ------------------------------------------------------------
# SIMULATED ANNEALING
# ------------------------------------------------------------

ax[1].plot(X, Y)

ax[1].scatter(
  path_sa,
  [f(x) for x in path_sa],
  s=15
)

ax[1].set_title("Simulated Annealing")

ax[1].set_xlabel("x")
ax[1].set_ylabel("f(x)")

plt.tight_layout()
plt.show()
