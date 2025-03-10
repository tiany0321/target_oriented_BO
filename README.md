# Target-oriented Bayesian optimization strategy 
A target-oriented Bayesian optimization strategy, t-EGO, is demonstrated for designing materials with target-specific properties.

The accompanying documentation includes codes for implementing t-EGO, along with two other Bayesian optimization strategies.

## 1. Iterative Optimization Process of Different Acquisition Functions in a 1-Dimensional Mathematical Function Case

###  1.1 1-Dimensional Mathematical Function
>>#### $y=\sin(4 \cdot (x - 1.3)^4) \cdot \cos((x - 1.4)) + \frac{(x - 0.9)}{2}$

### 1.2 Acquisition Functions
>>#### 1.2.1 Acquisition Functions for identifying the option with y=t
>>>>##### (1) Target-oriented EGO (t-EGO)
>>>>##### (2) Constrained EGO (CEGO)
>>>>##### (3) Pure exploitation (PureExp)

>>#### 1.2.2 Acquisition Functions for Minimizing |y - t|
>>>>##### (1) Efficient Global Optimization EGO
>>>>##### (2) Multi-objective acquisition Functions (MOAF)

### 1.3 Graphical Illustrations Depicting the Evolution of t-EI and EI as the Function is Queried



