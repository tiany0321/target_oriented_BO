# Target-oriented Bayesian optimization strategy 
A target-oriented Bayesian optimization strategy, t-EGO, is demonstrated for designing materials with target-specific properties.

The accompanying documentation includes codes for implementing t-EGO, along with several other Bayesian optimization strategies.

To test t-EGO, evaluations were conducted on some databases. The 1D/specific folder contains annotations for the program.

## 1. Iterative Optimization Process of Different BO Methods in a 1-Dimensional Mathematical Function Case

###  1.1 1-Dimensional Mathematical Function

### 1.2 BO Methods
>>#### 1.2.1 BO Methods for identifying the option with y=t
>>>>##### (1) Target-oriented EGO (t-EGO)
>>>>##### (2) Constrained EGO (CEGO)
>>>>##### (3) Pure exploitation (PureExp)

>>#### 1.2.2 BO Methods for Minimizing |y - t|
>>>>##### (1) Efficient Global Optimization (EGO)
>>>>##### (2) Multi-objective acquisition Functions (MOAF)

### 1.3 Graphical Illustrations Depicting the Evolution of t-EI and EI as the Function is Queried

## 2. Test on STYTANG function(6D)
### 2.1 BO Methods
>>#### 2.1.1 BO Methods for identifying the option with y=t
>>>>##### (1) Target-oriented EGO (t-EGO)
>>>>##### (2) Constrained EGO (CEGO)
>>>>##### (3) Pure exploitation (PureExp)
>>>>##### (4) Thompson sampling (TS)

>>#### 2.1.2 BO Methods for Minimizing |y - t|
>>>>##### (1) Efficient Global Optimization EGO
>>>>##### (2) Lower confidence boundary (LCB)
>>>>##### (3) Probability Improvement (PI)
>>>>##### (4) Multi-objective acquisition Functions (MOAF)
>>>>##### (5) Expected Quantile Improvement (EQI)
## 3. Test on Active hydrogen evolution reaction catalyst ($MA_2Z_4$)

## 4. Search for SMAs with target specific property

# If you use this code please cite our paper:

## Tian, Y., Li, T., Pang, J. et al. Materials design with target-oriented Bayesian optimization. npj Comput Mater 11, 209 (2025). https://doi.org/10.1038/s41524-025-01704-4
