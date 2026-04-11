# agec840tariffmodel

An R package implementing the AGEC 840 partial equilibrium tariff model used in Lecture 16. The package provides functions for calibrating benchmark data, solving counterfactual equilibria, and decomposing welfare effects for tariffs, subsidies, and import quotas.

## Installation

```r
# install.packages("devtools")
devtools::install_github("<your-github-username>/agec840tariffmodel")
```

## Basic usage

```r
library(agec840tariffmodel)
cal <- calibrate.model(kd = 1000, ks = 700, md = 300,
                       eta.d = -0.30, eps.s = 0.50, eps.ms = 5.00,
                       tau = 0.10)
base <- solve.model(cal)
cf <- solve.model(cal, tau = 0.20)
welfare <- welfare.change(base, cf, cal)
print.welfare(welfare)
```
