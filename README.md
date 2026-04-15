# agec840tariffmodel

This package implements a one-country, one-commodity partial equilibrium trade policy model based on the Perfect Substitutes Trade Model by Francois (1997). It extends the base framework to cover import tariffs, production subsidies, consumption subsidies, and binding import quotas.

The package provides functions for calibrating benchmark data, solving counterfactual equilibria, and decomposing welfare effects.

## Installation

```r
# install.packages("devtools")
devtools::install_github("nvilloria/agec840tariffmodel", build_vignettes = TRUE)
```

## Basic usage

```r
library(agec840tariffmodel)

# Calibrate model parameters
cal <- calibrate_model(kd = 1000, ks = 700, md = 300,
                       eta.d = -0.30, eps.s = 0.50, eps.ms = 5.00,
                       tau = 0.10)

# Solve for equilibria
base <- solve_taxes(cal)
cf <- solve_taxes(cal, tau = 0.20)

# Calculate and print welfare effects
welfare <- welfare_change(base, cf, cal)
print_welfare(welfare)
```

## References

Francois, J.F., and Hall, H.K. (1997). "Partial Equilibrium Modeling." Chapter 5 in *Applied Methods for Trade Policy Analysis: A Handbook*, edited by J.F. Francois and K.A. Reinert. Cambridge: Cambridge University Press.

The original spreadsheet implementations are available from the Institute for International and Development Economics at: <https://www.i4ide.org/people/~francois/Models/index.htm>
```
