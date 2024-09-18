# PostCard (development version)

## Version 0.2.0.9000 (development version)

Added function `simulate_collection` that takes function arguments for how to simulate covariates and model the outcome in the historical and "current" data to give the user full flexibility.

`sim.lm` which simulates data from a multivariate normal distribution and models the outcome with a linear model is now a wrapper of `simulate_collection`.

## Version 0.2.0

Package functions to simulate and analyse data for specific case of multivariate normal distribution of covariates and linear model of the outcome.
