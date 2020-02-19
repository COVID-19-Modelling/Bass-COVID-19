#' Epidemic forecasting
#'
#' Based on the results of model fitting, generate epidemic curves under
#' - baseline parameters
#' - lockdown intervention: fixed kappa to zero as reduced models
#' 
#' Author: Chu-Chang Ku
#'

rm(list = ls())


##### Load requirements -----
source("Source/statistics.R")
source("Source/simulateBassSIR.R")
source("Source/simulateSIR.R")

load(file = "Output/Fitted.rdata")

max_iter <- 300 # 3000 to match the posterior parameters

set.seed(1166)


##### Simulate -----
sims_bass <- lapply(list_bass, forecast_bass, end = 80, max_iter = max_iter)
sims_sir <- lapply(list_sir, forecast_sir, end = 80, max_iter = max_iter)


##### Output -----
save(sims_bass, sims_sir, file = "Output/Simulation.rdata")
