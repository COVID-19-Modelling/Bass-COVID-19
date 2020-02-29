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
library(BassSIR)

load(file = "Output/Fitted.rdata")

max_iter <- 3000 # to match the posterior parameters

set.seed(1166)


##### Simulate -----
sims_bass <- lapply(est_bass, simulate, nsim = max_iter)
sims_sir <- lapply(est_sir, simulate, nsim = max_iter)


##### Epidemic indices
epis_bass <- data.table::rbindlist(lapply(sims_bass, function(x) summary(x)$Indices))
epis_sir <- data.table::rbindlist(lapply(sims_sir, function(x) summary(x)$Indices))


##### Output -----
save(sims_bass, sims_sir, file = "Output/Simulation.rdata")
save(epis_bass, epis_sir, file = "Output/Epidemiology.rdata")
