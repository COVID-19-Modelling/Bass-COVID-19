#' todo meta data
#'
#'
#'
#'

rm(list = ls())


##### Load requirements -----
source("Source/statistics.R")
source("Source/simulateBassSIR.R")
source("Source/simulateSIR.R")

load(file = "Output/Fitted.rdata")

max_iter <- 100

set.seed(1166)


##### Simulate -----
sims_bass <- lapply(list_bass, forecast_bass, end = 80, max_iter = max_iter)
sims_sir <- lapply(list_sir, forecast_sir, end = 80, max_iter = max_iter)


##### Output -----
save(sims_bass, sims_sir, file = "Output/Simulation.rdata")
