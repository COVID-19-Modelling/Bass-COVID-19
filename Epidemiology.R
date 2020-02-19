#' Epidemiological evaluation with simulation results
#'
#' Calculate:
#' - Outbreak peak
#' - R(t)
#' - Fractions of exogenous force of infection among all incident cases
#'
#'
#' Author: Chu-Chang Ku
#'

rm(list = ls())


##### Load requirements -----
source("Source/statistics.R")
source("Source/evaluateEpi.R")
load(file = "Output/Simulation.rdata")


epi_bass <- lapply(sims_bass, eval_epi)
epi_sir <- lapply(sims_sir, eval_epi)


save(epi_bass, epi_sir, file = "Output/Epidemiology.rdata")
