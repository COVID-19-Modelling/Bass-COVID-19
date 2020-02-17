#' todo meta data
#'
#'
#'
#'

rm(list = ls())


##### Load requirements -----
source("Source/statistics.R")
source("Source/evaluateEpi.R")
load(file = "Output/Simulation.rdata")


epi_bass <- lapply(sims_bass, eval_epi)
epi_sir <- lapply(sims_sir, eval_epi)


save(epi_bass, epi_sir, file = "Output/Epidemiology.rdata")
