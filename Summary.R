#' Summary
#' 
#' Output summary tables for 
#' - Model fitting and model comparison
#' - Epidemiological evaluation
#'
#'
#' Author: Chu-Chang Ku
#'


##### Result of model fitting -----
rm(list = ls())
load(file = "Output/Fitted.rdata")

results_bass <- data.table::rbindlist(lapply(est_bass, function(x) summary(x)$Pars))
results_si <- data.table::rbindlist(lapply(est_sir, function(x) summary(x)$Pars))
results_comp <- data.table::rbindlist(est_comp)

write.csv(results_bass, "Output/Table/EstBassSIR.csv")
write.csv(results_si, "Output/Table/EstSIR.csv")
write.csv(results_comp, "Output/Table/BayesFactor.csv")


##### Results of simulation
rm(list = ls())
load(file = "Output/Epidemiology.rdata")

write.csv(epis_bass, "Output/Table/EpiIndices.csv")


##### Results of scenario analysis
rm(list = ls())
library(tidyverse)
load(file = "Output/Intervention.rdata")
load(file = "Output/Scenarios.rdata")


results_lockdown <- list()
for (pro in names(intv_lockdown)) {
  x <- intv_lockdown[[pro]]
  results_lockdown[[pro]] <- rbind(
    cbind(Location = pro, index = "PrEx", x$Trajectories$PrEx %>% filter(Time == "2020-02-12")),
    cbind(Location = pro, index = "PAF_2w", x$Changes$I %>% filter(Time == "2020-02-26" & Scenario != "Baseline")),
    cbind(Location = pro, index = "PAF_4w", x$Changes$I %>% filter(Time == "2020-03-11" & Scenario != "Baseline"))
  )
}

results_lockdown <- data.table::rbindlist(results_lockdown)
write.csv(results_lockdown, "Output/Table/Lockdown.csv")

results_scs <- res_agg_scenarios$Trajectories$I 
write.csv(results_scs, "Output/Table/Scenarios.csv")

