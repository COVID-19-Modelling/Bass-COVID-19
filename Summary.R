#' Summary
#' 
#' Output summary tables for 
#' - Model fitting and model comparison
#' - Epidemiological evaluation
#'
#'
#' Author: Chu-Chang Ku
#'
rm(list = ls())
library(tidyverse)
library(BassSIR)

##### Result of model fitting -----
load(file = "Output/Fitted.rdata")

results_bass <- data.table::rbindlist(lapply(est_bass, function(x) c(list(Location = x$Cases$ID), summary(x)$Pars)))
results_sir <- data.table::rbindlist(lapply(est_sir, function(x) c(list(Location = x$Cases$ID), summary(x)$Pars)))
results_comp <- data.table::rbindlist(est_comp)

write.csv(results_bass, "Output/Table/EstBassSIR.csv")
write.csv(results_sir, "Output/Table/EstSIR.csv")
write.csv(results_comp, "Output/Table/BayesFactor.csv")


##### Results of simulation
load(file = "Output/Simulation.rdata")
load(file = "Output/Epidemiology.rdata")


traj_I <- list()
traj_Rt <- list()

for (pro in names(est_bass)) {
  traj_I[[pro]] <- to_vis_trajectory(est_bass[[pro]], sims_bass[[pro]], traj = "I")
  traj_Rt[[pro]] <- to_vis_trajectory(est_bass[[pro]], sims_bass[[pro]], traj = "Rt")
}


jsonlite::write_json(traj_I, "Output/JSON/TrajectoryPrv.json")
jsonlite::write_json(traj_Rt, "Output/JSON/TrajectoryRt.json")


write.csv(epis_bass, "Output/Table/EpiIndices.csv")
jsonlite::write_json(epis_bass, "Output/JSON/EpiIndices.json")


##### Results of scenario analysis
load(file = "Output/Intervention.rdata")
load(file = "Output/Scenarios.rdata")


results_lockdown <- list()
for (pro in names(intv_lockdown)) {
  x <- intv_lockdown[[pro]]
  results_lockdown[[pro]] <- rbind(
    cbind(Location = pro, variable = "PrEx", x$Trajectories$PrEx %>% filter(Time == "2020-02-12")),
    cbind(Location = pro, variable = "PAF_2w", x$Changes$I %>% filter(Time == "2020-02-26" & Scenario != "Baseline")),
    cbind(Location = pro, variable = "PAF_4w", x$Changes$I %>% filter(Time == "2020-03-11" & Scenario != "Baseline"))
  )
}

results_lockdown <- data.table::rbindlist(results_lockdown)
write.csv(results_lockdown, "Output/Table/Lockdown.csv")
jsonlite::write_json(results_lockdown, "Output/JSON/Lockdown.json")

results_scs <- res_agg_scenarios$Trajectories$I %>% mutate(variable = "Prevalence")
write.csv(results_scs, "Output/Table/Scenarios.csv")
jsonlite::write_json(results_scs, "Output/JSON/Scenarios.json")



##### Data for visualisation
save(epis_bass, traj_I, traj_Rt, results_lockdown, results_scs, file = "Output/ForVis.rdata")


