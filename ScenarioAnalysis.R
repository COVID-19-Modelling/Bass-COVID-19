#' Scenario Analysis
#'
#' Thinking the scenarios
#' - Recovery rate decreases due to health system dysfunctioning and economic burden
#' - Return to normal life (contact back to the normal level)
#' 
#' 
#' Author: Chu-Chang Ku
#'

rm(list = ls())

options(odin.verbose = F,
        odin.target = ifelse(odin::can_compile(), "c", "r"),
        odin.compiler_warnings = F,
        odin.no_check_unused_equations = T
)


##### Load requirements -----
library(BassSIR)
source("Source/scenarios.R")
load(file = "Output/Simulation.rdata")


##### Lockdown intervention -----
intv_lockdown <- list()

for (pro in names(sims_bass)) {
  sim <- sims_bass[[pro]]
  lockdown <- run_scenario(sim, zero_kappa)
  intv_lockdown[[pro]] <- compare_scenarios(sim, Lockdown = lockdown, fn_change = "Y0")
}


##### Scenarios -----
res_scenarios <- list()

s1 <- gen_rec_decay(0.2)
s2 <- gen_rec_decay(0.5)
s3 <- gen_bost_m(at = 12, by = 2)
s4 <- gen_bost_m(at = 19, by = 2)


for (pro in names(sims_bass)) {
  sim <- sims_bass[[pro]]

  res_scenarios[[pro]] <- 
    compare_scenarios(sim, 
                      Rec2 = run_scenario(sim, s1),
                      Rec5 = run_scenario(sim, s2),
                      Open12 = run_scenario(sim, s3),
                      Open19 = run_scenario(sim, s4),
                      fn_change = "Yt")
}

scs <- c("Baseline", 
         "Recovery rate decaying by 20% per month", 
         "Recovery rate decaying by 50% per month", 
         "Effective population size doubled after 24th Feb", 
         "Effective population size doubled after 2nd Mar"
)

##### Output -----
save(intv_lockdown, file = "Output/Intervention.rdata")
save(res_scenarios, scs, file = "Output/Scenarios.rdata")
