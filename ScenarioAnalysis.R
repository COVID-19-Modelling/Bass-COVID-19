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


##### Load requirements -----
source("Source/statistics.R")
source("Source/analyseScenarios.R")
load(file = "Output/Fitted.rdata")


max_iter <- 1000
set.seed(1166)


##### Simulate -----
res_scenarios <- list()
for (i in names(list_bass)) {
  res_scenarios[[i]] <- simulate_scenarios(list_bass[[i]], 0, 30, max_iter = max_iter)
  print(paste0(i, " -- completed"))
}


res_agg_scenarios <- aggregate_scenarios(res_scenarios)


##### Output -----
save(res_scenarios, res_agg_scenarios, file = "Output/Scenarios.rdata")
