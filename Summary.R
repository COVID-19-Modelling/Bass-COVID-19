#' todo meta data
#'
#'
#'
#'

rm(list = ls())

source("Source/statistics.R")
source("Source/summarise.R")

load(file = "Output/Fitted.rdata")
load(file = "Output/Epidemiology.rdata")


tab_fitted <- summary_fitted(list_bass, list_bfs)

write.csv(tab_fitted, "Output/Table/SummaryFitting.csv")


dat <- collect_statistics(epi_bass)

peaks <- dat %>% filter(Loc != "Overall") %>% select(Loc, PeakSize) %>% group_by(Loc) %>% 
  summarise_all(funs(mean = round(mean(.)), lower = round(quantile(., 0.025)), 
                     upper = round(quantile(., 0.975)))) %>% 
  mutate(cs = paste0(mean, " (95% PrI: ", lower, ", ", upper, ")"))

write.csv(peaks, "Output/Table/Peak.csv")

