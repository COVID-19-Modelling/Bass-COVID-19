#' todo meta data
#'
#'
#'
#'

rm(list = ls())

source("Source/statistics.R")
source("Source/summarise.R")

load(file = "Output/Fitted.rdata")


tab_fitted <- summary_fitted(list_bass, list_bfs)

write.csv(tab_fitted, "Output/Table/SummaryFitting.csv")
