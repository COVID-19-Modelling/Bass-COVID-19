#' todo meta data
#'
#'
#'
#'

rm(list = ls())


##### Load requirements -----
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)

source("Source/statistics.R")
source("Source/data4vis.R")
source("Source/visualise.R")

load(file = "Output/Epidemiology.rdata")
load(file = "Output/Scenarios.rdata")

width <- 6
height <- 7


##### Reproduction numbers -----
g_re <- visualise_re(epi_bass)

ggsave(plot = g_re, filename = "Output/Figure/R0.pdf", height = height, width = width)


##### Epidemic peak -----
gs_peak <- visualise_peak(epi_bass)

ggsave(plot = gs_peak$g_peak_size, filename = "Output/Figure/PeakSize.pdf", height = height, width = width)
ggsave(plot = gs_peak$g_peak_time, filename = "Output/Figure/PeakTime.pdf", height = height, width = width)


##### Lockdown assessment -----
g_lock <- visualise_lockdown(epi_bass, bind = T)
ggsave(plot = g_lock, filename = "Output/Figure/Lock.pdf", height = 7, width = 10)

gs_lock <- visualise_lockdown(epi_bass, bind = F)
ggsave(plot = gs_lock$g_exo, filename = "Output/Figure/ExFOI.pdf", height = height, width = width)
ggsave(plot = gs_lock$g_paf, filename = "Output/Figure/PAF.pdf", height = height, width = width)


##### Epi curves by province -----
g_prv_log <- visualise_ts_prv(epi_bass, log = T)
g_prv <- visualise_ts_prv(epi_bass, log = F)
g_re <- visualise_ts_r0(epi_bass)
g_pex <- visualise_ts_pex(epi_bass)

ggsave(plot = g_prv, filename = "Output/Figure/Cases.pdf", height = 11, width = 8)
ggsave(plot = g_prv_log, filename = "Output/Figure/CasesLog.pdf", height = 11, width = 8)
ggsave(plot = g_re, filename = "Output/Figure/R(t).pdf", height = 11, width = 8)
ggsave(plot = g_pex, filename = "Output/Figure/Pr(Exo).pdf", height = 11, width = 8)

##### Scenario analysis

g_scs <- visualise_scenarios(res_agg_scenarios)

ggsave(plot = g_scs, filename = "Output/Figure/Scenarios.pdf", height = 9, width = 8)

##### Individaul epi curves -----

gs_prov_bass <- list()

for (i in names(epi_bass)) {
  epi <- epi_sir[[i]]
  g_prov <- visualise_ts_prov(epi, i)
  gs_prov_bass[[i]] <- g_prov
  
  ggsave(plot = g_prov, filename = paste0("Output/Figure/ByProvince/Epi", i, "_Bass.pdf"), 
         height = height, width = width)
}


gs_prov_sir <- list()
for (i in names(epi_sir)) {
  epi <- epi_sir[[i]]
  g_prov <- visualise_ts_prov(epi, i)
  gs_prov_sir[[i]] <- g_prov
  
  ggsave(plot = g_prov, filename = paste0("Output/Figure/ByProvince/Epi", i, "_SIR.pdf"), 
         height = height, width = width)
}
