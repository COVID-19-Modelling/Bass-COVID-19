#' Test different lengths of the disease duration on R0 and R(t)
#'
#'
#'
#'

rm(list = ls())


##### Load requirements -----
library(R2jags)
library(data.table)
source("Source/statistics.R")
source("Source/fitBassSIR.R")
source("Source/fitSIR.R")


##### Load data -----
load(file = "Data/data.rdata")
names_province <- unique(n_province$Prov)

get_cases <- function(prov) {
  dat <- subset(n_province, Prov == prov)
  I <- ts(dat$Confirmed, start = min(dat$time))
  A <- ts(dat$Cured + dat$Dead, start = min(dat$time))
  return(list(I = I, A = A))
}


##### Set parameters -----
n_iter <- 1E5 # number of iterations
r_rec <- 1 / 12 # recovery rate
r_death <- 1 / 12 # death rate of the infected

meta <- list(
  r_rec = r_rec,
  r_death = r_death,
  disease_duration = 1 / (r_rec + r_death),
  n_iter = niter,
  models = c("BassSIR"),
  provinces = names_province
)

##### Fit models -----
list_bass <- list()

exc <- c()
for (pro in names_province) {
  dat <- get_cases(pro)
  
  if (length(dat$I) > 5) {
    cat(pro, "--")
    list_bass[[pro]] <- fit_bass(pro, dat, n_iter, r_rec = r_rec, r_death = r_death)
    
    cat(" completed\n")
  } else {
    exc <- c(exc, pro)
    cat("---- low sample size\n")
  }
}


save(meta, list_bass, exc, file = "Output/Fitted_6d.rdata")
list_bass_6 <- list_bass

##### Set parameters -----
n_iter <- 1E5 # number of iterations
r_rec <- 1 / 44 # recovery rate
r_death <- 1 / 44 # death rate of the infected

meta <- list(
  r_rec = r_rec,
  r_death = r_death,
  disease_duration = 1 / (r_rec + r_death),
  n_iter = niter,
  models = c("BassSIR"),
  provinces = names_province
)

##### Fit models -----
list_bass <- list()

exc <- c()
for (pro in names_province) {
  dat <- get_cases(pro)
  
  if (length(dat$I) > 5) {
    cat(pro, "--")
    list_bass[[pro]] <- fit_bass(pro, dat, n_iter, r_rec = r_rec, r_death = r_death)
    
    cat(" completed\n")
  } else {
    exc <- c(exc, pro)
    cat("---- low sample size\n")
  }
}


save(meta, list_bass, exc, file = "Output/Fitted_6d.rdata")
list_bass_22 <- list_bass


##### Summarise results -----
load(file = "Output/Fitted.rdata")


list_bass_22 <- list_bass
list_bass_11 <- list_bass
list_bass_6 <- list_bass





##### Output results -----
sens <- summary_sensitivity(list_bass_6, list_bass_11, list_bass_22, 
                            lables = c("D6", "D11", "D22"))


write.csv(sens, "Output/Table/Sensitivity.csv")

save(sens, list_bass_6, list_bass_11, list_bass_22, file = "Output/Sensitivity.rdata")
