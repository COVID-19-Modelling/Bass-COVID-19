#' todo meta data
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
r_rec <- 1 / 22.3 # recovery rate
r_death <- 1 / 22.2 # death rate of the infected

meta <- list(
  r_rec = r_rec,
  r_death = r_death,
  disease_duration = 1 / (r_rec + r_death),
  n_iter = niter,
  models = c("BassSIR", "SIR"),
  provinces = names_province
)

##### Fit models -----
list_bfs <- list()
list_bass <- list()
list_sir <- list()

exc <- c()
for (pro in names_province) {
  dat <- get_cases(pro)

  if (length(dat$I) > 5) {
    fitted_bass <- fit_bass(pro, dat, n_iter, r_rec = r_rec, r_death = r_death)
    fitted_sir <- fit_sir(pro, dat, n_iter, r_rec = r_rec, r_death = r_death)

    cat(pro, "--")
    list_bass[[pro]] <- fitted_bass
    list_sir[[pro]] <- fitted_sir

    cat("--")
    list_bfs[[pro]] <- calc_bayes_factor(fitted_bass, fitted_sir)
    
    cat(" completed\n")
  } else {
    exc <- c(exc, pro)
    cat("---- low sample size\n")
  }
}


##### Result extraction -----
results_bass <- as.data.frame(rbindlist(lapply(list_bass, function(x) x$Summary)))
results_si <- as.data.frame(rbindlist(lapply(list_sir, function(x) x$Summary)))
results_bfs <- as.data.frame(rbindlist(lapply(list_bfs, function(x) as.list(x))))


##### Output results -----
write.csv(results_bass, "Output/Table/EstBassSIR.csv")
write.csv(results_si, "Output/Table/EstSIR.csv")
write.csv(results_bfs, "Output/Table/BayesFactor.csv")

save(meta, list_bass, list_sir, list_bfs, exc, file = "Output/Fitted.rdata")
