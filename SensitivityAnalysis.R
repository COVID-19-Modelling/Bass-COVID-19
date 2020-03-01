#' Sensitivity Analysis
#' 
#' Test different lengths of the disease duration on R0 and R(t)
#'
#'
#' Author: Chu-Chang Ku
#'

rm(list = ls())


##### Load requirements -----
library(BassSIR)

names_province <- sort(names(n_covid19))


##### Set parameters -----
n_iter <- 1E4 # number of iterations
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
est_bass <- list()

exc <- c()
for (pro in names_province) {
  cases <- as_bass_data(n_covid19[[pro]], id = pro)
  
  if (cases$len > 5) {
    est_bass[[pro]] <- BassSIR::fit(cases, r_rec = r_rec, r_death = r_death, type = "BassSIR", n_iter = n_iter)
    cat(" completed\n")
  } else {
    exc <- c(exc, pro)
    cat("---- low sample size\n")
  }
}

results_bass_6 <- data.table::rbindlist(lapply(est_bass, 
                                        function(x) c(list(Location = x$Cases$ID), summary(x)$Pars)))


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
est_bass <- list()

exc <- c()
for (pro in names_province) {
  cases <- as_bass_data(n_covid19[[pro]], id = pro)
  
  if (cases$len > 5) {
    est_bass[[pro]] <- BassSIR::fit(cases, r_rec = r_rec, r_death = r_death, type = "BassSIR", n_iter = n_iter)
    cat(" completed\n")
  } else {
    exc <- c(exc, pro)
    cat("---- low sample size\n")
  }
}

results_bass_22 <- data.table::rbindlist(lapply(est_bass, 
                                         function(x) c(list(Location = x$Cases$ID), summary(x)$Pars)))



##### Summarise results -----
load(file = "Output/Fitted.rdata")


results_bass_11 <- data.table::rbindlist(lapply(est_bass, 
                                         function(x) c(list(Location = x$Cases$ID), summary(x)$Pars)))


##### Output results -----


sens <- results_bass_11
sens <- merge(sens, results_bass_6, by = "Location", suffixes = c("", "::D6"))
sens <- merge(sens, results_bass_22, by = "Location", suffixes = c("", "::D22"))

names(sens)[2:ncol(results_bass_11)] <- paste0(names(sens)[2:ncol(results_bass_11)], "::D11")


write.csv(sens, "Output/Table/Sensitivity.csv")

save(sens, results_bass_6, results_bass_11, results_bass_22, file = "Output/Sensitivity.rdata")
