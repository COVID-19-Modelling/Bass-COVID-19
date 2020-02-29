#' Model fitting
#' 
#' Fit Bass-SIR models to notification data of comfirmed cases, the recovered, and the dead
#' 
#' Author: Chu-Chang Ku
#'

rm(list = ls())


##### Load requirements -----
library(BassSIR)

names_province <- sort(names(n_covid19))


##### Set parameters -----
n_iter <- 1E4 # number of iterations
r_rec <- 1 / 22.3 # recovery rate
r_death <- 1 / 22.2 # death rate of the infected

meta <- list(
  r_rec = r_rec,
  r_death = r_death,
  disease_duration = 1 / (r_rec + r_death),
  n_iter = n_iter,
  models = c("BassSIR", "SIR"),
  provinces = names_province
)

##### Fit models -----
est_comp <- list()
est_bass <- list()
est_sir <- list()

fitted_bass <- list()
fitted_sir <- list()

exc <- c()
for (pro in names_province) {
  cases <- as_bass_data(n_covid19[[pro]], id = pro)
  
  if (cases$len > 5) {
    fb <- BassSIR::fit(cases, r_rec = r_rec, r_death = r_death, type = "BassSIR", n_iter = n_iter)
    fs <- BassSIR::fit(cases, r_rec = r_rec, r_death = r_death, type = "SIR", n_iter = n_iter)

    cat(pro, "--")
    est_bass[[pro]] <- fb
    est_sir[[pro]]  <- fs
    
    fitted_bass[[pro]] <- fitted(fb)
    fitted_sir[[pro]] <- fitted(fs)

    cat("--")
    est_comp[[pro]] <- compare_models(BassSIR = fb, SIR = fs)
    
    cat(" completed\n")
  } else {
    exc <- c(exc, pro)
    cat("---- low sample size\n")
  }
}


##### Result extraction -----
results_bass <- data.table::rbindlist(lapply(est_bass, function(x) summary(x)$Pars))
results_si <- data.table::rbindlist(lapply(list_sir, function(x) summary(x)$Pars))
results_comp <- data.table::rbindlist(est_comp)


##### Output results -----
write.csv(results_bass, "Output/Table/EstBassSIR.csv")
write.csv(results_si, "Output/Table/EstSIR.csv")
write.csv(results_bfs, "Output/Table/BayesFactor.csv")

save(meta, est_comp, est_bass, est_sir, 
     fitted_bass, fitted_sir, exc, file = "Output/Fitted.rdata")
