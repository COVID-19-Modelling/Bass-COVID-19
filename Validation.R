#' todo meta data
#'
#'
#'
#'

rm(list = ls())


##### Load requirements -----
library(R2jags)
library(data.table)
source("Source/validate.R")


##### Load data -----
load(file = "Data/data.rdata")
names_province <- unique(n_province$Prov)

get_cases <- function(prov) {
  if (prov == "Overall") {
    dat <- n_china
  } else {
    dat <- subset(n_province, Prov == prov)
  }
  I <- ts(dat$Confirmed, start = min(dat$time))
  A <- ts(dat$Cured + dat$Dead, start = min(dat$time))
  return(list(I = I, A = A))
}


##### Set parameters -----
n_iter <- 1E5 # number of iterations
n_sim <- 1000
n_valid <- 5
r_rec <- 1 / 22.3 # recovery rate
r_death <- 1 / 22.2 # death rate of the infected


list_errors <- list()

exc <- c()
for (pro in names_province) {
  dat <- get_cases(pro)
  
  if (length(dat$I) > 5) {
    list_errors[[pro]] <- validate_bass(pro, dat, r_rec = r_rec, r_death = r_death, n_iter = n_iter, 
                                     n_sim = n_sim, n_valid = n_valid) 
    cat(" completed\n")
  } else {
    exc <- c(exc, pro)
    cat("---- low sample size\n")
  }
}


tab_errors <- rbindlist(list_errors)


g_vld <- visualise_ts_error(tab_errors)

ggsave(g_vld, filename = "Output/Figure/Validation.pdf", height = 10, width = 8)

save(g_vld, tab_errors, file = "Output/Validation.rdata")
