eval_epi <- function(sim, date0 = as.Date("2020-02-12", "%Y-%m-%d"), target = 30) {
  tab <- sim$Base
  intv <- sim$Lock
  
  start <- 43
  start_i <- 1
  target <- start + target
  target_i <- target - start + 1
  date_start <- date0 + start_i - 1
  
  n_iter <- dim(tab)[3]
  
  epi <- data.frame(ID = 1:n_iter)
  epi$Re <- tab[start_i, "Re", ]
  epi$PeakTime <- apply(tab[, "I", ], 2, which.max)
  epi$PeakSize <- apply(tab[, "I", ], 2, max)
  epi$RE1Time <- apply(tab[, "Inc", ], 2, which.max)
  epi$RE1Size <- apply(tab[, "Inc", ], 2, max)
  epi$m <- sim$Shanghai$Parameters$m
  
  if ("PF_Ex" %in% dimnames(tab)[[2]]) {
    epi$PrExFOI <- tab[start_i, "PF_Ex", ]
    epi$PrExFOI <- tab[target_i, "PF_Ex", ]
  }
  
  epi$EffN <- sim$Parameters$m
  
  paf <- (tab[, "I", ] - intv[, "I", ]) / tab[, "I", ] * 100 
  epi$PAF_s <- paf[start_i, ]
  epi$PAF_c <- paf[target_i, ]
  epi$PAF_2 <- paf[start_i + 14, ]
  epi$PAF_4 <- paf[start_i + 28, ]
  
  
  tss_fore <- list(
    Date = date_start + 1:dim(tab)[1] - 1, 
    PAF = paf,
    Inc = tab[, "Inc", ],
    Risky =  matrix(sim$Parameters$m, dim(tab)[1], dim(tab)[3], byrow = T) - tab[, "S", ],
    Prv = tab[, "I", ],
    Re = tab[, "Re", ]
  )
  
  tss_back <- list(
    Date = date_start - (nrow(sim$Cases_hat):1) + 1,
    Re_hat = sim$Re_hat,
    Cases_hat = sim$Cases_hat
  )
  
  cases <- sim$Data$I
  n_c <- length(cases)
  dat <- data.table::data.table(Index = rep(c("Comfirmed cases"), each = n_c), 
                    Value =  c(cases),
                    Date = date_start - n_c:1 + 1)
  
  return(list(
    Location = sim$Location,
    Statistics = data.table(epi),
    Forecasts = tss_fore,
    Backcasts = tss_back,
    Data = dat
  ))
}
