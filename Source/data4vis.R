collect_statistics <- function(epis) {
  dat <- list()
  for (i in names(epis)) {
    tab <- epis[[i]]$Statistics[, -1]
    tab <- cbind(Loc = i, tab)
    dat[[i]] <- tab
  }
  return(rbindlist(dat))
}



extract_tab <- function(epi, key, name) {
  sel <- epi$Forecasts[[key]]
  
  tss <- data.table(Date = epi$Forecasts$Date, 
                    Index = name,
                    mean = apply(sel, 1, mean),
                    lower = apply(sel, 1, quantile, p = 0.025), 
                    upper = apply(sel, 1, quantile, p = 0.975))
  return(tss)
}


extract_fitted <- function(epi, key, name) {
  sel <- epi$Backcasts[[key]]
  
  tss <- data.table(Date = epi$Backcasts$Date, 
                    Index = name,
                    mean = apply(sel, 1, mean),
                    lower = apply(sel, 1, quantile, p = 0.025), 
                    upper = apply(sel, 1, quantile, p = 0.975))
  return(tss)
}


extract_peak <- function(epi, key, name) {
  date0 <- epi$Date0
  
  sel <- epi$TimeSeries[[key]]
  nn <- apply(sel, 2, max)
  tt <- apply(sel, 2, which.max)
  
  return(data.table(
    Index = name,
    size_mean = mean(nn),
    size_lower = quantile(nn, 0.025),
    size_upper = quantile(nn, 0.975),
    time_mean = round(mean(tt)) + date0,
    time_lower = round(quantile(tt, 0.025)) + date0,
    time_upper = round(quantile(tt, 0.975)) + date0
  ))
}
