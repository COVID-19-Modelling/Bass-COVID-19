summary_r0 <- function(lbass) {
  res <- rbindlist(lapply(lbass, function(f) {
    with(f$Parameters, {
      n_t <- nrow(f$Cases_hat)
      cases <- f$Cases_hat[nrow(f$Cases_hat),]
      sus <- m - cases - f$Cases$A[length(f$Cases$A)] 
      
      r0 <- beta / sum(f$Offsets)
      re <- r0 * sus / m
      
      list(
        Location = f$Summary$Location,
        R0 = stats_fn(r0),
        "R(t)" = stats_fn(re)
      )
    })
  }))
  
  res <- res[order(Location), ]
  
  return(res)
}


summary_fitted <- function(lbass, lbf) {
  res <- rbindlist(lapply(lbass, function(f) {
    with(f$Parameters, {
      n_t <- nrow(f$Cases_hat)
      cases <- f$Cases_hat[nrow(f$Cases_hat),]
      sus <- m - cases - f$Cases$A[length(f$Cases$A)] 
      
      r0 <- beta / sum(f$Offsets)
      re <- r0 * sus / m
      
      list(
        Location = f$Summary$Location,
        "Kappa * 100" = stats_fn(kappa * 100), 
        Beta = stats_fn(beta), 
        "Effective population size (thousand)" = stats_fn(m / 1E3), 
        R0 = stats_fn(r0),
        "R(t)" = stats_fn(re),
        Deviance = stats_fn(deviance)
      )
    })
  }))
  
  res <- res[order(Location), ]
  
  bfs <- rbindlist(lbf)
  bfs$BF <- round(1 / bfs$BF.SIR, 2)
  bfs$DIC.BassSIR <- round(bfs$DIC.BassSIR, 2)
  bfs$DIC.SIR <- round(bfs$DIC.SIR, 2)
  bfs <- bfs[, c("Location", "BF", "DIC.BassSIR", "DIC.SIR")]

  
  res <- merge(res, bfs, by = "Location")
  
  return(res)
}


summary_intervention <- function() {
  
}