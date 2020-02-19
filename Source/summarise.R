summary_r0 <- function(lbass) {
  res <- data.table::rbindlist(lapply(lbass, function(f) {
    with(f$Parameters, {
      n_t <- nrow(f$Cases_hat)
      cases <- f$Cases_hat[nrow(f$Cases_hat),]
      sus <- m - cases - f$Cases$A[length(f$Cases$A)] 
      
      r0 <- beta / sum(f$Offsets)
      re <- r0 * sus / m
      
      list(
        Location = f$Summary$Location,
        "Effective population size (thousand)" = stats_fn(m / 1E3), 
        R0 = stats_fn(r0),
        "R(t)" = stats_fn(re)
      )
    })
  }))
  
  res <- res[order(Location), ]
  
  return(res)
}


summary_fitted <- function(lbass, lbf) {
  res <- data.table::rbindlist(lapply(lbass, function(f) {
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


summary_sensitivity <- function(..., labels = NULL) {
  models <- list(...)
  
  if (length(labels) != length(models)) {
    labels <- paste0("::S ", 1:length(models))
  } else {
    labels <- paste0("::", labels)
  }
  
  tab <- lapply(models, function(fitted) {
    res <- data.table::rbindlist(lapply(fitted, function(f) {
      with(f$Parameters, {
        n_t <- nrow(f$Cases_hat)
        cases <- f$Cases_hat[nrow(f$Cases_hat),]
        sus <- m - cases - f$Cases$A[length(f$Cases$A)] 
        
        r0 <- beta / sum(f$Offsets)
        re <- r0 * sus / m
        
        list(
          Location = f$Summary$Location,
          "Effective population size (thousand)" = stats_fn(m / 1E3), 
          R0 = stats_fn(r0),
          "R(t)" = stats_fn(re)
        )
      })
    }))
    
    res <- res[order(Location), ]
  })
  
  res <- tab[[1]]
  for (i in 2:length(models)) {
    res <- merge(res, tab[[i]], by="Location", suffixes = c("", labels[i]))
  }
  names(res)[2:4] <- paste0(names(res)[2:4], labels[1])
  
  return(res)
}