
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