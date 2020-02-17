m_sir <- odin::odin({
  deriv(S) <- - foi * S
  deriv(I) <- foi * S - dropout * I
  
  initial(S) <- m - I0 - A0
  initial(I) <- I0
  
  output(Inc) <- foi * S
  output(FOI) <- foi
  output(Re) <- (beta / dropout) * (S / m)
  
  foi <- beta * (I / m) 
  
  m <- user(1000)
  I0 <- user(5)
  A0 <- user(5)
  
  beta <- user(0.3)
  dropout <- user(0)
})


get_sir_pars <- function(fitted, i) {
  xs <- fitted$Parameters[i, ]
  cases <- fitted$Cases_hat[, i]
  
  list(
    beta = xs$beta,
    dropout = sum(fitted$Offsets),
    m = xs$m,
    I0 = cases[length(cases)],
    A0 = fitted$Cases$A[length(fitted$Cases$A)]
  )
}


forecast_sir <- function(fitted, start = 24, end = 100, max_iter = 1000) {
  cm_sir <- m_sir()
  
  ava <- 1:nrow(fitted$Parameters)
  n_iter <- length(ava)

  if (max_iter == n_iter) {
    selected <- ava
  } else if (max_iter < n_iter) {
    selected <- sample(ava, max_iter, rep = F)
  } else {
    selected <- sample(ava, max_iter, rep = T)
  }
  
  start <- start + length(fitted$Cases$I) + 2
  res <- list(
    Location = fitted$Summary$Location,
    Time = start:end,
    Data = fitted$Cases,
    Parameters = fitted$Parameters[selected, ]
  )
  
  times <- seq(start - 1, end, by = 0.5)
  
  test <- cm_sir$run(times)
  
  i_hat <- matrix(0, length(fitted$Cases$I) - 2, max_iter)
  re_hat <- matrix(0, length(fitted$Cases$I) - 2, max_iter)
  
  sims <- array(0, c(dim(test), max_iter))
  
  for (i in 1:max_iter) {
    key <- selected[i]
    p <- get_sir_pars(fitted, key)

    i_hat[, i] <- fitted$Cases_hat[, key]
    re_hat[, i] <- p$beta / p$dropout * (p$m - fitted$Cases$A[-c(1:2)] - i_hat[, i]) / p$m 
    
    cm_sir$set_user(user = p)
    sims[, , i] <- cm_sir$run(times)
  }
  
  dimnames(sims)[[2]] <- colnames(test)
  colnames(i_hat) <- colnames(re_hat) <- dimnames(sims)[[3]] <- selected
  
  sims <- sims[sims[,"t", 1] == round(sims[, "t", 1]),,]
  
  res$Cases_hat <- i_hat
  res$Re_hat <- re_hat
  res$Base <- sims
  res$Lock <- sims
  class(res) <- "Simulation_SIR"
  
  return(res)
}
