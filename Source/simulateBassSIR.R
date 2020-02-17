m_bass <- odin::odin({
  deriv(S) <- - foi * S
  deriv(I) <- foi * S - dropout * I
  
  initial(S) <- m - I0 - A0
  initial(I) <- I0
  
  output(Inc) <- foi * S
  output(FOI) <- foi
  output(PF_Ex) <- foi_ex / foi * 100
  output(Re) <- beta / dropout * S / m
  
  foi <- foi_en + foi_ex
  foi_en <- beta * (I / m) 
  foi_ex <- kappa
  
  m <- user(1000)
  I0 <- user(5)
  A0 <- user(5)
  
  kappa <- user(0.01)
  beta <- user(0.3)
  dropout <- user(0)
})


get_bass_pars <- function(fitted, i) {
  xs <- fitted$Parameters[i, ]
  cases <- fitted$Cases_hat[, i]
  
  list(
    kappa = xs$kappa,
    beta = xs$beta,
    dropout = sum(fitted$Offsets),
    m = xs$m,
    I0 = cases[length(cases)],
    A0 = fitted$Cases$A[length(fitted$Cases$A)]
  )
}


get_bass_pars_lockdown <- function(fitted, i) {
  pars <- get_bass_pars(fitted, i)
  pars$kappa <- 0
  return(pars)
}


forecast_bass <- function(fitted, start = 24, end = 100, max_iter = 1000) {
  cm_bass <- m_bass()
  
  ava <- (1:nrow(fitted$Parameters))[!is.na(fitted$Parameters$beta) & !is.na(fitted$Parameters$m)]
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
  
  times <- seq(start, end, by = 0.5)
  
  test <- cm_bass$run(times)
  
  i_hat <- matrix(0, length(fitted$Cases$I) - 2, max_iter)
  re_hat <- matrix(0, length(fitted$Cases$I) - 2, max_iter)
  
  sims <- array(0, c(dim(test), max_iter))
  locks <- array(0, c(dim(test), max_iter))
  
  for (i in 1:max_iter) {
    key <- selected[i]
    p <- get_bass_pars(fitted, key)
    i_hat[, i] <- fitted$Cases_hat[, key]
    re_hat[, i] <- p$beta / p$dropout * (p$m - fitted$Cases$A[-c(1:2)] - i_hat[, i]) / p$m 
    
    cm_bass$set_user(user = p)
    sims[, , i] <- cm_bass$run(times)
    
    cm_bass$set_user(user = get_bass_pars_lockdown(fitted, key))
    locks[, , i] <- cm_bass$run(times)
    
  }
  
  
  dimnames(sims)[[2]] <- dimnames(locks)[[2]] <- colnames(test)
  colnames(i_hat) <- colnames(re_hat) <- dimnames(sims)[[3]] <- dimnames(locks)[[3]] <- selected
  
  
  sims <- sims[sims[,"t", 1] == round(sims[, "t", 1]),,]
  locks <- locks[locks[,"t", 1] == round(locks[, "t", 1]),,]
  
  res$Cases_hat <- i_hat
  res$Re_hat <- re_hat
  res$Base <- sims
  res$Lock <- locks
  class(res) <- "Simulation_BassSIR"
  return(res)
}
