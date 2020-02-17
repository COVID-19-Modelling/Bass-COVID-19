m_intv <- odin::odin({
  # ODEs
  deriv(S) <- - foi * S
  deriv(I) <- foi * S - gam_t * I
  
  
  # Initial values
  initial(S) <- m_t - I0 - A0
  initial(I) <- I0
  
  I0 <- user(5)
  A0 <- user(5)
  
  
  # Output variable
  output(Rt) <- beta_t / gam_t * S / m_t
  
  foi <- beta_t * (I / m_t) + kappa

  
  # Time varying parameters
  m_t <- interpolate(tt, m, "linear")
  beta_t <- interpolate(tt, beta, "linear")
  gam_t <- interpolate(tt, gam, "linear")
  
  
  # Parameter loading
  n_tt <- length(tt)
  
  tt[] <- user()
  dim(tt) <- user()
  
  m[] <- user()
  dim(m) <- n_tt
  
  beta[] <- user()
  dim(beta) <- n_tt
  
  gam[] <- r_rec[i] + r_die[i]
  dim(gam) <- n_tt
  
  r_rec[] <- user()
  dim(r_rec) <- n_tt
  
  r_die[] <- user()
  dim(r_die) <- n_tt
  
  kappa <- user(0.01)
})


get_intv_pars <- function(fitted, i, tt = 0:50) {
  xs <- fitted$Parameters[i, ]
  cases <- fitted$Cases_hat[, i]
  
  n_tt <- length(tt)
  
  return(list(
    tt = tt,
    kappa = xs$kappa,
    beta = rep(xs$beta, n_tt),
    r_rec = rep(fitted$Offsets[1], n_tt),
    r_die = rep(fitted$Offsets[2], n_tt),
    m = rep(xs$m, n_tt),
    I0 = cases[length(cases)],
    A0 = fitted$Cases$A[length(fitted$Cases$A)]
  ))
}


scenario_none <- function(pars, model, times) {
  model$set_user(user = pars)
  ys <- model$run(times)
  ys <- ys[times == round(times),]
  return(ys)
}


scenario_rec_discount <- function(pars, model, times, dis = 0.2) {
  rr <- - log(1 - dis) / 30
  pars$r_rec <- pars$r_rec * exp(-rr * (pars$tt - pars$tt[1]))
  
  return(scenario_none(pars, model, times))
}


scenario_opening_m <- function(pars, model, times, at = 12, by = 2) {
  times0 <- times[times <= at]
  times1 <- times[times >= at]
  
  ys_o <- scenario_none(pars, model, times0)

  n_t0 <- nrow(ys_o)
  
  pars$I0 <- ys_o[n_t0, "I"]
  pars$A0 <- pars$m[n_t0] - ys_o[n_t0, "I"] - ys_o[n_t0, "S"]
  pars$m <- pars$m * by
  
  ys_i <- scenario_none(pars, model, times1)
  return(rbind(ys_o, ys_i[-1, ]))
}


scenario_opening_beta <- function(pars, model, times, at = 12, by = 2) {
  times0 <- times[times <= at]
  times1 <- times[times >= at]
  
  ys_o <- scenario_none(pars, model, times0)
  
  n_t0 <- nrow(ys_o)
  
  pars$I0 <- ys_o[n_t0, "I"]
  pars$A0 <- pars$m[n_t0] - ys_o[n_t0, "I"] - ys_o[n_t0, "S"]
  pars$beta <- pars$beta * by
  
  ys_i <- scenario_none(pars, model, times1)
  return(rbind(ys_o, ys_i[-1, ]))
}


summary_scenarios <- function(prvs, scs, date0 = as.Date("2020-02-12", "%Y-%m-%d")) {
  summ <- list()
  for (i in 1:length(scs)) {
    sc <- scs[i]
    prv <- prvs[,, i]
    
    summ[[i]] <- data.table(Date = date0 + 1:nrow(prv) - 1, 
                          Scenario = sc,
                          mean = apply(prv, 1, mean),
                          lower = apply(prv, 1, quantile, p = 0.025), 
                          upper = apply(prv, 1, quantile, p = 0.975)
    )
  }
  
  return(summ)
}


simulate_scenarios <- function(fitted, start = 0, end = 30, max_iter = 1000, date0 = as.Date("2020-02-12", "%Y-%m-%d")) {
  ava <- 1:nrow(fitted$Parameters)
  n_iter <- length(ava)
  
  if (max_iter == n_iter) {
    selected <- ava
  } else if (max_iter < n_iter) {
    selected <- sample(ava, max_iter, rep = F)
  } else {
    selected <- sample(ava, max_iter, rep = T)
  }
  
  
  times <- seq(start, end, by = 0.5)
  pars <- get_intv_pars(fitted, 1)
  cm <- m_intv(user = pars)
  
  test <- scenario_none(pars, cm, times)
  
  prvs <- array(0, c(nrow(test), max_iter, 5))
  rts <- array(0, c(nrow(test), max_iter, 5))
  incrs <- array(0, c(nrow(test), max_iter, 5))
  
  for (i in 1:max_iter) {
    pars <- get_intv_pars(fitted, i)

    ys0 <- scenario_none(pars, cm, times)
    ys1 <- scenario_rec_discount(pars, cm, times, 0.2)
    ys2 <- scenario_rec_discount(pars, cm, times, 0.5)
    ys3 <- scenario_opening_m(pars, cm, times, 12, 2)
    ys4 <- scenario_opening_m(pars, cm, times, 19, 2)
    # ys5 <- scenario_opening_beta(pars, cm, times, 12, 2)
    # ys6 <- scenario_opening_beta(pars, cm, times, 19, 2)
    prvs[, i, 1] <- ys0[, "I"]
    prvs[, i, 2] <- ys1[, "I"]
    prvs[, i, 3] <- ys2[, "I"]
    prvs[, i, 4] <- ys3[, "I"]
    prvs[, i, 5] <- ys4[, "I"]
    # prvs[, i, 6] <- ys5[, "I"]
    # prvs[, i, 7] <- ys6[, "I"]
    
    rts[, i, 1] <- ys0[, "Rt"]
    rts[, i, 2] <- ys1[, "Rt"]
    rts[, i, 3] <- ys2[, "Rt"]
    rts[, i, 4] <- ys3[, "Rt"]
    rts[, i, 5] <- ys4[, "Rt"]
    # rts[, i, 6] <- ys5[, "Rt"]
    # rts[, i, 7] <- ys6[, "Rt"]
    
    incrs[, i, ] <- (prvs[, i, ] / prvs[, i, 1] - 1)  * 100
  }
  
  scs <- c("Baseline", 
           "Recovery rate decaying by 20%/month", "Recovery rate decaying by 50%/month", 
           "Double population at risk after 24th Feb", "Double population at risk after 2nd Mar"
           # "beta*2 after 24th Feb", "beta*2 after 2nd Mar"
           )
  
  dimnames(prvs)[[1]] <- dimnames(rts)[[1]] <- dimnames(incrs)[[1]] <- test[, 1]
  dimnames(prvs)[[2]] <- dimnames(rts)[[2]] <- dimnames(incrs)[[2]] <- selected
  dimnames(prvs)[[3]] <- dimnames(rts)[[3]] <- dimnames(incrs)[[3]] <- scs

  res <- list(
    Scenarios = scs,
    Prevalence = prvs,
    Rt = rts,
    Increase = incrs,
    SummaryPrv = summary_scenarios(prvs, scs, date0),
    SummaryIncr = summary_scenarios(incrs, scs, date0)
  )
  return(res)
}


aggregate_scenarios <- function(res_scs, date0 = as.Date("2020-02-12", "%Y-%m-%d")) {
  prvs <- array(0, dim(res_scs[[1]]$Prevalence))
  dimnames(prvs) <- dimnames(res_scs[[1]]$Prevalence)
  
  incrs <- array(0, dim(res_scs[[1]]$Prevalence))
  dimnames(incrs) <- dimnames(res_scs[[1]]$Prevalence)
  
  for (i in names(res_scs)) {
    prvs <- prvs + res_scs[[i]]$Prevalence
  }
  for (i in 1:dim(prvs)[2]) {
    incrs[, i, ] <- (prvs[, i, ] / prvs[, i, 1] - 1)  * 100
  }
  
  scs <- res_scs[[1]]$Scenarios
  
  res <- list(
    Scenarios = scs,
    Prevalence = prvs,
    Increase = incrs,
    SummaryPrv = summary_scenarios(prvs, scs, date0),
    SummaryIncr = summary_scenarios(incrs, scs, date0)
  )
  return(res)
}
