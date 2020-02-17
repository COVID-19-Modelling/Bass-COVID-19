model_sir <- function() {
  for (i in 1:n_t){
    s[i] ~ dnorm(mu[i], tau)
    mu[i] <- 2 * (alpha1[i] * x1[i] + alpha2 * x2[i])

    alpha1[i] = (beta * (1 - A[i] / m) - nu) / 2
  }
  
  alpha2 = (- beta / m)
  tau ~ dgamma(1E-3, 1E-3)
  
  
  m ~ dunif(mx, mx * 10)
  beta ~ dunif(0, 1)
}

fit_sir <- function(pro, cases, n_iter = 1E4, r_rec = 0, r_death = 0) {
  dat <- (function(nc) {
    ni <- nc$I
    nim2 <- rev(rev(ni)[-(1:2)]) 
    nim0 <- ni[-(1:2)]
    nr <- nc$A[-c(1, length(ni))]
    
    list(
      n_t = length(ni) - 2,
      s = nim0 - nim2,
      x1 = nim0 + nim2,
      x2 = nim0 * nim2,
      A = nr,
      nu = r_rec + r_death,
      mx = max(ni)
    )
  })(cases)
  
  jags.inits <- function(){
    list(tau = 0.01, beta = 0.1, m = dat$mx)
  }
  
  fitted <- jags(data = dat, 
                 inits = jags.inits, 
                 parameters.to.save = c("mu", "beta", "m"),
                 n.iter = n_iter, 
                 model.file = model_sir)
  
  cases_hat <- (function() {
    mus <- fitted$BUGSoutput$sims.matrix[, paste0("mu[", 1:dat$n_t, "]")]
    mus <- t(mus)
    n_hat <- (mus + dat$x1) / 2
    return(n_hat)
  })()
  
  pars <- with(as.data.frame(fitted$BUGSoutput$sims.matrix), {
    data.frame(kappa = 0, beta = qexp(beta), m = m, deviance = deviance)
  })
  
  res <- list(
    ModelType = "SIR",
    Parameters = pars,
    Cases = cases,
    Cases_hat = cases_hat,
    Offsets = c(r_rec = r_rec, r_death = r_death),
    Summary = with(pars, { 
      list(
        Location = pro,
        Model = "SIR",
        Kappa = NA,
        Beta = stats_fn(beta),
        "Effective N (thousand)" = stats_fn(m / 1E3), 
        DIC = round(fitted$BUGSoutput$DIC),
        Deviance = stats_fn(deviance)) 
    }),
    DIC = fitted$BUGSoutput$DIC
  )
  class(res) <- "Fitted_SIR"
  
  return(res)
}
