fm_bass <- function() {
  for (i in 1:n_t){
    s[i] ~ dnorm(mu[i], tau)
    mu[i] <- 2 * (alpha0[i] + alpha1[i] * x1[i] + alpha2 * x2[i])
    
    alpha0[i] = kappa * (m - A[i])
    alpha1[i] = (beta * (1 - A[i] / m) - kappa - nu) / 2
  }
  
  alpha2 = (- beta / m)
  tau ~ dgamma(1E-3, 1E-3)
  
  
  m ~ dunif(mx, mx * 10)
  kappa ~ dunif(0, 1)
  beta ~ dunif(0, 1)
}


sm_bass <- odin::odin({
  deriv(S) <- - foi * S
  deriv(I) <- foi * S - dropout * I
  
  initial(S) <- m - I0 - A0
  initial(I) <- I0
  
  foi <- beta * (I / m)  + kappa
  
  m <- user(1000)
  I0 <- user(5)
  A0 <- user(5)
  
  kappa <- user(0.01)
  beta <- user(0.3)
  dropout <- user(0)
})



validate_bass <- function(pro, cases, n_iter = 1E4, r_rec = 0, r_death = 0, n_sim = 500, n_valid = 5) {
  nr <- length(cases$I)
  
  cases_fit <- list(
    I = cases$I[1:(nr - n_valid)],
    A = cases$A[1:(nr - n_valid)]
  )
  
  cases_valid <- list(
    I = cases$I[(nr - n_valid):nr],
    A = cases$A[(nr - n_valid):nr]
  )
  
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
      mx = max(nc$I + nc$A)
    )
  })(cases_fit)
  
  jags.inits <- function(){
    list(tau = 0.01, kappa = 0.01, beta = 0.1, m = dat$mx)
  }
  
  fitted <- jags(data = dat, 
                 inits = jags.inits, 
                 parameters.to.save = c("mu", "kappa", "beta", "m"),
                 n.iter = n_iter, 
                 model.file = fm_bass)
  
  cases_hat <- (function() {
    mus <- fitted$BUGSoutput$sims.matrix[, paste0("mu[", dat$n_t, "]")]
    n_hat <- (mus + dat$x1[dat$n_t]) / 2
    return(n_hat)
  })()
  
  pars <- with(as.data.frame(fitted$BUGSoutput$sims.matrix), {
    data.frame(kappa = qexp(kappa), beta = qexp(beta), m = m)
  })
  
  get_bass_pars <- function(i) {
    xs <- pars[i, ]
    cases <- cases_hat[i]
    
    list(
      kappa = xs$kappa,
      beta = xs$beta,
      dropout = r_death + r_rec,
      m = xs$m,
      I0 = cases_hat[i],
      A0 = cases_valid$A[1]
    )
  }
  
  cm_bass <- sm_bass()
  times <- seq(0, n_valid, by = 0.5)
  
  ii <- matrix(0, n_valid + 1, n_sim)
  
  for (i in 1:n_sim) {
    cm_bass$set_user(user = get_bass_pars(i))
    ys <- cm_bass$run(times)
    ii[, i] <- ys[times == round(times), "I"]
  }
  
  tss <- data.table(Date = as.Date("20200212", "%Y%m%d") - n_valid:0, 
                    Loc = pro,
                    mean = 100 * apply(ii / cases_valid$I - 1, 1, mean),
                    lower = 100 * apply(ii / cases_valid$I - 1, 1, quantile, p = 0.025), 
                    upper = 100 * apply(ii / cases_valid$I - 1, 1, quantile, p = 0.975))

  return(tss)
}


visualise_ts_error <- function(vld) {
  g_vld <- ggplot(vld, aes(x = Date)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_line(aes(y = mean)) +
    scale_y_continuous("Forecasting errors (%) of active cases", limits = c(-100, 100)) + 
    facet_wrap(Loc~., ncol = 5) +
    scale_x_date("Date", date_labels = "%e %b %Y") +
    scale_color_discrete("") +
    labs(title = "") +
    theme_bw() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 60, hjust = 1), 
          text = element_text(family = "serif"))
  
  return(g_vld)
}
