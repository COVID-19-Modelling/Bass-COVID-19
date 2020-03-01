zero_kappa <- function(pars) {
  pars$kappa <- rep(0, length(pars$kappa))
  return(pars)
}


gen_rec_decay <- function(dis = 0.2) {
  rr <- - log(1 - dis) / 30
  function(pars) {
    pars$r_rec <- pars$r_rec * exp(-rr * (pars$tt - pars$tt[1]))
    return(pars)
  }
}


gen_bost_m <- function(at = 12, by = 2) {
  function(pars) {
    pars$m <- ifelse(pars$tt >= at, pars$m * by, pars$m)
    return(pars)
  }
}


gen_bost_beta <- function(at = 12, by = 2) {
  function(pars) {
    pars$beta <- ifelse(pars$tt >= at, pars$beta * by, pars$beta)
    return(pars)
  }
}
