#' Quanta emission rate
#' 
#' @param n number of samples
#' @param x quanta rate

rq <- function(n, disease = "TB") {
  LaplacesDemon::rtrunc(n, spec = "st", a = 0, b = 200, mu = 2, sigma = 2.5, nu = 1)
}

dq <- function(x) {
  LaplacesDemon::dtrunc(x, spec = "st", a = 0, b = 200, mu = 2, sigma = 2.5, nu = 1)
}

#' Viral inactivation rate
#' 

rlambda <- function(n, disease = "TB") {
  rgamma(1, shape = 2.25, rate = 1.5)
}

dlambda <- function(x, disease = "TB") {
  dgamma(x, shape = 2.25, rate = 1.5)
}


#' Unmasked TB Patients
#' 
#' @param n number of samples
#' @param mu expected number of unmasked TB patients
#' 

rTBunmasked <- function(n, lambda) {
  rpois(n, lambda)
}


#' Compute air change rate using transient mass balance method
#' 
#' @param data data frame with columns C1 (lead CO2), C (CO2), n (number of people), and V (volume)
#' @param G CO2 generation rate in L/min
#' @param dt time step of C and n in h

# estimate air change rate
#' based on Equation 16, page 7, Batterman et al. (2017): https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5334699/
#' assume CO2 generation rate based G on Persily et al. (2007): https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5666301/
#' --> use 0.004 L/s = 0.24 L/min
transient_mass_balance <- function(A, C, n, V, Cr, G, dt) {
  Q <- A * V
  C1hat <- 6 * 10^4 * n * G / Q * (1 - exp(- Q / V * dt)) + (C - Cr) * exp(- Q / V * dt) + Cr
  return(C1hat)
}

# minimize residual sum of squares
min_rss <- function(data, par, G, dt) {
  #' par[1] is the air change rate
  #' par[2] is the outdoor CO2 level
  with(data, sum( (C1 - transient_mass_balance(par[1], C, n, V, par[2], G, dt)) ^ 2))
}

# optimize 
estimate_aer <- function(data, G = 0.004 * 60, dt = 1 / 60) { 
  optim(par = c(4, 400), fn = min_rss, data = data, G = G, dt = dt, lower = c(0.01, 350), upper = c(100, 500), method = "L-BFGS-B")$par[1]
}

#' Compute air change rate with steady-state method
#' 
#' @param n number of people in steady state
#' @param G CO2 generation rate in L/min
#' @param V volume
#' @param Cs steady-state CO2
#' @param Cr outdoor CO2 level (default 400ppm)

steady_state_aer <- function(n, G, V, Cs, Cr = 400) {
  6 * 10^4 * n * G / (V * (Cs-Cr))
}