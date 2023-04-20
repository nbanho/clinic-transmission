#' Compute quanta removal
#' 
#' @param C N x M matrix of quanta concentration per grid cell
#' @param aer air exchange rate per hour
#' @param k rate per hour for particle deposition on surfaces
#' @param lambda viral inactivation rate per hour
#' @param dt time step (in s)
#' 

removal <- function(C, aer, k = 0, lambda = 0, dt = 1) {
  
  # quanta removal rate
  rr <- aer + k + lambda
  
  # removal
  qr <- function(x) x * exp(- rr * dt)
  
  # quanta concentration after removal
  C_rq <- apply(C, c(1,2), qr)
  
  return(C_rq)
}


#' Compute quanta emission
#' 
#' @param C N x M matrix of quanta concentration per grid cell
#' @param W N x M matrix with weights for quanta emission (in m)
#' @param qer quanta emission rate (in quanta/s)
#' @param dt time step (in s)
#' 

emission <- function(C, W, qer, dt = 1) {
  
  # emitted quanta 
  eq <- qer * dt
  
  # quanta concentration after emission
  C_eq <- C + W * eq
  
  return(C_eq)
}


#' Compute air exchange rate per hour
#' 
#' @param G CO2 generation rate in L/s per person
#' @param Co CO2 concentration in outdoor air (in ppm)
#' @param C CO2 concentration (in ppm)
#' @param V volume of ventilated space (in m3)
#' @param n number of people in ventilated space (in 1/m3) 
#' 

compute_aer <- function(G, Co, C, n, V) {
  # add 1 person for the person working behind the registry (and such that n is not 0)
  n <- n + 1
  # excess CO2 in ppm
  Ce <- C - Co
  # TODO: is there are more elegant way to ensure that the denominator is not 0?
  Ce <- ifelse(Ce < 1, 1, Ce) 
  # excess CO2 in l/m3 (1l/m3 = 1,000ppm)
  Ce <- Ce / 1000
  # ventilation rate (in l/s per person)
  Q <- G / Ce
  # air exchange rate (in s)
  aer <- Q * n / V
  # air exchange rate (in h)
  aer <- 3600 * aer
  return( aer )
}

#' Compute risk of infection
#' 
#' @param C quanta concentration (quanta / m3)
#' @param t duration of exposure (in seconds)
#' @param p is the breathing rate (in L/seconds)
#' 

riskOfInfection <- function(C, t = 1, p = 8.0 / 60) {
  1 - exp(-C * t * p)
} 


#' Relative viral concentration by distance to infector
#' 
#' @param distance distance in m
#' @param mu mean of the exponential decaying distribution
#' 

dIc <- function(distance, mu) { 
  1 - pexp(distance, 1 / mu)
}

#' Mean parameter of exponentially decaying distribution for the relative viral concentration by distance to infector
#' 
#' @param n number of samples

rMuIc <- function(n) {
  rgamma(n, shape = 10, scale = .2)
}


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


#' Average volume of exhaled gas
#' 
#' @param n number of samples
#' 


rV <- function(n) {
  runif(n, 0.1, 0.17)
}


#' CO2 generation rate
#' 
#' @param n number of samples
#' 

rG <- function(n) {
  runif(n, 0.003, 0.005)
}


#' Unmasked TB Patients
#' 
#' @param n number of samples
#' @param mu expected number of unmasked TB patients
#' 

rTBunmasked <- function(n, lambda) {
  rpois(n, lambda)
}


#' Spatial diffusion of quanta
#' 
#' @param C N x M matrix of quanta concentration per grid cell
#' @param dx the size of the grid cell in x direction (in m)
#' @param dy the size of the grid cell in y direction (in m)
#' @param Lx the size of the room in x direction (in m)
#' @param Ly the size of the room in y direction (in m)
#' @param D diffusion constant (in m2/s)
#' @param dt time step (in s)
#' 

diffusion <- function(C, dx, dy, Lx, Ly, D, dt = 1) {
  
  # parameters
  nx <- ncol(C)
  ny <- nrow(C)
  
  # Compute the change in quanta due to diffusion
  #' C.y and C.x are the boundary conditions
  #' flux ensures mass consistency
  dC <- ReacTran::tran.2D(C, C.x.up = rep(0, nx), C.x.down = rep(0, nx),
                          C.y.up = rep(0, ny), C.y.down = rep(0, ny),
                          flux.x.up = rep(0, nx), flux.x.down = rep(0, nx),
                          flux.y.up = rep(0, ny), flux.y.down = rep(0, ny),
                          D.x = D, D.y = D, dx = dx, dy = dy)$dC
  
  # Update the concentration using the Euler method
  C_diff <- C + dC
  
  return(C_diff)
}
