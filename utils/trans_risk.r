#' Compute the quanta removal rate
#' 
#' 
#' @param AER air exchange rate per hour
#' @param k rate per hour for particle deposition on surfaces
#' @param lambda viral inactivation rate per hour
#' 

compute_RR <- function(AER, k = 0, lambda = 0) {
  return(AER + k + lambda)
}


#' Compute air exchange rate per hour
#' 
#' @param G CO2 generation rate in L/s per person
#' @param Co CO2 concentration in outdoor air (in ppm)
#' @param C CO2 concentration (in ppm)
#' @param V volume of ventilated space (in m3)
#' @param n number of people in ventilated space (in 1/m3) 
#' 

compute_AER <- function(G, Co, C, n, V) {
  # add 1 person for the person working behind the registry (and such that n is not 0)
  n <- n + 1
  # excess CO2 in ppm
  Ce <- C - Co
  # TODO: is there are more elegant way to ensure that the denominator is not 0?
  Ce <- ifelse(Ce < 0, 1, Ce) 
  # excess CO2 in l/m3 (1l/m3 = 1,000ppm)
  Ce <- Ce / 1000
  # ventilation rate (in l/s per person)
  Q <- G / Ce
  # air exchange rate (in s)
  AER <- Q * n / V
  # air exchange rate (in h)
  AER <- 3600 * AER
  return( AER )
}


#' Compute quanta concentration (quanta / m3)
#' 
#' 
#' @param E quanta emission (quanta emission rate times the number of infectious)
#' @param I number of infectious people in space
#' @param RR quanta removal rate
#' @param N0 initial quanta concentration
#' @param V volume of space (in 1/m3) 
#' 
#' @references Buonanno G, Stabile L, Morawska L. Estimation of airborne viral emission:
#' Quanta emission rate of SARS-CoV-2 for infection risk assessment. 
#' Environment international. 2020;141:105794.

compute_Nt <- function(E, RR, N0, V) {
  return( E / (RR * V) + (N0 + E / (RR * V)) * exp(-RR) )
}


#' Compute risk of infection
#' 
#' 
#' @param N quanta concentration (quanta / m3)
#' @param t duration of exposure (in seconds)
#' @param p is the breathing rate (in L/seconds)
#' 

compute_P <- function(N, t = 1, p = 8.0 / 60) {
  1 - exp(-N * t * p)
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