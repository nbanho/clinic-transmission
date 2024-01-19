#' Number of undiagnosed TB patients
#' 
#' @param n number of samples 
#' @param empirical sample based on empirical counts (default `True`) or population-level prevalence
#' @param counts number of counts of TB patients (0, 1, 2, ...) vising the clinic  
#' @param prev_est prevalence estimates in %, either a vector with mean and sd, or a ISO-3 char for the country (if estimates are available)
#' 
#' @details 
#' Distribution parametrized empirically based on the counts of occurrences of the
#' daily number of diagnosed TB patients visiting the clinic,
#' using a multinomial distribution, and
#' assuming that the number of undiagnosed TB patients is similar in number.
#' 
#' Population-level prevalence for South Africa taken from Moyo. 
#' 
#' @return if empirical, number of undiagnosed TB patients, otherwise proportion of undiagnosed TB patients
#' 
#' @references 
#' - Moyo: Moyo et al. (2022) in Lancet Infect Dis (doi.org/10.1016/S1473-3099(22)00149-9)

rundiag <- function(n, empirical = T, counts = c(6, 6, 7, 4, 5, 1), prev_est = "RSA") {
  if (empirical) {
    BM <- rmultinom(n, size = 1, prob = counts) 
    n_undiag <- apply(BM, 2, function(col) which(col == 1)) - 1
    return(n_undiag)
  } else {
    if (is.character(prev_est)) {
      if (prev_est == "RSA") {
        prev_mean <- 852 / 100000
        prev_sd <- (1026 - 679) / 100000 / (qnorm(.975) * 2)
      } else {
        stop("No prevalence estimates currently included for this country.")
      }
    } else {
      prev_mean <- prev_est[1]
      prev_sd <- prev_est[2]
    }
    p_undiag <- LaplacesDemon::rtrunc(n, "norm", a = 0, b = 1, mean = prev_mean, sd = prev_sd)
    return(p_undiag)
  }
}


#' Reduction in quanta emission rate through surgical mask wearing
#' 
#' @param n number of samples
#' 
#' @details 
#' Distribution approximated based on the reported mean and confidence interval
#' by Dharmadhikari for the reduction with face masks worn by patients with
#' multi-drug resistant TB.
#' 
#' @references 
#' - Dharmadhikari: Dharmadhikari et al. (2012) in Am J Respir Crit Care Med (doi.org/10.1164/rccm.201107-1190OC)

rrmask <- function(n) {
  LaplacesDemon::rtrunc(n, spec = "norm", a = 0, b = 1, mean = 0.56, sd = 0.11)
}


#' Inhalation rate
#' 
#' @param activity one of `c("resting", "sitting", "standing", "walking")`
#' @param sex one of `c("male", "female")`
#' 
#' @return inhalation rate in m3 per h
#' 
#' @details 
#' Walking corresponds to walking slowly at 2.5 mph (light activity).
#' 
#' @references 
#' - Adams: Adams (1993) [Report] Measurement of Breathing Rate and Volume in Routinely Performed Daily Activities.
#' 

IR <- function(activity, sex) {
  IR <- matrix(c(7.12, 8.98, 
                 7.72, 9.30,
                 8.36, 10.65,
                 20.32, 24.13),
               ncol = 2, byrow = T,
               dimnames = list(c("resting", "sitting", "standing", "walking"), 
                               c("male", "female")))
  IR <- IR / 2 / 1000 * 60
  return(IR[activity,sex])
}


#' Quanta emission rate
#' 
#' @param viral_load RNA viral or CFU bacillary load
#' @param cf conversion factor to quanta
#' @param activity physical activity (either waiting/sitting or walking)
#' @param breathing_weight proportion of breathing (other: speaking)
#' 
#' @details 
#' Mikszewski estimates the quanta generation rate distribution using 
#' the inhalation rates (IR) for resting, standing, and light activity (slow walking) 
#' from Adams (1993) and computes the droplet volume emission rate VER as the product
#' of the IR and the droplet volume concentration Vd, using the droplet 
#' emission rates from Stadnytskyi. Stadnytskyi estimates the transmission rates
#' only for speaking, and (I assume) Mikszewski scales them to other respiratory 
#' activities (breathing, loud speaking), using the multipliers derived 
#' from the estimates for Vd from Morawska as done in Buonanno. 
#' Accordingly, we also re-scale the VER reported by Mikszewski by multiplying them with 
#' IR ratios for different physical activity levels and weight the VER by the proportion
#' of breathing and speaking.  
#' 
#' @references 
#' - Mikszewski: Mikszewski et al. (2022) in Geoscience frontiers (doi.org/10.1016/j.gsf.2021.101285)
#' - Adams: Adams (1993) [Report] Measurement of Breathing Rate and Volume in Routinely Performed Daily Activities.
#' - Stadnytskyi: Stadnytskyi et al. (2020) in PNAS (doi.org/10.1073/pnas.2006874117)
#' - Morawska: Morawska et al. (2009) in Journal of Aerosol Science (doi.org/10.1016/j.jaerosci.2008.11.002)
#' - Buonanno: Buonanno et al. (2020) in Environment International (doi.org/10.1016/j.envint.2020.106112)
#' - Andrews: Andrews et al. (2014) in JID (doi.org/10.1093/infdis/jiu138)

rq <- function(viral_load, cf = 2.0e-3, activity = "waiting", breathing_weight = .8) {
  # inhalation rates
  IR <- c(c(7.12 + 8.98) / 2 / 1000 * 60,
          c(7.72 + 9.30) / 2 / 1000 * 60,
          c(8.36 + 10.65) / 2 / 1000 * 60,
          c(20.32 + 24.13) / 2 / 1000 * 60)
  names(IR) <- c("lying (resting)", "sitting", "standing", "walking slowly (light activity)")
  
  # droplet emission rates
  VER_M <- c(9.8e-4, 4.9e-3, 8.3e-2)
  names(VER_M) <- c("resting, oral breathing", "standing, speaking", "light activity, speaking loudly")
  
  # define 2 activity levels 
  if (activity == "waiting") {
    VER <- breathing_weight * IR["sitting"] / IR["lying (resting)"] * VER_M["resting, oral breathing"] + 
      (1-breathing_weight) * IR["sitting"] / IR["standing"] * VER_M["standing, speaking"]
  } else if (activity == "walking") {
    VER <- breathing_weight * IR["walking slowly (light activity)"] / IR["lying (resting)"] * VER_M["resting, oral breathing"] +
      (1-breathing_weight) * IR["walking slowly (light activity)"] / IR["standing"] * VER_M["standing, speaking"]
  } else {
    stop("Error: activity undefined")
  }
  names(VER) <- paste(activity, "with", 100*breathing_weight, "% breathing") 
  
  # compute quanta emission rate
  q <- viral_load * cf * VER
  
  return(q)
}


#' Viral load distribution
#' 
#' @param n number of samples
#' 

bact_load.mtb <- function(n) rlnorm(n, log(10^5.5), log(10^1.3)) # CFU mL-1

#' Viral inactivation rate
#' 
#' @param n number of samples
#' 
#' @details 
#' Based on a crude approximation to various estimates in the literature (see references).
#' 
#' @references 
#' - Loudon et al. (1969) in Am Rev Respir Dis (doi.org/10.1164/arrd.1969.100.2.165)
#' - Gannon et al. (2007) in Res J Vet Sci (doi.org/10.1016/j.rvsc.2006.07.011)
#' - Klein and Yang (2014) in Int J Mycobateriol (doi.org/10.1016/j.ijmyco.2014.04.002)
#' - Lever et al. (2000) in Lett Appl Microbiol (doi.org/10.1046/j.1365-2672.2000.00807.x)

rlambda <- function(n, disease = "TB") {
  rlnorm(n, meanlog = log(1), sdlog = 1)
}


#' Compute air change rate using transient mass balance method
#' 
#' @param data data frame with columns C1 (lead CO2), C (CO2), n (number of people), and V (volume)
#' @param G CO2 generation rate in L/min
#' @param dt time step of C and n in h
#' 
#' @details 
#' Based on Equation 16, page 7, in Batterman assuming CO2 generation rate based G on Persily.
#' 
#' @references 
#' - Batterman et al. (2017): https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5334699/
#' - Persily et al. (2007): https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5666301/
#' 

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
estimate_aer <- function(data, G = 0.004 * 60, dt = 1 / 60, aer_range = c(0.01, 10, 1000), Cr_range = c(300, 450, 600)) { 
  res <- optim(par = c(aer_range[2], Cr_range[2]), fn = min_rss, data = data, G = G, dt = dt, 
               lower = c(aer_range[1], Cr_range[1]), upper = c(aer_range[3], Cr_range[3]), method = "L-BFGS-B")
  return(list(res))
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