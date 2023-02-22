lnorm_moments <- function(meanlog, sdlog) {
  m <- exp(meanlog + (1/2)*sdlog^2)
  s <- exp(meanlog + (1/2)*sdlog^2)*sqrt(exp(sdlog^2) - 1)
  return(list(mean = m, sd = s))
}


inv_lnorm_moments <- function(mean, sd) {
  m <- log(mean) - 0.5 * log((sd/mean)^2 + 1)
  s <- sqrt(log((sd/mean)^2 + 1))
  return(list(meanlog = m, sdlog = s))
}
