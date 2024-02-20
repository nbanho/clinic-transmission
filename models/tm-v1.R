#' Spatiotemporal model of the quanta concentration
#'
#' @description Function to compute the quanta concentration at each time point.
#'
#' @param time time vector in 1s
#' @param c0 2d-matrix with initial quanta concentration (in quanta per m3)
#' @param inf data frame with time, x- and y-coordinates, and generated quanta q
#' @param aer air changes per second
#' @param lambda viral inactivation rate per second
#' @param deposit deposition rate per second due to gravitational settling
#'
#' @details The default for parameters q, lambda, and k is based on prior research for Mtb.
#' The diffusion constant is computed relative to the air change rate, see Cheng (2011).
#'
#' @references Cheng KC, Acevedo-Bolton V, Jiang RT, Klepeis NE, Ott WR, Fringer OB, Hildemann LM.
#' Modeling exposure close to air pollution sources in naturally ventilated residences:
#' Association of turbulent diffusion coefficient with air change rate.
#' Environmental science & technology. 2011;45(9):4016-22.
#'
#' @return a list of matrices with the quanta concentration at each time step t_i
#'

tm <- function(time,
               c0,
               inf,
               aer = 3 / 3600,
               lambda = 1 / 3600,
               deposit = 0) {
  # room dimensions
  nr <- nrow(c0)
  nc <- ncol(c0)
  n_cells <- nr * nc

  # initialize
  nT <- length(time)
  time_int <- 1:nT
  ct <- replicate(nT + 2, c0)
  x_crds <- row.names(c0)
  y_crds <- colnames(c0)

  # add start and end without quanta generation
  if (nrow(inf) == 0 & sum(c0) == 0) {
    return(ct[, , -1])
  } else if (nrow(inf) == 0) {
    inf <- tibble(t = 1, data = list(data.frame(x = y_crds[1], y = x_crds[1], q = 0)))
    inf$te <- max(time_int) + 2
  } else {
    inf0 <- inf[1, ]
    inf0$t <- 0
    inf0$q <- 0
    inf <- rbind(inf0, inf)
    inf <- dplyr::arrange(inf, t)
    inf <- tidyr::nest(inf, data = c(x, y, q))
    inf$t <- inf$t + 1
    inf$te <- c(inf$t[-1], max(time_int) + 2)
  }
  inf$dt <- inf$te - inf$t

  # total consumption rate
  r <- aer + lambda + deposit

  for (k in 1:nrow(inf)) {
    # time point
    t_k <- inf$t[k]
    te_k <- inf$te[k]
    dt_k <- inf$dt[k]

    # previous quanta
    ct[, , t_k + 1] <- ct[, , t_k]

    # quanta generation
    for (i in 1:nrow(inf$data[[k]])) {
      ct[, , t_k + 1] <- ct[, , t_k + 1] + inf$data[[k]]$q[i] / n_cells
    }

    # removal
    ct[, , (t_k + 1):(te_k)] <- sapply(1:dt_k, function(dt_k_i) ct[, , t_k + 1] * exp(-r * dt_k_i), simplify = "array")
  }

  return(ct[, , -c(1, nT + 2)])
}
