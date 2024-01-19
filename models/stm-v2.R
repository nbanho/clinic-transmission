#' Spatiotemporal model of the quanta concentration
#' 
#' @description Function to compute the quanta concentration at each time point. 
#' 
#' @param time time vector in 1s
#' @param c0 2d-matrix with initial quanta concentration (in quanta per m3)
#' @param inf data frame with time, x- and y-coordinates, and generated quanta q
#' @param cellLength length of a grid square cell (in m)
#' @param pd propagation distance of emitted quanta (in number of neighboring cells)
#' @param vol room volume (in m3)
#' @param aer air changes per second
#' @param lambda viral inactivation rate per second
#' @param deposit deposition rate per second due to gravitational settling
#' 
#' @details The diffusion constant is computed relative to the air change rate, see Cheng (2011).
#' 
#' @references Cheng KC, Acevedo-Bolton V, Jiang RT, Klepeis NE, Ott WR, Fringer OB, Hildemann LM. 
#' Modeling exposure close to air pollution sources in naturally ventilated residences: 
#' Association of turbulent diffusion coefficient with air change rate. 
#' Environmental science & technology. 2011;45(9):4016-22. 
#' 
#' @return a list of matrices with the quanta concentration at each time step t_i   
#' 

stm <- function(time,
                c0, 
                inf, 
                cellLength = 0.5,
                pd = 1,
                vol = 100,
                aer = 3/3600,
                lambda = 1/3600,
                deposit = 0) {
  
  # room dimensions
  nr <- nrow(c0)
  nc <- ncol(c0)
  
  # initialize 
  nT <- length(time)
  time_int <- 1:nT
  ct <- replicate(nT+2, c0)
  x_crds <- row.names(c0)
  y_crds <- colnames(c0)
  
  # add start and end without quanta generation
  if (nrow(inf) == 0 & sum(c0) == 0) {
    return(ct[,,-1])
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
  r <- exp(-(aer + lambda + deposit)) - 1
  
  # diffusion rate
  D <- (0.52 * aer + 8.61e-5) * vol^(2/3)  
  
  # diffusion model
  Diff2D <- function(t, y, parms) {
    CONC <- matrix(nrow = nr, ncol = nc, y)
    dCONC <- ReacTran::tran.2D(CONC, 
                               C.x.up = rep(0, nc), C.x.down = rep(0, nc), C.y.up = rep(0, nr), C.y.down = rep(0, nr),
                               flux.x.up = rep(0, nc), flux.x.down = rep(0, nc), flux.y.up = rep(0, nr), flux.y.down = rep(0, nr),
                               D.x = D, D.y = D, dx = cellLength, dy = cellLength)$dC + r * CONC
    return(list(dCONC))
  }
  
  
  for (k in 1:nrow(inf)) {
    
    # time point
    t_k <- inf$t[k]
    te_k <- inf$te[k] 
    dt_k <- inf$dt[k]
    
    # previous quanta
    ct[,,t_k+1] <- ct[,,t_k]
    
    for (i in 1:nrow(inf$data[[k]])) {
      
      # determine affected cells
      x_idx <- which(y_crds == inf$data[[k]]$x[i])
      y_idx <- which(x_crds == inf$data[[k]]$y[i])
      xl <- max(1,x_idx-pd) # x direction is columns
      xr <- min(nc,x_idx+pd) 
      yt <- max(1,y_idx-pd) # y direction is rows
      yb <- min(nr,y_idx+pd)
      
      # distribute quanta uniformly
      n_cells <- prod(dim(ct[yt:yb,xl:xr,t_k+1]))
      ct[yt:yb,xl:xr,t_k+1] <- ct[yt:yb,xl:xr,t_k+1] + inf$data[[k]]$q[i] / n_cells
      
    }
    
    # diffusion
    out <- deSolve::ode.2D(y = ct[,,t_k+1], t = 0:dt_k, func = Diff2D, parms = NULL, dim = c(nr,nc), names = c("room"), lrw = 160000)
    ct[,,(t_k+1):(te_k)] <- subset(out, select = "room", arr = T)[,,-1]
    
  }
  
  return(ct[,,-c(1,nT+2)])
  
}
