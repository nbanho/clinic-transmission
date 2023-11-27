#' Spatiotemporal model of the quanta concentration
#' 
#' @description Function to compute the quanta concentration at each time point. 
#' 
#' @param c0 matrix with initial quanta concentration
#' @param aer data frame with columns time and aer (air change rate in air changes per hour)
#' @param inf data frame with columns time, x and y indicating the cell where the infectious person is located
#' @param roomDim room dimension vector (length x, length y, height)
#' @param cellLength length of a grid square cell (in m3)
#' @param pd propagation distance of emitted quanta (in number of neighboring cells)
#' @param q random generation distribution function for the emitted quanta (quanta per hour)
#' @param lambda random generation distribution function for the viral inactivation rate (per hour)
#' @param k random generation distribution function for the deposition rate due to gravitational settling (per hour)
#' @param seed seed number
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

stm <- function(c0, 
                aer, 
                inf, 
                roomDim,
                cellLength = 0.5,
                pd = 1,
                q = 10, 
                lambda = 0.5,
                k = 0,
                seed = 12345) {
  
  # room dimensions
  vol <- prod(roomDim)
  nr <- nrow(c0)
  nc <- ncol(c0)
  
  # determine the time step
  dt <- as.numeric(diff(aer$time, units = "sec"))
  
  # initialize 
  ct <- list()
  ct[[1]] <- c0
  set.seed(seed)
  
  for (t in 1:length(dt)) {
    
    # previous quanta
    ct[[t+1]] <- ct[[t]]
    
    # quanta emission
    inf_t <- dplyr::filter(inf, time == aer$time[t+1])
    if (nrow(inf_t) > 0) {
      for (i in 1:nrow(inf_t)) {
        # determine affected cells
        xl <- max(1,inf_t$x[i]-pd) # x direction is columns
        xr <- min(nc,inf_t$x[i]+pd) 
        yt <- max(1,inf_t$y[i]-pd) # y direction is rows
        yb <- min(nr,inf_t$y[i]+pd)
        
        # distribute quanta uniformly
        quanta_per_sec <- q / 3600
        n_cells <- prod(dim(ct[[t+1]][yt:yb,xl:xr]))
        ct[[t+1]][yt:yb,xl:xr] <- ct[[t+1]][yt:yb,xl:xr] + quanta_per_sec * dt[t] / n_cells
      }
    }
    
    # diffusion
    aer_per_sec <- aer$aer[t+1] / 3600
    #' diffusion constant relationship to aer, see Eq. (8) here: https://doi.org/10.1016/j.buildenv.2019.106591
    D <- (0.52 * aer_per_sec + 8.61e-5) * vol^(2/3)  
    #' Euler method with 5-point stencil
    #' C.y and C.x are the boundary conditions
    #' flux ensures mass consistency
    for (delta in 1:dt[t]) {
      dc <- ReacTran::tran.2D(ct[[t+1]], C.x.up = rep(0, nc), C.x.down = rep(0, nc),
                              C.y.up = rep(0, nr), C.y.down = rep(0, nr),
                              flux.x.up = rep(0, nc), flux.x.down = rep(0, nc),
                              flux.y.up = rep(0, nr), flux.y.down = rep(0, nr),
                              D.x = D, D.y = D, dx = cellLength, dy = cellLength)$dC
      ct[[t+1]] <- ct[[t+1]] + dc
    }
    
    # removal
    lambda_per_sec <- lambda / 3600
    k_per_sec <- k / 3600
    ct[[t+1]] <- ct[[t+1]] * exp(-(aer_per_sec+lambda_per_sec+k_per_sec) * dt[t])
  }
  
  return(ct)
  
}
