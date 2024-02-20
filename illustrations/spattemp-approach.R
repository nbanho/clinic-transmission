euclidean <- function(x1, x2, y1, y2) {
  sqrt( (x1-x2)^2 + (y1-y2)^2 )
}

# Setup room
gridCellLength <- 0.25
nx <- 40
ny <- 40
dy <- gridCellLength * ny
dx <- gridCellLength * nx
c0 <- matrix(0, nrow = ny, ncol = nx, dimnames = list(as.character(1:ny), as.character(1:nx)))

# mixed airspace
conc <- function(d) {
  exp(-d/10)
}

c1 <- c0
c1[,] <- 1
c1[10,10] <- 2.5
c1[30,30] <- 3
for (x in 1:nrow(c1)) {
  for (y in 1:ncol(c1)) {
    c1[x,y] <- c1[10,10] * conc(euclidean(x, 10, y, 10)) + c1[30,30] * conc(euclidean(x, 30, y, 30))
  }
}
png(filename = "illustrations/mixed-space.png")
image(c1, zlim = c(min(c1), max(c1)))
dev.off()



# well-mixed airspace
c2 <- c0
c2[,] = 1
png(filename = "illustrations/well-mixed-space.png")
image(c2, zlim = c(min(c1), max(c1)))
dev.off()
