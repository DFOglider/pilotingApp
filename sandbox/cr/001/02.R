rm(list=ls())
library(oce)
library(deSolve)

## create a function to proscribe a heading "error" as a function of depth
d <- 0:250
headingError <- function(d, dc=70, a=-1, b=10) {
    n <- length(d)
    h <- rep(NA, n)
    for (i in 1:n) {
        if (d[i] < dc) {
            h[i] <- a*d[i] + b
        } else {
            h[i] <- -a*d[i] + (-2*abs(a)*dc + b)
            if (h[i] > 0) h[i] <- 0
        }
    }
    return(h)
}

plot(headingError(d), d, ylim=rev(range(d)), type='l', xlab='Heading Error')
grid()

depth <- function(t, vz=0.15, maxd=100) {
    period <- 2*maxd/vz
    2*maxd*abs(t/period - floor(t/period + 1/2))
}

dt <- 20
t <- seq(0, 86400, dt)
vx <- vy <- NULL
v <- 0.3
maxd <- 100
for (i in seq_along(t)) {
    vx[i] <- v*cos(headingError(depth(t[i], maxd=maxd))*pi/180)
    vy[i] <- v*sin(headingError(depth(t[i], maxd=maxd))*pi/180)
}
x <- cumsum(vx)*dt
y <- cumsum(vy-mean(vy))*dt
xt <- cumsum(rep(v, length(t)))*dt
yt <- cumsum(rep(0, length(t)))*dt

plot(xt, yt, type='l', ylim=c(-100, 100))
lines(x, y, col=2)
title(paste('DeltaX =', (max(xt)-max(x))/1000, 'km'), line=0.25)
title(paste('Max Depth =', maxd, 'meters, over', max(t)/3600, 'hours'), line=1.25)
