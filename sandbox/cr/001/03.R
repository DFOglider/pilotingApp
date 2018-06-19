rm(list=ls())
library(oce)
library(geosphere)
source('readSeaExplorerRealTime.R')
source('../../../oxygenCalibrationCoefficients.R') # used to convert oxygen from units of Hz to ml/l
source('../../../swSatO2.R') # for use in sbeO2Hz2Sat.R
source('../../../sbeO2Hz2Sat.R') # calculate oxygen from Hz to ml/l from seaBird instrument

d <- readSeaExplorerRealTime('./data', 'SEA024', 'M25')
depth <- d$NAV$depth
Heading <- d$NAV$Heading
DesiredHeading <- d$NAV$DesiredHeading
AngCmd <- d$NAV$AngCmd
AngPos <- d$NAV$AngPos
roll <- d$NAV$Roll
pitch <- d$NAV$Pitch
dH <- Heading - DesiredHeading
dH[abs(dH) > 100] <- NA

if (!interactive()) png('03-%03d.png', width=7, height=7, res=150, units='in')

plot(dH, depth, pch='.', xlim=c(-100, 100), ylim=rev(range(depth)), xlab='Heading-DesiredHeading')
grid()
bm <- binMean1D(depth, dH, seq(0, max(depth, na.rm=TRUE), length.out=50))
bs <- binApply1D(depth, dH, seq(0, max(depth, na.rm=TRUE), length.out=50), sd, na.rm=TRUE)
lines(bm$result, bm$xmids, col=2, lwd=3)
lines(bm$result - bs$result, bm$xmids, col=2, lwd=2, lty=2)
lines(bm$result + bs$result, bm$xmids, col=2, lwd=2, lty=2)
abline(v=0, lwd=3, col='white')
abline(v=0, lwd=1)
title('SEA024 M25')

b <- binCount2D(dH, depth,
                seq(-100, 100, length.out=50),
                seq(0, max(depth, na.rm=TRUE), length.out=50))
col <- colorRampPalette(c('white', oceColorsJet(9)))
imagep(b$xmids, b$ymids, b$number, flipy=TRUE, col=col,
       xlab=expression(Delta*Heading),
       ylab='Depth')
lines(bm$result, bm$xmids, col=1, lwd=3)
title('SEA024 M25')

plot(dH, roll, pch='.', xlim=c(-100, 100), ylim=c(-20, 20), xlab='Heading-DesiredHeading')
grid()
title('SEA024 M25')

plot(dH, AngPos, pch='.', xlim=c(-100, 100), xlab='Heading-DesiredHeading')
grid()
title('SEA024 M25')

b <- binCount2D(dH, AngPos,
                seq(-100, 100, length.out=50),
                seq(-40, 40, length.out=50))
imagep(b$xmids, b$ymids, b$number, xlab=expression(Delta*Heading), ylab='AngPos',
       col=col)
title('SEA024 M25')

plot(AngCmd, AngPos, pch=19, cex=0.5)
grid()
abline(0, 1)
title('SEA024 M25')

plot(AngPos, roll, pch=19, cex=0.25, ylim=c(-20, 20))
grid()
title('SEA024 M25')

b <- binCount2D(AngPos, roll,
                seq(-40, 40, length.out=50),
                seq(-20, 20, length.out=50))
imagep(b$xmids, b$ymids, b$number, xlab='AngPos', ylab='roll',
       col=col)
title('SEA024 M25')

plot(roll, depth, pch='.', xlim=c(-20, 20), ylim=rev(range(depth, na.rm=TRUE)))
grid()
bm <- binMean1D(depth, roll, seq(0, max(depth, na.rm=TRUE), length.out=50))
bs <- binApply1D(depth, roll, seq(0, max(depth, na.rm=TRUE), length.out=50), sd, na.rm=TRUE)
lines(bm$result, bm$xmids, col=2, lwd=3)
lines(bm$result - bs$result, bm$xmids, col=2, lwd=2, lty=2)
lines(bm$result + bs$result, bm$xmids, col=2, lwd=2, lty=2)
abline(v=0, lwd=3, col='white')
abline(v=0, lwd=1)
title('SEA024 M25')

b <- binCount2D(roll, depth,
                seq(-20, 20, length.out=50),
                seq(0, max(depth, na.rm=TRUE), length.out=50))
imagep(b$xmids, b$ymids, b$number, flipy=TRUE, xlab='roll', ylab='depth',
       col=col)
abline(v=0, lwd=3, col='white')
abline(v=0, lwd=1)
title('SEA024 M25')

plot(AngPos, depth, pch='.', xlim=c(-45, 45), ylim=rev(range(depth, na.rm=TRUE)))
grid()
bm <- binMean1D(depth, AngPos, seq(0, max(depth, na.rm=TRUE), length.out=50))
bs <- binApply1D(depth, AngPos, seq(0, max(depth, na.rm=TRUE), length.out=50), sd, na.rm=TRUE)
lines(bm$result, bm$xmids, col=2, lwd=3)
lines(bm$result - bs$result, bm$xmids, col=2, lwd=2, lty=2)
lines(bm$result + bs$result, bm$xmids, col=2, lwd=2, lty=2)
abline(v=0, lwd=3, col='white')
abline(v=0, lwd=1)
title('SEA024 M25')

b <- binCount2D(AngPos, depth,
                seq(-40, 40, length.out=50),
                seq(0, max(depth, na.rm=TRUE), length.out=50))
imagep(b$xmids, b$ymids, b$number, flipy=TRUE, xlab='AngPos', ylab='depth',
       col=col)
abline(v=0, lwd=3, col='white')
abline(v=0, lwd=1)
lines(bm$result, bm$xmids, col=1, lwd=3)
title('SEA024 M25')

### This doesn't really work because density is sampled a lot less than
## density <- approx(PLDold$timesci, PLDold$SigTheta, NAVold$time)$y
## density[density < 20] <- NA
## plot(dH, density, pch='.', xlim=c(-100, 100), xlab='Heading-DesiredHeading')
## grid()
## bm <- binMean1D(density, dH, seq(min(density, na.rm=TRUE), max(density, na.rm=TRUE), length.out=50))
## bs <- binApply1D(density, dH, seq(min(density, na.rm=TRUE), max(density, na.rm=TRUE), length.out=50), sd, na.rm=TRUE)
## lines(bm$result, bm$xmids, col=2, lwd=3)
## lines(bm$result - bs$result, bm$xmids, col=2, lwd=2, lty=2)
## lines(bm$result + bs$result, bm$xmids, col=2, lwd=2, lty=2)
## abline(v=0, lwd=3, col='white')
## abline(v=0, lwd=1)

## b <- binCount2D(dH, density,
##                 seq(-100, 100, length.out=50),
##                 seq(min(density, na.rm=TRUE), max(density, na.rm=TRUE), length.out=50))
## imagep(b$xmids, b$ymids, b$number)
## lines(bm$result, bm$xmids, col=1, lwd=3)

## plot(density, depth)

if (!interactive()) dev.off()
