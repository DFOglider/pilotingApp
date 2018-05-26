rm(list=ls())
library(oce)

load('../../../data/SEA021/M33/data.rda')
depth <- NAVold$depth
Heading <- NAVold$Heading
DesiredHeading <- NAVold$DesiredHeading
dH <- Heading - DesiredHeading
dH[abs(dH) > 100] <- NA

if (!interactive()) png('01.png', width=7, height=7, res=150, units='in')

plot(dH, depth, pch='.', xlim=c(-100, 100), ylim=rev(range(depth)), xlab='Heading-DesiredHeading')
grid()
bm <- binMean1D(depth, dH, seq(0, max(depth, na.rm=TRUE), length.out=50))
bs <- binApply1D(depth, dH, seq(0, max(depth, na.rm=TRUE), length.out=50), sd, na.rm=TRUE)
lines(bm$result, bm$xmids, col=2, lwd=3)
lines(bm$result - bs$result, bm$xmids, col=2, lwd=2, lty=2)
lines(bm$result + bs$result, bm$xmids, col=2, lwd=2, lty=2)
abline(v=0, lwd=3, col='white')
abline(v=0, lwd=1)

if (!interactive()) dev.off()
