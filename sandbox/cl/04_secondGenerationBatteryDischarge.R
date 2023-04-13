rm(list=ls())
library(digitize)

jpg <- 'secondGenerationBatteryDischarge.png'

d <- digitize(jpg)

save(d, file = 'secondGenerationBatteryDischarge.rda')

plot(d[['x']], d[['y']],
     xlim = c(100, 0))
