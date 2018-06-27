rm(list=ls())
library(XML)
# function from readKML package, not avaliable for R version 3.4.1
# the package was downloaded from source and the function
# was copied over here. The package appears to no long
# be maintained
source('readGPX.R')
files <- list.files(pattern = '*.gpx')

d <- readGPX(gpx.file = files[1])
d1A <- vector(mode = 'logical')
d1A$lon <- d$routes$hfxShipping1A$lon
d1A$lat <- d$routes$hfxShipping1A$lat

d <- readGPX(gpx.file = files[2])
d1A1Bzone <- vector(mode = 'logical')
d1A1Bzone$lon <- d$routes$hfxShipping1A1Bzone$lon
d1A1Bzone$lat <- d$routes$hfxShipping1A1Bzone$lat

d <- readGPX(gpx.file = files[3])
d1B1Czone <- vector(mode = 'logical')
d1B1Czone$lon <- d$routes$hfxShipping1B1Czone$lon
d1B1Czone$lat <- d$routes$hfxShipping1B1Czone$lat

d <- readGPX(gpx.file = files[4])
d1C1B <- vector(mode = 'logical')
d1C1B$lon <- d$routes$hfxShipping1C1B$lon
d1C1B$lat <- d$routes$hfxShipping1C1B$lat

d <- readGPX(gpx.file = files[5])
d1E <- vector(mode = 'logical')
d1E$lon <- d$routes$hfxShipping1E$lon
d1E$lat <- d$routes$hfxShipping1E$lat

d <- readGPX(gpx.file = files[6])
d1E1Fzone <- vector(mode = 'logical')
d1E1Fzone$lon <- d$routes$hfxShipping1E1Fzone$lon
d1E1Fzone$lat <- d$routes$hfxShipping1E1Fzone$lat

d <- readGPX(gpx.file = files[7])
d1F <- vector(mode = 'logical')
d1F$lon <- d$routes$hfxShipping1F$lon
d1F$lat <- d$routes$hfxShipping1F$lat

d <- readGPX(gpx.file = files[8])
d2C2Dzone <- vector(mode = 'logical')
d2C2Dzone$lon <- d$routes$hfxShipping2C2Dzone$lon
d2C2Dzone$lat <- d$routes$hfxShipping2C2Dzone$lat

d <- readGPX(gpx.file = files[9])
d3C3Dzone <- vector(mode = 'logical')
d3C3Dzone$lon <- d$routes$hfxShipping3C3Dzone$lon
d3C3Dzone$lat <- d$routes$hfxShipping3C3Dzone$lat

save(d1A, d1A1Bzone, d1C1B, d1B1Czone, d1E, d1E1Fzone, d1F, d3C3Dzone, d2C2Dzone,
     file = 'shippingBoundaries.rda')