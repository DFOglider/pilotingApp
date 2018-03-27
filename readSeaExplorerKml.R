readSeaExplorerKml <- function(datadir, glider, mission){
  dir <- paste(datadir,
               glider,
               mission,
               '',
               sep = '/')
  file <- paste(dir, as.list(list.files(path = dir, pattern = '*.trk.kml')), sep = '')
  d <- xmlParse(file)
  dx <- xmlToList(d)
  dxd <- dx$Document

  coord <- dxd[[4]]$LineString$coordinates
  coordinates <- strsplit(coord, '\n')[[1]]
  position <- strsplit(coordinates, ',')

  lon <- as.numeric(unlist(lapply(position, function(k) k[1])))
  lat <- as.numeric(unlist(lapply(position, function(k) k[2])))
  
  good <- !(lon == 0 & lat == 0) #remove 0,0 coordinates
  
  lat <- lat[good]
  lon <- lon[good]
  
  good2 <- !(is.na(lon) & is.na(lat))
  
  lat <- lat[good2]
  lon <- lon[good2]
  
  invisible(list(lat = lat, lon = lon))
}
