# choose where to save downloaded files from ftp site
# it is suggested that data be saved locally in /data
# data will be saved to /data/gliderName/mission
# e.g /data/SEA019/M28
datadir <- "./data"

# url for glider ftp site
# directory structure as of 2018-03-14: /realData/gliderNames/MissionNumbers/dataFiles
# e.g /realData/SEA019/M28/sea019.28.gli.sub.2.gz
url <- 'ftp://ftp.dfo-mpo.gc.ca/glider'
dirs <- getURL(paste(url,'', sep ="/"), ftp.use.epsv = FALSE, dirlistonly = TRUE)
dirnamess <- strsplit(dirs, "\r*\n")[[1]]

okdir <- which(dirnamess == 'realData')

dirnames <- dirnamess[okdir]
#get directories for gliders
gliderdirs <- getURL(paste(url, 
                           dirnames,
                           '', sep ="/"),
                     ftp.use.epsv = FALSE, dirlistonly = TRUE)
gliderdirnames <- strsplit(gliderdirs, "\r*\n")[[1]]
gdnok <- grepl(pattern = 'SEA0[0-9][0-9]', x = gliderdirnames) #find glider directories
gliderdirnames <- gliderdirnames[gdnok]

# directories for gliders offline
gliderdirsoffline <- list.dirs(path = datadir,
                        full.names = FALSE,
                        recursive = FALSE)
gdnook <- nchar(gliderdirsoffline) > 0
gliderdirnamesoffline <- gliderdirnames[gdnook]

getMissions <- function(glider){
  missiondirs <-  getURL(paste(url, 
                               dirnames, 
                               glider,
                               '', sep ="/"), 
                         ftp.use.epsv = FALSE, dirlistonly = TRUE)
  missiondirnames <- strsplit(missiondirs, "\r*\n")[[1]]
  
  missiondirnames[grepl(pattern = "M[0-9][0-9]", x = missiondirnames)]
}

getMissionsOffline <- function(glider){
  missiondirs <- list.dirs(path = paste(datadir,
                                        glider, sep = '/'),
                           full.names = FALSE)
  okdirs <- nchar(missiondirs) > 0
  missiondirnames <- missiondirs[okdirs]
}

downloadData <- function(datadir, glider, mission){
  savedir <- paste(datadir, glider, mission,'', sep='/')
  # check if savedir exists, if not, create it
  if(!dir.exists(savedir)){
    dir.create(savedir, recursive = TRUE)
  }
  # get existing files
  existing_files <- list.files(path = savedir)
  # get files for the glider and mission from ftp
  filepath <- paste(url, 
                    dirnames, 
                    glider, 
                    mission, 
                    '', 
                    sep = '/')
  files <- getURL(url = filepath,
                  ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- strsplit(files, "\r*\n")[[1]]
  f <- filenames[grep(pattern = '*.sub.*.gz' , x = filenames)] #nav and pld
  # find which files to download
  files_to_get <- f[!(f %in% existing_files)]
  # download nav and pld files
  if(length(files_to_get) != 0){
    for (file in files_to_get){
      download.file(url = paste(url,
                                dirnames,
                                glider,
                                mission,
                                file,
                                sep = '/'),
                    destfile = paste(savedir, 
                                     file,
                                     sep=''))
    }
  }
  
  kml <- filenames[grep(pattern = '*.trk.kml', x = filenames)]
  # download kml file, downloads everytime
  download.file(url = paste(url,
                            dirnames,
                            glider,
                            mission,
                            kml,
                            sep='/'),
                destfile = paste(savedir,
                                 kml,
                                 sep=''))
}