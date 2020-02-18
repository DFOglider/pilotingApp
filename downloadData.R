# choose where to save downloaded files from ftp site
# it is suggested that data be saved locally in /data
# data will be saved to /data/gliderName/mission
# e.g /data/SEA019/M28
datadir <- "./data"

# url for glider ftp site
# directory structure as of 2018-03-14: /realData/gliderNames/MissionNumbers/dataFiles
# e.g /realData/SEA019/M28/sea019.28.gli.sub.2.gz
#url <- 'ftp://ftp.dfo-mpo.gc.ca/glider'
#url <- 'ftp://dfoftp.ocean.dal.ca/pub/dfo/glider'

getGliderNames <- function(ftpUrl){
  dirs <- getURL(paste(ftpUrl,'', sep ="/"), ftp.use.epsv = FALSE, dirlistonly = TRUE)
  dirnamess <- strsplit(dirs, "\r*\n")[[1]]
  okdir <- grepl('^SEA\\w+$', dirnamess) # just in case something else gets put there
  dirnames <- dirnamess[okdir]
  dirnames
  # 
  # #get directories for gliders
  # gliderdirs <- getURL(paste(ftpUrl, 
  #                           dirnames,
  #                           '', sep ="/"),
  #                     ftp.use.epsv = FALSE, dirlistonly = TRUE)
  # gliderdirnames <- strsplit(gliderdirs, "\r*\n")[[1]]
  # gdnok <- grepl(pattern = 'SEA0[0-9][0-9]', x = gliderdirnames) #find glider directories
  # gliderdirnames <- gliderdirnames[gdnok]
  # gliderdirnames
}



getMissions <- function(ftpUrl, glider){
  missiondirs <-  getURL(paste(ftpUrl,
                               glider,
                               '', sep ="/"), 
                         ftp.use.epsv = FALSE, dirlistonly = TRUE)
  missiondirnames <- strsplit(missiondirs, "\r*\n")[[1]]
  missiondirnames
  #missiondirnames[grepl(pattern = "^M[0-9][0-9]$", x = missiondirnames)]
}

downloadData <- function(ftpUrl, datadir, glider, mission){
  # dirs <- getURL(paste(ftpUrl,'', sep ="/"), ftp.use.epsv = FALSE, dirlistonly = TRUE)
  # dirnamess <- strsplit(dirs, "\r*\n")[[1]]
  # okdir <- which(dirnamess == 'realData')
  # dirnames <- dirnamess[okdir]
  savedir <- paste(datadir, glider, mission,'', sep='/')
  # check if savedir exists, if not, create it
  if(!dir.exists(savedir)){
    dir.create(savedir, recursive = TRUE)
  }
  # get existing files
  existing_files <- list.files(path = savedir)
  # get files for the glider and mission from ftp
  filepath <- paste(ftpUrl, 
                    #dirnames, 
                    glider, 
                    mission,
                    'C-Csv',
                    '', 
                    sep = '/')
  files <- getURL(url = filepath,
                  ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames <- strsplit(files, "\r*\n")[[1]]
  f <- filenames[grep(pattern = '^\\w+\\.\\w+\\.\\w+\\.sub.\\w+$' , x = filenames)] # nav and pld, new grep should be OK but has potential to be wrong
  # find which files to download
  files_to_get <- f[!(f %in% existing_files)]
  # download nav and pld files
  if(length(files_to_get) != 0){
    for (file in files_to_get){
      download.file(url = paste(ftpUrl,
                                #dirnames,
                                glider,
                                mission,
                                'C-Csv',
                                file,
                                sep = '/'),
                    destfile = paste(savedir, 
                                     file,
                                     sep=''))
    }
  }
  kmlpath <-  paste(ftpUrl, 
                    #dirnames, 
                    glider, 
                    mission,
                    'F-Geo',
                    '', 
                    sep = '/')
  kmlfiles <- getURL(url = kmlpath,
                     ftp.use.epsv = FALSE, dirlistonly = TRUE)
  kmlfilenames <- strsplit(kmlfiles, "\r*\n")[[1]]
  kml <- kmlfilenames[grep(pattern = '^\\w+\\.\\w+\\.gps\\.all\\.csv$', x = kmlfilenames)]
  # download kml file, downloads everytime if there
  if(length(kml) != 0){
  download.file(url = paste0(kmlpath, kml),
                destfile = paste(savedir,
                                 kml,
                                 sep=''))
  }
  # # msn file, in directory above data files
  # msavedir <- paste(datadir, glider,'', sep='/')
  # msnpath <- paste(ftpUrl, 
  #                   dirnames, 
  #                   glider, 
  #                   '', 
  #                   sep = '/')
  # mfiles <- getURL(url = msnpath,
  #                 ftp.use.epsv = FALSE, dirlistonly = TRUE)
  # mfilenames <- strsplit(mfiles, "\r*\n")[[1]]
  # msn <- mfilenames[grep(pattern = paste0(glider,mission,'.msn'), x = mfilenames)]
  # if(length(msn) != 0){
  #   download.file(url = paste(ftpUrl,
  #                             dirnames,
  #                             glider,
  #                             msn,
  #                             sep = '/'),
  #                 destfile = paste(msavedir,
  #                                  msn,
  #                                  sep = ''))
  # }
}
