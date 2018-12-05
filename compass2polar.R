compass2polar <- function(theta){
  thetapol <- NULL
  idx <- theta >= 0 & theta < 90 & !is.na(theta)
  thetapol[idx] <- abs(theta[idx] - 90)
  
  idx2 <- theta >= 90 & theta <= 360 & !is.na(theta)
  thetapol[idx2] <- abs(450 - theta[idx2])
  
  idxna <- is.na(theta)
  thetapol[idxna] <- NA
  
  thetapol
}