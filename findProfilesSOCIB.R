#' @title Profile detection
#' 
#' @details Profile detection for gliders using the MATLAB SOCIB processing 
#' toolbox method
#' 
#' @param time time vector of length n
#' @param pressure pressure vector of length n
#' @param stall_length maximum range of a stalled segment, in the same units as 
#' pressure. Only intervals of constant vertical direction spanning a depth range 
#' not less than the given value are considered valid cast segments. Shorter 
#' intervals are considered stalled segements inside or between casts. Default 
#' value of 0 indicates that all segments are valid cast segments
#' @param shake_length maximum duration of a shake segments, in the same unit as
#' time. Only intervals of constatn certical direction with duration not less than
#' the given value are considered valid cast segments. Briefer intervals are
#' considered shake segments inside or between casts. Default value of 0 indicates
#' all segments are valid cast segments
#' @param inversion_length maximum depth inversion between cast segments of a profile.
#' Consecutive valid cast segments with the same direction are joined together in 
#' the same profile if the range of the introduced depth inversion, if any, is less
#' than the given value. Default value of 0 indicates to never join cast segments
#' @param interrupt_length maximum time separation between cast segments of a profile.
#' Consecutive valid cast segments with the same direction are joined together in the 
#' same profile if the duration of the lapse, which is the sequence of stalled segments
#' or shakes between them, is less than the given value. Default value of 0 indicates
#' to never join segments
#' @param length_length minimum length of a profile. A sequence of joined cast segments
#' will be considered a valid profile only if the total spanned depth is greater or 
#' equal than the given value A default value of 0 indicates that all profiles are valid
#' @param period_length minimum duration of a profile. A sequence of joined cast segments
#' will be considered a valid profile only if the total duration is greater or equal
#' than the given value. Default value of 0 indicates that all profiles are valid.
#' 
#' @return a vector of profile indicies
#' 
#' @example  
#' pressure <- c(0,0,0,1,2,3,4,3,2,1,0,0,0,1,2,1,0)
#' time <- seq(1, length(p),by=1)
#' profile_index <- findProfilesSOCIB(time=time, pressure=pressure)
#' num_profiles <- floor(max(profile_index))
#' profile_range <- seq(1,num_profiles, by=1)
#' profile_index_ok <- lapply(profile_range, function(k) profile_index == k)
#' plot(time, p, type='o', ylim=ylim, xlim=xlim)
#' plotpoints <- lapply(profile_index_ok, function(k) points(time[k], p[k], 
#' col='black', pch=20))
#' @author Chantelle Layton

findProfilesSOCIB <- function(time, pressure, stall_length=0, shake_length=0, 
                              inversion_length=0, interrupt_length=0,
                              length_length=0, period_length=0){
  

p <- pressure

index <- which(!is.na(p))
sdz <- sign(diff(p[index]))
depth_peak <- rep(TRUE, length(index))
depth_peak[2:(length(depth_peak)-1)] <- diff(sdz) != 0
valid_depth_peak <- depth_peak != 0
depth_peak_index <- index[valid_depth_peak]

sgmt_frst <- time[depth_peak_index[1:(length(depth_peak_index)-1)]]
sgmt_last <- time[depth_peak_index[2:length(depth_peak_index)]]

sgmt_strt <- p[depth_peak_index[1:(length(depth_peak_index)-1)]]
sgmt_fnsh <- p[depth_peak_index[2:length(depth_peak_index)]]

sgmt_sinc <- sgmt_last - sgmt_frst
sgmt_vinc <- sgmt_fnsh - sgmt_strt
sgmt_vdir <- sign(sgmt_vinc)

cast_sgmt_ok <- which(!(abs(sgmt_vinc) <= stall_length | sgmt_sinc <= shake_length))

end <- length(cast_sgmt_ok)
cast_sgmt_lapse <- sgmt_frst[cast_sgmt_ok[2:end]] - sgmt_last[cast_sgmt_ok[1:(end-1)]]
cast_sgmt_space <- -sgmt_vdir[cast_sgmt_ok[1:(end-1)]] * (sgmt_strt[cast_sgmt_ok[2:end]] -
                                                            sgmt_fnsh[cast_sgmt_ok[1:(end-1)]])

cast_sgmt_dirch <- diff(sgmt_vdir[cast_sgmt_ok])
cast_sgmt_bound <- !(cast_sgmt_dirch == 0 &
                       cast_sgmt_lapse <= interrupt_length &
                       cast_sgmt_space <= inversion_length)
cast_sgmt_head_valid <- rep(TRUE, length.out=length(cast_sgmt_ok))
cast_sgmt_tail_valid <- rep(TRUE, length.out=length(cast_sgmt_ok))

end <- length(cast_sgmt_ok)
cast_sgmt_head_valid[2:end] <- cast_sgmt_bound
cast_sgmt_tail_valid[1:(end-1)] <- cast_sgmt_bound

cast_head_index <- depth_peak_index[cast_sgmt_ok[cast_sgmt_head_valid]]
cast_tail_index <- depth_peak_index[cast_sgmt_ok[cast_sgmt_tail_valid] + 1]

cast_length <- abs(p[cast_tail_index] - p[cast_head_index])
cast_period <- time[cast_tail_index] - time[cast_head_index]

cast_valid <- !(cast_length <= length_length | cast_period <= period_length)
cast_head <- rep(0, length(p))
cast_tail <- rep(0, length(p))

cast_head[cast_head_index[cast_valid] + 1] <- 0.5
cast_tail[cast_tail_index[cast_valid]] <- 0.5
profile_index <- 0.5 + cumsum(cast_head + cast_tail)

profile_direction <- rep(NA, length(p))
for (i in 1:(length(index)-1)){
  profile_direction[index[i]:index[(i+1)]-1] <- sdz[i]
}
profile_index
}
