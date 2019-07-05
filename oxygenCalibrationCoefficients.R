oxycalib <- vector(mode = 'list', length = 5)
names(oxycalib) <- c('SEA019', 'SEA021', 'SEA022', 'SEA024', 'SEA032')

# R:/Shared/Gliders/SEA019/Calibration_files/SEA019_CTD_DO_calib_files_SN0186_2016.pdf [as of 2018-03-14]
oxycalib$SEA019[[1]] <- c(Soc = 3.2305e-4, Foffset = -830.74, A = -3.4562e-3, B = 1.1709e-4,
                  C = -1.7302e-6, Enom = 0.036, date = as.POSIXct('2016-04-21', tz = 'UTC'))
# calibration done in December 2018
# R:/Shared/Gliders/SEA019/Calibration_files/SEA019_CTD_DO_calib_files_SN0186_2018.pdf [as of 2019-01-09]
oxycalib$SEA019[[2]] <- c(Soc = 2.8304e-4, Foffset = -835.35, A = -3.7649e-3, B = 1.1716e-4,
                     C = -1.3298-6, Enom = 0.036, date = as.POSIXct('2018-12-19', tz = 'UTC'))

# R:/Shared/Gliders/SEA021/Calibration_files/SEA021_CTD_DO_calib_files_SN0184_2016.pdf [as of 2018-03-14]
oxycalib$SEA021[[1]] <- c(Soc = 2.7945e-4, Foffset = -794.20, A = -3.4437e-3, B = 1.5480e-4,
                          C = -2.3721e-6, Enom = 0.036, date = as.POSIXct('2016-04-22', tz = 'UTC'))
# atlshares/MARFIS/Shares/Gliders/SEA021/Calibration_files/SEA021_CTD_DO_calib_files_SN0184_2018 [as of 2018-12-07]
# calibration done in July 2018
oxycalib$SEA021[[2]] <- c(Soc = 2.8722e-4, Foffset = -777.23, A = -4.4454e-3, B = 1.8630e-4,
                          C = -2.8650e-6, Enom = 0.036, date = as.POSIXct('2018-08-18', tz = 'UTC'))

# R:/Shared/Gliders/SEA022/Calibration_files/SEA022_CTD_DO_calib_files_SN0175_2016.pdf [as of 2018-03-14]
oxycalib$SEA022[[1]] <- c(Soc = 3.1884e-4, Foffset = -807.15, A = -4.2074e-3, B = 2.2413e-4,
                  C = -3.4516e-6, Enom = 0.036, date = as.POSIXct('2016-01-30', tz = 'UTC'))
# calibration done in March 2019
# R:/Shared/Gliders/SEA022/Calibration_files/SEA022_DO_calib_files_SN0175_2019.pdf [as of 2018-03-14]
oxycalib$SEA022[[2]] <- c(Soc = 2.9165e-4, Foffset = -800.49, A = -5.0595e-3, B = 2.3090e-4,
                     C = -3.2232-6, Enom = 0.036, date = as.POSIXct('2019-03-12', tz = 'UTC'))

# R:/Shared/Gliders/SEA022/Calibration_files/SEA024_CTD_DO_calib_files_SN0188_2016.pdf [as of 2018-03-14]
oxycalib$SEA024[[1]] <- c(Soc = 2.8277e-4, Foffset = -847.84, A = -2.8377e-3, B = 1.2076e-4,
                          C = -2.0639e-6, Enom = 0.036, date = as.POSIXct('2016-04-20', tz = 'UTC'))
# atlshares/MARFIS/Shares/Gliders/SEA024/Calibration_files/SEA024_CTD_DO_calib_files_SN0188_2018 [as of 2018-12-07]
# calibration done in July 2018
oxycalib$SEA024[[2]] <- c(Soc = 3.1993e-4, Foffset = -834.99, A = -3.6818e-3, B = 1.4127e-4,
                          C = -2.2880e-6, Enom = 0.036, date = as.POSIXct('2018-08-14', tz = 'UTC'))