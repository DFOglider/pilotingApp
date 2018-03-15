oxycalib <- vector(mode = 'list', length = 5)
names(oxycalib) <- c('SEA019', 'SEA021', 'SEA022', 'SEA024', 'SEA032')

# R:/Shared/Gliders/SEA019/Calibration_files/SEA019_CTD_DO_calib_files_SN0186.pdf [as of 2018-03-14]
oxycalib$SEA019 <- c(Soc = 3.2305e-4, Foffset = -830.74, A = -3.4562e-3, B = 1.1709e-4,
                  C = -1.7302e-6, Enom = 0.036)

# R:/Shared/Gliders/SEA021/Calibration_files/SEA021_CTD_DO_calib_files_SN0184.pdf [as of 2018-03-14]
oxycalib$SEA021 <- c(Soc = 2.7945e-4, Foffset = -794.20, A = -3.4437e-3, B = 1.5480e-4,
                  C = -2.3721e-6, Enom = 0.036)

# R:/Shared/Gliders/SEA022/Calibration_files/SEA022_CTD_DO_calib_files_SN0175.pdf [as of 2018-03-14]
oxycalib$SEA022 <- c(Soc = 3.1884e-4, Foffset = -807.15, A = -4.2074e-3, B = 2.2413e-4,
                  C = -3.4516e-6, Enom = 0.036)

# R:/Shared/Gliders/SEA022/Calibration_files/SEA024_CTD_DO_calib_files_SN0188.pdf [as of 2018-03-14]
oxycalib$SEA024 <- c(Soc = 2.8277e-4, Foffset = -847.84, A = -2.8377e-3, B = 1.2076e-4,
                  C = -2.0639e-6, Enom = 0.036)

# To-do : different oxygen sensor on SEA032 need to implement calibration and conversion
