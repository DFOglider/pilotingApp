f <- 'icon1.bmp'
width <- 30
height <- 30
bmp(f, width = width, height = height, bg = 'transparent', type = 'cairo')
par(mar=c(0,0,0,0))
plot(.5, .5, pch = 15, col = 'black', cex = min(width, height) / 8,
     bty = 'n', axes = FALSE, ylab = '', xlab = '', type = 'p')
dev.off()

f <- 'icon2.bmp'
width <- 50
height <- 50
tiff(f, width = width, height = height, bg = 'transparent', type = 'cairo')
par(mar=c(0,0,0,0))
plot(.5, .5, pch = 18, col = 'black', cex = min(width, height) / 8,
     bty = 'n', axes = FALSE, ylab = '', xlab = '', type = 'p')
dev.off()



