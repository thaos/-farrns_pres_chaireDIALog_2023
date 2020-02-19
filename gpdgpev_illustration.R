library(magrittr)

x = rnorm(100)
block = rep(1:10, rep(10, 10))
imax = tapply(x, block, function(x) x == max(x)) %>% unlist()
thresh = quantile(x, p = 0.9)
iabove = x >= thresh
col = c("grey", "red")
png(filename = "img/blockmaxima.png", width = 600, height = 160)
par(mar = c(2, 3, 0, 1))
plot(x, type = "n", xlab = "", ylab = "", cex.axis  = 2)
points(x, col = col[imax+1], cex = 3, pch = 20)
abline(v = 0.5 + 1:10 * 10, lty = 4, lwd = 3)
dev.off()

png(filename = "img/peakovethreshold.png", width = 600, height = 160)
par(mar = c(2, 3, 0, 1))
plot(x, type = "n", xlab = "", ylab = "", cex.axis= 2, lwd.axis = 2)
abline(h = thresh, lty = 4, lwd = 3)
for(i in seq_along(x)){
  if(iabove[i]) segments(i, x[i],  i, thresh, lwd = 4)
}
points(x, col = col[iabove+1], cex = 3, pch = 20)
dev.off()
