######################################
# Conceptual figure for ANOVA
# Will Vieira
# November 12, 2019
######################################


# generate data for 3 diff normal dist
x = seq(0, 21, length.out = 500)
y1 = dnorm(x, 14, 1.7)
y2 = dnorm(x, 12.4, 1.7)
y3 = dnorm(x, 6.5, 1.7)


# plot
colorBlind = c("#000000", "#E69F00", "#56B4E9")

par(xaxs = "i", yaxs = "i", mar = c(2, 0.7, 0.7, 0.7))
plot(x, y1, type = 'l', axes = FALSE, lwd = 3.5, xlab = "", ylab = "", ylim = c(0, 0.34), yaxt = "n", yaxt = "n", col = colorBlind[1])
lines(x, y2, lwd = 3.5, col = colorBlind[2])
lines(x, y3, lwd = 3.5, col = colorBlind[3])
axis(1, at = c(-20, 30), lwd = 2.5)
axis(2, at = c(-5, 5), lwd = 2.5)


# get arrow positions
posList = list() # list to save position
yLevel = c(0.028, 0.018, 0.008) # y value for each arrow

for(i in 1:3)
{
  # get closest value in the rigth and left size of the distribution for the respective yLevel value
  y <- get(paste0('y', i))
  leftValue = which(abs(y[1:250] - yLevel[i]) == min(abs(y[1:250] - yLevel[i])))
  rigthValue = which(abs(y[251:500] - yLevel[i]) == min(abs(y[251:500] - yLevel[i])))

  # get the position of the highest value of y
  hv <- which(abs(y - max(y)) == min(abs(y - max(y))))

  # asign to vector
  posList[[i]] <- c(leftValue, (rigthValue + 251), hv)
}


# plot arrows (difference within groups)
for(i in 1:3)
  arrows(x[posList[[i]][1]], yLevel[i], x[posList[[i]][2]], yLevel[i], lwd = 2, lty = 1, col = colorBlind[i], angle = 15, code = 3)

# plot abline for mean of each group
for(i in 1:3)
{
  y <- get(paste0('y', i))
  arrows(x[posList[[i]][3]], 0, x[posList[[i]][3]], y[posList[[i]][3]] + 0.015, col = colorBlind[i], lwd = 2, lty = 3, code = 0)
}

# add text mean
mtext(expression(mu[1]), 1, at = x[posList[[1]][3]], line = -9.7, cex = 2)
mtext(expression(mu[2]), 1, at = x[posList[[2]][3]], line = -9.7, cex = 2)
mtext(expression(mu[3]), 1, at = x[posList[[3]][3]], line = -9.7, cex = 2)

# plot arrow (difference between groups)
arrows(x[posList[[3]][3]], y[posList[[3]][3]] + 0.06, x[posList[[1]][3]], y[posList[[3]][3]] + 0.06, lwd = 2, lty = 2, code = 0)
arrows(x[posList[[3]][3]], y[posList[[3]][3]] + 0.05, x[posList[[3]][3]], y[posList[[3]][3]] + 0.07, lwd = 2, lty = 1, code = 0)
arrows(x[posList[[1]][3]], y[posList[[3]][3]] + 0.05, x[posList[[1]][3]], y[posList[[3]][3]] + 0.07, lwd = 2, lty = 1, code = 0)
mtext('Variance between group', at = 10.25, line = -1.4, cex = 1.4)
mtext('Variance within group', 1, at = x[posList[[3]][3]], line = 0.3, cex = 1.4, col = colorBlind[3])
