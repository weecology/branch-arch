treesum <- read.csv("TreeSummary.csv", sep = ",", head=T)
yield <- read.csv("AppleYield.csv", sep =',', head=T)


Diameter_Yield <- sma(log10(treesum$yield[1:19])~log10(treesum$trunk_diam[1:19]))
plot(log(treesum$trunk_diam[1:19]), log(treesum$yield[1:19]), #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Trunk Diameter  )", ylab = "log ( Yield )", cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
abline(summary(Diameter_Yield)$coef[1,1], summary(Diameter_Yield)$coef[2,1], lwd = 3, lty = 3)
legend('bottomleft', legend=expression(R^2 == 0.193), bty='n', cex=3)


Spurs_Yield <- lm(treesum$yield[1:19]~(treesum$tot_no_scars[1:19]/treesum$tot_no_spurs[1:19]))
plot(treesum$tot_no_scars[1:19]/treesum$tot_no_spurs[1:19], treesum$yield[1:19], #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Scars / Spurs )", ylab = "log ( Yield )")
#abline(summary(Spurs_Yield)$coef[1,1], summary(Spurs_Yield)$coef[3,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.594), bty='n', cex=3)

Volume_Yield <- lm(log(treesum$yield[1:19])~log(treesum$canopy_volume[1:19]))
plot(log(treesum$canopy_volume[1:19]), log(treesum$yield[1:19]), #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Canopy Volume  )", ylab = "log ( Yield )")
abline(summary(Volume_Yield)$coef[1,1], summary(Volume_Yield)$coef[2,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.278), bty='n', cex=3)

Pf_Yield <- lm(treesum$yield[1:19])~log(treesum$canopy_volume[1:19]))
plot(treesum$Pf[1:19], treesum$yield[1:19]), #xlim = c(0,6), ylim = c(0,12),
xlab = "log ( Pf  )", ylab = "log ( Yield )")
abline(summary(Pf_Yield)$coef[1,1], summary(Pf_Yield)$coef[2,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.278), bty='n', cex=3)

PredYield<-glm(treesum$yield[1:19]~log(treesum$trunk_diam[1:19])+treesum$Pf[1:19]+
  treesum$tot_no_scars[1:19]/treesum$tot_no_spurs[1:19], data=treesum, family = gaussian())
YieldFit<-lm(predict(fit, type="response")~1+treesum$yield[1:19])
plot(treesum$yield[1:19],predict(fit, type="response"),
     xlab = "Observed Yield", ylab = "Predicted Yield", cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
abline(0, 1, lwd = 3, lty = 3)
legend('bottomright', legend=expression(R^2 == 0.409), bty='n', cex=3)