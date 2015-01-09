###This script explores the relationship between light and sugar content (brix) in the Utah Co tarts data.

scaffold <- read.csv('scaffold.csv', sep=',', head=T)
light_raw <- read.csv('light.csv', sep=',', head=T)
sugar <- read.csv('sugars.csv', sep=',', head=T)

absorbed <- round((light_raw$light_sun - light_raw$light) / light_raw$light_sun, 3)
extinction <- round(log(light_raw$light_sun / light_raw$light), 3)

light <- cbind(light_raw, absorbed, extinction)


#Tree-level averages
for (i in 1:15){
  
  light_block <- light[light$id==i,]
  sugar_block <- sugar[sugar$id==i,]
  scaffold_block <- subset(scaffold, light_id==i)
  
  subout <- matrix(ncol= 10, nrow = 5)
  
  for (j in 1:5){
    
    light_ind <- light_block[light_block$tree==j,]
    sugar_ind <- sugar_block[sugar_block$tree==j,]
    scaffold_id <- scaffold_block[scaffold_block$tree==j,]
    trunk <- scaffold_id[scaffold_id$scaffold==0,]
    light_pos <- light_ind[light_ind$absorbed > 0,]
    
    subout[j,1] = i
    subout[j,2] = j
    subout[j,3] = trunk$diameter
    subout[j,4] = round(mean(scaffold_id$length), 0)
    subout[j,5] = round(mean(scaffold_id$diameter), 0)
    subout[j,6] = round(mean(light_pos$absorbed), 3)
    subout[j,7] = round(mean(light_pos$extinction), 3)
    subout[j,8] = round(mean(light_ind$extinction[1:8]), 3)
    subout[j,9] = round(mean(light_ind$extinction[9:16]), 3)
    subout[j,10] = round(mean(sugar_ind$sugar, na.rm=T), 3)
    
  }
  
  if (exists('averages'))
    averages = rbind(averages, subout)
  
  else
    averages <- subout
    colnames(averages) = c('block', 'tree', 'trunk', 'avg_scaffold_l', 'avg_scaffold_d', 'avg_absorbed', 
                           'avg_extinction', 'avg_extinction_low', 'avg_extinction_high', 'avg_sugar') 
}

pdf(file="SugarPred.pdf", width= 12, height=8,family="Helvetica", pointsize=12)
par(oma = c(2,2,0,0))
    
#trunk diameter vs. sugar (R2 = 0.464)
test <- lm(averages[,10]~ averages[,3])
plot(averages[,3], averages[,10],
     xlim = c((min(averages[,3])-0.2),(max(averages[,3])+0.2)), 
     ylim = c((min(averages[,10], na.rm=T)-0.2),(max(averages[,10], na.rm=T)+0.2)),
     xlab = 'Trunk Diameter [mm]', ylab = 'Fruit Sugar Content[Brix]', cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
abline(test$coefficients[1], test$coefficients[2], lwd = 3, lty = 2)
legend('topright', legend=expression(R^2 == 0.464), bty='n', cex=3)  

#scaffold length vs. sugar (R2 = 0.600)
test <- lm(averages[,10]~ averages[,4])
plot(averages[,4], averages[,10],
     xlim = c((min(averages[,4])-0.2),(max(averages[,4])+0.2)), 
     ylim = c((min(averages[,10], na.rm=T)-0.2),(max(averages[,10], na.rm=T)+0.2)),
     xlab = 'Average Scaffold Length [cm]', ylab = 'Fruit Sugar Content [Brix]', cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
abline(test$coefficients[1], test$coefficients[2], lwd = 3, lty = 2)
legend('topright', legend=expression(R^2 == 0.600), bty='n', cex=3)  

#scaffold diameter vs. sugar (R2 = 0.431)
test <- lm(averages[,10]~ averages[,5])
plot(averages[,5], averages[,10], 
     xlim = c((min(averages[,5])-0.2),(max(averages[,5])+0.2)), 
     ylim = c((min(averages[,10], na.rm=T)-0.2),(max(averages[,10], na.rm=T)+0.2)),
     xlab = 'Average Scaffold Diameter [mm]', ylab = 'Fruit Sugar Content [Brix]', cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
abline(test$coefficients[1], test$coefficients[2], lwd = 3, lty = 2)
legend('topright', legend=expression(R^2 == 0.431), bty='n', cex=3)  

#light abs vs. sugar (R2 = 0.109)
test <- lm(averages[,10]~ averages[,6])
plot(averages[,6], averages[,10], 
     xlim = c((min(averages[,6], na.rm=T)-0.05),(max(averages[,6], na.rm=T)+0.05)), 
     ylim = c((min(averages[,10], na.rm=T)-0.05),(max(averages[,10], na.rm=T)+0.05)),
     xlab = 'Average Light Absorption', ylab = 'Fruit Sugar Content [Brix]', cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
abline(test$coefficients[1], test$coefficients[2], lwd = 3, lty = 2)
legend('bottomleft', legend=expression(R^2 == 0.109), bty='n', cex=3)  

dev.off()