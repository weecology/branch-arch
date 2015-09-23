###This script generates the per tree averages of light and sugar content (brix) in the Utah Co tarts data.

gen_plot <- function(x, y, x_lab, y_lab, r2) {
  test <- lm(y~x)
  plot(x, y,
       xlim=c(min(x, na.rm=T)-0.2, max(x, na.rm=T)+0.2), 
       ylim=c(min(y, na.rm=T)-0.2, max(y, na.rm=T)+0.2),
       xlab=x_lab, ylab=y_lab, cex.lab=1.5, cex=2.5, pch=19, col="black")
  abline(test$coefficients[1], test$coefficients[2], lwd=3, lty=2)
  legend('topright', legend=r2, bty='n', cex=3)
  print(summary(test)$r.squared)
}

scaffold <- read.csv('scaffold.csv', sep=',', head=T)
light_raw <- read.csv('light.csv', sep=',', head=T)
sugar <- read.csv('sugars.csv', sep=',', head=T)

absorbed <- round((light_raw$light_sun - light_raw$light) / light_raw$light_sun, 3)
extinction <- round(log(light_raw$light_sun / light_raw$light), 3)

light <- cbind(light_raw, absorbed, extinction)

#Tree-level averages table
for (i in 1:15){
  
  light_block <- light[light$id==i,]
  sugar_block <- sugar[sugar$id==i,]
  scaffold_block <- subset(scaffold, light_id==i)
  
  subout <- matrix(ncol= 12, nrow = 5)
  
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
    subout[j,10] = round(mean(sugar_ind$sugar, na.rm=T), 2)
    subout[j,11] = round(mean(sugar_ind$sugar[1:4], na.rm=T), 2)
    subout[j,12] = round((subout[j,11] - sugar_ind$sugar[5]) / subout[j,11],3)
  }
  
  if (exists('averages'))
    averages <- rbind(averages, subout)
  
  else
    averages <- subout
    colnames(averages) = c('block', 'tree', 'trunk', 'avg_scaffold_l', 
                           'avg_scaffold_d', 'avg_absorbed', 'avg_extinction', 
                           'avg_extinction_low', 'avg_extinction_high', 
                           'avg_sugar', 'avg_sugar_out', 'sugar_diff') 
}

#write.csv(averages, "tree-averages.csv")


### Tree average regression

averages <- read.csv("tree-averages.csv")
TCSA <- pi*(averages$trunk/20)^2

#trunk diameter vs. scaffold diameter
gen_plot(averages$trunk, averages$avg_scaffold_d,
         'Trunk Diameter [mm]', 'Avg. Scaffold Diameter',
         expression(R^2 == 0.898))

#trunk diameter vs. scaffold length
gen_plot(averages$trunk, averages$avg_scaffold_l,
         'Trunk Diameter [mm]', 'Avg. Scaffold Length',
         expression(R^2 == 0.872))

#scaffold diameter vs. scaffold length
gen_plot(averages$avg_scaffold_d, averages$avg_scaffold_l,
         'Avg Scaffold Diameter', 'Avg. Scaffold Length',
         expression(R^2 == 0.811))

#TCSA vs. scaffold diameter
gen_plot(TCSA, averages$avg_scaffold_d,
         'TCSA [cm2]', 'Avg. Scaffold Diameter',
         expression(R^2 == 0.876))

#TCSA vs. scaffold length
gen_plot(TCSA, averages$avg_scaffold_l,
         'TCSA [cm2]', 'Avg. Scaffold Length',
         expression(R^2 == 0.839))

#trunk diameter vs. sugar (R2 = 0.438)
gen_plot(averages$trunk, averages$avg_sugar, 
         'Trunk Diameter [mm]', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.438))

#TCSA vs. sugar (R2 = 0.365)
gen_plot(TCSA, averages$avg_sugar, 
         'TCSA [cm2]', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.365))  

#scaffold length vs. sugar 
gen_plot(averages$avg_scaffold_l, averages$avg_sugar,
         'Average Scaffold Length [cm]', 'Fruit Sugar Content [Brix]', 
         expression(R^2 == 0.554))  

#scaffold diameter vs. sugar
gen_plot(averages$avg_scaffold_d, averages$avg_sugar, 
         'Average Scaffold Diameter [mm]', 'Fruit Sugar Content [Brix]', 
         expression(R^2 == 0.381))  

#light abs vs. sugar
gen_plot(averages$avg_absorbed, averages$avg_sugar, 
         'Average Light Absorption', 'Fruit Sugar Content [Brix]',
         expression(R^2 == 0.097))  