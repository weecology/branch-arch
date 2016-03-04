### This code generates the analysis for Utah Co tarts at the BLOCK level
library('dplyr')

gen_plot <- function(x, y, x_lab, y_lab, r2, location, letter) {
  test <- lm(y~x)
  par(mar=c(5,5,1,1))
  plot(x, y,
       xlim=c(min(x, na.rm=T)-(.1*mean(x, na.rm=T)), 
              max(x, na.rm=T)+(.1*mean(x, na.rm=T))), 
       ylim=c(min(y, na.rm=T)-(.1*mean(y, na.rm=T)), 
              max(y, na.rm=T)+(.1*mean(y, na.rm=T))),
       xlab=x_lab, ylab=y_lab, cex.lab=1.8, cex.axis=1.3, cex=3, pch=19, 
       col="black")
  abline(test$coefficients[1], test$coefficients[2], lwd=4, lty=2)
  legend(location, legend=r2, bty='n', cex=2.5, x.intersp=0, inset = -0.04)
  legend("topleft", letter, cex=2.5, bty="n", x.intersp=0, inset=-0.08)
  print(summary(test)$r.squared)
}

gen_plot_poly <- function(x, y, x_lab, y_lab, r2, location, letter) {
  test <- lm(y~poly(x, 2, raw=T))
  par(mar=c(5,5,1,1))
  plot(x, y,
       xlim=c(min(x, na.rm=T)-(.1*mean(x, na.rm=T)), 
              max(x, na.rm=T)+(.1*mean(x, na.rm=T))), 
       ylim=c(min(y, na.rm=T)-(.1*mean(y, na.rm=T)), 
              max(y, na.rm=T)+(.1*mean(y, na.rm=T))),
       xlab=x_lab, ylab=y_lab, cex.lab=1.8, cex.axis=1.3, cex=3, pch=19, 
       col="black")
  curve(test$coef[3]*x^2 + test$coef[2]*x + test$coef[1],
        min(x, na.rm=T)-(.05*mean(x, na.rm=T)), 
        max(x, na.rm=T)+(.05*mean(x, na.rm=T)),
        lwd=4, lty=2, add=T)
  legend(location, legend=r2, bty='n', cex=3, x.intersp=0)
  legend("topleft", letter, cex=2.5, bty="n", x.intersp=0, inset=-0.05)
  print(summary(test)$r.squared)
}

insert_blank <- function(){
  plot(range(0,1), range(0,1), bty='n', main = '', xaxt='n', yaxt='n', 
       xlab='', ylab='', type ='n')
}

source("block-summaries.R")

## How does tree size affect tree architecture and canopy size?

#architecture [Fig 1]
pdf(file="block-architecture.pdf", width=4, height=9, family="Helvetica", 
    pointsize=14)
par(mfrow=c(3,1))
gen_plot(avg_vol$TCSA, avg_vol$cum_BCSA,
         'TCSA [cm2]', 'BCSA [cm2]',
         expression(r^2 == 0.945), 'bottomright', 'A')

gen_plot_poly(avg_vol$TCSA, avg_vol$scaffold_l,
         'TCSA [cm2]', 'Scaffold Length [cm]',
         expression(r^2 == 0.876), 'bottomright', 'B')

gen_plot_poly(avg_vol$scaffold_d, avg_vol$scaffold_l,
         'Scaffold Diameter [cm]', 'Scaffold Length [cm]',
         expression(r^2 == 0.812), 'bottomright', 'C')
dev.off()

#canopy [Fig 2]
pdf(file="block-canopy.pdf", width=4, height=9, family="Helvetica", 
    pointsize=14)
par(mfrow=c(3,1))
gen_plot_poly(avg_vol$TCSA, avg_vol$height,
              'TCSA [cm2]', 'Height [cm]',
              expression(r^2 == 0.789), 'bottomright', 'A')

gen_plot_poly(avg_vol$TCSA, avg_vol$spread,
              'TCSA [cm2]', 'Canopy Spread [cm]',
              expression(r^2 == 0.698), 'bottomright', 'B')

gen_plot_poly(avg_vol$TCSA, avg_vol$volume,
              'TCSA [cm2]', 'Canopy Volume [m3]',
              expression(r^2 == 0.691), 'bottomright', 'C')
dev.off()


## How does tree size affect fruit quality? [Fig 3 & 4]
pdf(file="block-sugar.pdf", width=10, height=8, family="Helvetica", 
    pointsize=14)
par(mfrow=c(2,2))
gen_plot(avg_vol_light$TCSA, avg_vol_light$sugar_out, 
         'TCSA [cm2]', 'Sugar Content[Brix]',
         expression(r^2 == 0.477), 'topright', 'A')  

gen_plot(avg_vol_light$height, avg_vol_light$sugar_out,
         'Height [cm]', 'Sugar Content [Brix]',
         expression(r^2 == 0.765), 'topright', 'B')

gen_plot(avg_vol_light$spread, avg_vol_light$sugar_out,
         'Canopy Spread [cm]', 'Sugar Content [Brix]',
         expression(r^2 == 0.523), 'topright', 'C')

gen_plot(avg_vol_light$volume, avg_vol_light$sugar_out,
         'Canopy Volume [m3]', 'Sugar Content [Brix]',
         expression(r^2 == 0.558), 'topright', 'D')
dev.off()

pdf(file="block-sugar-byTCSA.pdf", width=5, height=8, family="Helvetica", 
    pointsize=14)
par(mfrow=c(2,1))
gen_plot(avg_vol_light$height/avg_vol_light$TCSA, avg_vol_light$sugar_out, 
         'Height : TCSA', 'Sugar Content [Brix]',
         expression(r^2 == 0.560), 'bottomright', 'A')  

gen_plot(avg_vol_light$spread/avg_vol_light$TCSA, avg_vol_light$sugar_out, 
         'Spread : TCSA', 'Sugar Content [Brix]',
         expression(r^2 == 0.629), 'bottomright', 'B') 
dev.off()
  

## How does tree size afect light?

pdf(file="block-light.pdf", width=4.5, height=7, family="Helvetica", 
    pointsize=14)

par(mfrow=c(2,1))

gen_plot(avg_vol_light$TCSA, avg_vol_light$absorbed, 
         'TCSA [cm2]', 'Absorbed Light',
         expression(R^2 == 0.000), 'bottomleft', 'A')

gen_plot(avg_vol_light$volume, avg_vol_light$absorbed,
         'Canopy Volume [m3]', 'Absorbed Light',
         expression(R^2 == 0.015), 'bottomleft', 'B')
dev.off()


## How does grower affect tree shape?
pdf(file="grower.pdf", width=7.5, height=5.5, family="Helvetica", 
    pointsize=12)
plot(avg_vol$grower.y, avg_vol$TCSA, ylab="TCSA")
plot(avg_vol$grower.y, avg_vol$height, ylab="Height")
plot(avg_vol$grower.y, avg_vol$no_scaffold, ylab="No. of Scaffolds")
plot(avg_vol$grower.y, avg_vol$angles, ylab="Branch Angle", ylim=c(40,70))
plot(avg_vol$grower.y, avg_vol$spread, ylab="Canopy Spread")
plot(avg_vol$grower.y, avg_vol$tree_acre, ylab="Tree / Acre")
plot(avg_vol$grower.y, avg_vol$tree_yield_2014, ylab="Yield / Tree")
plot(avg_vol_light$grower.y, avg_vol_light$sugar_out, ylab="Sugar Content")
plot(avg_vol_light$grower.y, avg_vol_light$sugar_out/avg_vol_light$TCSA, 
     ylab="Sugar Content / TCSA")
plot(avg_vol_light$grower.y, avg_vol_light$absorbed, ylab="Light Absorption")
plot(avg_vol_light$grower.y, avg_vol_light$absorbed/avg_vol_light$TCSA, 
     ylab="Light Absorption / TCSA")
plot(avg_vol_light$grower.y, avg_vol_light$absorbed, ylab="Light Absorption")
dev.off()


### Other regressions

##Age
avg_vol_zero <- filter(avg_vol, !is.na(age))
gen_plot_poly((2014-avg_vol_zero$planting_year), avg_vol_zero$TCSA,
              'Age', 'TCSA',
              expression(r^2 == 0.825), 'bottomright', 'C')

gen_plot_poly(avg_vol_zero$scaffold_d/10, (2014-avg_vol_zero$planting_year),
         'Scaffold Diameter', 'Age',
         expression(r^2 == 0.775), 'bottomright', 'C')

gen_plot(avg_vol_zero$scaffold_l, (2014-avg_vol_zero$planting_year),
         'Scaffold Length', 'Age',
         expression(r^2 == 0.499), 'bottomright', 'C')

gen_plot(avg_vol_zero$height, (2014-avg_vol_zero$planting_year),
         'Height', 'Age',
         expression(r^2 == 0.499), 'bottomright', 'C')

gen_plot(avg_vol_light$TCSA/(2014-avg_vol_light$planting_year), avg_vol_light$absorbed,
         'Age', 'Absorbed Light',
         expression(R^2 == 0.150), 'bottomleft', 'B')

gen_plot(avg_vol_light$TCSA/(2014-avg_vol_light$planting_year), avg_vol_light$sugar_out, 
         'Age', 'Sugar Content[Brix]',
         expression(R^2 == 0.435), 'topright', 'A')

##Grower by Age
young <- filter(avg_vol, age <= 15) 
young_light <- filter(avg_vol_light, age <= 15)
old <- filter(avg_vol, age > 15) 
old_light <- filter(avg_vol_light, age > 15)

pdf(file="grower_age.pdf", width=6, height=9, family="Helvetica", 
    pointsize=14)
par(mfrow = c(3,2), mar=c(4,4,3,1))
plot(young$grower, young$TCSA, ylab="TCSA", main = "Young (<15 yrs)")
plot(old$grower, old$TCSA, ylab="TCSA", main = "Old (> 15 yrs)")
plot(young$grower, young$height, ylab="Height")
plot(old$grower, old$height, ylab="Height")
plot(young$grower, young$no_scaffold, ylab="No. of Scaffolds", xlab = "Grower")
plot(old$grower, old$no_scaffold, ylab="No. of Scaffolds", xlab = "Grower")
plot(young$grower, young$angles, ylab="Branch Angle", ylim=c(40,70), main = "Young (<15 yrs)")
plot(old$grower, old$angles, ylab="Branch Angle", ylim=c(40,70), main = "Old (> 15 yrs)")
plot(young$grower, young$spread, ylab="Canopy Spread")
plot(old$grower, old$spread, ylab="Canopy Spread")
plot(young$grower, young$tree_acre, ylab="Tree / Acre", xlab = "Grower")
plot(old$grower, old$tree_acre, ylab="Tree / Acre", xlab = "Grower")
plot(young$grower, young$tree_yield_2014, ylab="Yield / Tree", main = "Young (<15 yrs)")
plot(old$grower, old$tree_yield_2014, ylab="Yield / Tree", main = "Old (> 15 yrs)")
plot(young$grower, young$tree_yield_2014*young$tree_acre, ylab="Yield / Acre")
plot(old$grower, old$tree_yield_2014*old$tree_acre, ylab="Yield / Acre")
plot(young$grower, young$tree_yield_2014/young$TCSA, 
     ylab="Yield Efficiency", xlab = "Grower")
plot(old$grower, old$tree_yield_2014/old$TCSA, 
     ylab="Yield Efficiency", xlab = "Grower")
plot(young$grower, young$biennial, ylab="Biennial Index", main = "Young (<15 yrs)")
plot(old$grower, old$biennial, ylab="Biennial Index", main = "Old (> 15 yrs)")
plot(young_light$grower, young_light$sugar_out, ylab="Sugar Content")
plot(old_light$grower, old_light$sugar_out, ylab="Sugar Content")
plot(young_light$grower, young_light$sugar_out/young_light$TCSA, 
     ylab="Sugar Content / TCSA", xlab = "Grower")
plot(old_light$grower, old_light$sugar_out/old_light$TCSA, 
     ylab="Sugar Content / TCSA", xlab = "Grower")
dev.off()

plot(young_light$grower.x, young_light$absorbed, ylab="Light Absorption", main = "Young (<15 yrs)")
plot(old_light$grower.x, old_light$absorbed, ylab="Light Absorption", main = "Old (> 15 yrs)")
plot(young_light$grower.x, young_light$absorbed/young_light$TCSA, 
     ylab="Light Absorption / TCSA", main = "Young (<15 yrs)")
plot(old_light$grower.x, old_light$absorbed/old_light$TCSA, 
     ylab="Light Absorption / TCSA", main = "Old (> 15 yrs)")

##Spacing
hist(avg_vol$tree_hect)
hist(avg_vol$tree_acre)

abs(median(spacing$spacing_x)*0.3048)) * abs(100 / (median(avg_vol$spacing_y)*0.3048))
abs(100 / (min(avg_vol$spacing_x)*0.3048)) * abs(100 / (min(avg_vol$spacing_y)*0.3048))

gen_plot(avg_vol_light$spacing_x, avg_vol_light$sugar_out, 
         'In Row spacing', 'Sugar Content[Brix]',
         expression(R^2 == 0.163), 'topright', 'A')

gen_plot(bigs$spacing_x, bigs$spread, 
         'In Row spacing', 'Canopy Spread',
         expression(R^2 == 0.112), 'topright', 'A')

##Sugar
gen_plot(avg_vol_light$scaffold_l, avg_vol_light$sugar_out,
         'Scaffold Length [cm]', 'Sugar Content [Brix]', 
         expression(R^2 == 0.727), 'topright', 'B') 

gen_plot(avg_vol_light$TCSA/avg_vol_light$no_scaffold, avg_vol_light$sugar_out, 
         'TCSA : No.Scaffold', 'Sugar Content [Brix]',
         expression(R^2 == 0.446), 'topleft', 'H') 

gen_plot(avg_vol_light$TCSA/avg_vol_light$angles, avg_vol_light$sugar_out, 
         'TCSA : No.Scaffold', 'Sugar Content [Brix]',
         expression(R^2 == 0.366), 'topleft', 'H') 

gen_plot(averages_light$avg_scaffold_l, averages_light$avg_sugar,
         'Average Scaffold Length [cm]', 'Fruit Sugar Content [Brix]', 
         expression(R^2 == 0.554)) 

gen_plot(averages_light$trunk, averages_light$avg_sugar, 
         'Trunk Diameter [mm]', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.438))

gen_plot(averages_light$TCSA, averages_light$avg_sugar, 
         'TCSA [cm2]', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.364))  

gen_plot(averages_light$avg_scaffold_d, averages_light$avg_sugar, 
         'Average Scaffold Diameter [mm]', 'Fruit Sugar Content [Brix]', 
         expression(R^2 == 0.351))  

gen_plot(averages_light$sd_scaffold_d, averages_light$avg_sugar, 
         'St. Dev. Scaffold Diameter [mm]', 'Fruit Sugar Content [Brix]', 
         expression(R^2 == 0.110))

gen_plot(averages_light$sd_scaffold_l, averages_light$avg_sugar, 
         'St. Dev. Scaffold Diameter [mm]', 'Fruit Sugar Content [Brix]') 

gen_plot(averages_light$avg_absorbed, averages_light$avg_sugar, 
         'Average Light Absorption', 'Fruit Sugar Content [Brix]',
         expression(R^2 == 0.097))

gen_plot(averages$TCSA_cm2, averages$no_scaffold,
         'TCSA [cm2]', 'No. of Scaffolds',
         expression(R^2 == 0.004), 'topleft')

plot(avg_vol$height, avg_vol$angles)