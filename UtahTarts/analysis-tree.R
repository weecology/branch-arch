### This code generates the analysis for Utah Co tarts
library('dplyr')

gen_plot <- function(x, y, x_lab, y_lab, r2, location) {
  test <- lm(y~x)
  par(mar=c(5,5,1,1))
  plot(x, y,
       xlim=c(min(x, na.rm=T)-0.2, max(x, na.rm=T)+0.2), 
       ylim=c(min(y, na.rm=T)-0.2, max(y, na.rm=T)+0.2),
       xlab=x_lab, ylab=y_lab, cex.lab=1.8, cex.axis=1.3, cex=3, pch=19, 
       col="black")
  abline(test$coefficients[1], test$coefficients[2], lwd=4, lty=2)
  legend(location, legend=r2, bty='n', cex=3, x.intersp=0)
  print(summary(test)$r.squared)
}

gen_plot_poly <- function(x, y, x_lab, y_lab, r2, location) {
  test <- lm(y~poly(x, 2, raw=T))
  par(mar=c(5,5,1,1))
  plot(x, y,
       xlim=c(min(x, na.rm=T)-0.2, max(x, na.rm=T)+0.2), 
       ylim=c(min(y, na.rm=T)-0.2, max(y, na.rm=T)+0.2),
       xlab=x_lab, ylab=y_lab, cex.lab=1.8, cex.axis=1.3, cex=3, pch=19, 
       col="black")
  curve(test$coef[3]*x^2 + test$coef[2]*x + test$coef[1],
        min(x)-0.1, max(x)+0.1,
        lwd=4, lty=2, add=T)
  legend(location, legend=r2, bty='n', cex=3, x.intersp=0)
  print(summary(test)$r.squared)
}

averages <- read.csv('tree-averages-all.csv')
averages_light <- read.csv('tree-averages-light.csv')
volumes <- read.csv('canopy-size.csv')

scaffold <- read.csv('scaffold.csv')
block_id <- distinct(select(scaffold, grower, block, id, light_id))
block_code <- mutate(block_id, block_code = paste(grower, 0, block, sep=''))
block_code[37, ]$block_code <- 'SS10'
block_code[38, ]$block_code <- 'SS12'

avg_block_code <- inner_join(averages, block_code, by = c('block' = 'id'))
avg_vol <- inner_join(avg_block_code, volumes, 
                      by = c('block_code' = 'block', 'tree'))

avg_l_block_code <- inner_join(averages_light, block_code, by = c('block' = 'light_id'))
avg_vol_light <- inner_join(avg_l_block_code, volumes, 
                      by = c('block_code' = 'block', 'tree'))

## How does tree size affect tree architecture and canopy size?

#architecture
gen_plot(averages$trunk_mm, averages$avg_scaffold_d,
         'Trunk Diameter [mm]', 'Avg. Scaffold Diameter',
         expression(R^2 == 0.747), 'topleft')

gen_plot(averages$TCSA_cm2, averages$avg_scaffold_d,
         'TCSA [cm2]', 'Avg. Scaffold Diameter',
         expression(R^2 == 0.744), 'topleft')

gen_plot(averages$trunk_mm, averages$avg_scaffold_l,
         'Trunk Diameter [mm]', 'Avg. Scaffold Length',
         expression(R^2 == 0.720), 'topleft')

gen_plot(averages$TCSA_cm2, averages$avg_scaffold_l,
         'TCSA [cm2]', 'Avg. Scaffold Length',
         expression(R^2 == 0.674), 'topleft')

gen_plot(averages$avg_scaffold_d, averages$avg_scaffold_l,
         'Avg Scaffold Diameter', 'Avg. Scaffold Length',
         expression(R^2 == 0.676), 'topleft')

## How does tree size affect fruit quality?

gen_plot(averages_light$avg_scaffold_l, averages_light$avg_sugar_out,
         'Average Scaffold Length [cm]', 'Fruit Sugar Content [Brix]', 
         expression(R^2 == 0.590), 'topright') 

gen_plot(avg_vol_light$height, avg_vol_light$avg_sugar_out,
         'Canopy Volume [m3]', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.583), 'topright')

gen_plot(averages_light$trunk_mm, averages_light$avg_sugar_out, 
         'Trunk Diameter [mm]', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.455), 'topright')

gen_plot(averages_light$TCSA_cm2, averages_light$avg_sugar_out, 
         'TCSA [cm2]', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.383), 'topright')  

gen_plot(avg_vol_light$spread, avg_vol_light$avg_sugar_out,
         'Canopy spread', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.339), 'topright')

gen_plot(avg_vol_light$frustum, avg_vol_light$avg_sugar_out,
         'Canopy Volume [m3]', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.396), 'topright')

gen_plot(avg_vol_light$top_frust, avg_vol_light$avg_sugar_out,
         'Canopy Top', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.396), 'topright')

gen_plot(avg_vol_light$height/avg_vol_light$TCSA, avg_vol_light$avg_sugar_out, 
         'Height : TCSA', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.456), 'topleft')  

gen_plot(avg_vol_light$spread/avg_vol_light$TCSA, avg_vol_light$avg_sugar_out, 
         'Spread : TCSA', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.513), 'topleft') 

gen_plot(avg_vol_light$TCSA/avg_vol_light$no_scaffold, avg_vol_light$avg_sugar_out, 
         'TCSA : No.Scaffold', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.502), 'topright') 


## How does tree size afect light?

#TCSA vs. light
gen_plot(averages_light$TCSA_cm2, averages_light$avg_absorbed, 
         'TCSA [cm2]', 'Absorbed Light',
         expression(R^2 == 0.005), 'topright')

gen_plot(avg_vol_light$frustum, avg_vol_light$avg_absorbed,
         'Canopy Volume [m3]', 'Fruit Sugar Content[Brix]',
         expression(R^2 == 0.017), 'topright')


#canopy

gen_plot_poly(avg_vol$TCSA_cm2, avg_vol$height,
              'TCSA', 'Canopy Height',
              expression(R^2 == 0.643), 'topleft')

gen_plot_poly(avg_vol$TCSA_cm2, avg_vol$spread,
         'TCSA', 'Canopy Spread',
         expression(R^2 == 0.543), 'topleft')

gen_plot_poly(avg_vol$TCSA_cm2, avg_vol$frustum,
              'TCSA', 'Canopy Volume',
              expression(R^2 == 0.533), 'topleft')


### Other regression

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
         'St. Dev. Scaffold Diameter [mm]', 'Fruit Sugar Content [Brix]', 

gen_plot(averages_light$avg_absorbed, averages_light$avg_sugar, 
         'Average Light Absorption', 'Fruit Sugar Content [Brix]',
         expression(R^2 == 0.097))

gen_plot(averages$TCSA_cm2, averages$no_scaffold,
         'TCSA [cm2]', 'No. of Scaffolds,
         expression(R^2 == 0.004), 'topleft')