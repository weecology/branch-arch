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

tree_averages <- read.csv('tree-averages-all.csv')
tree_averages_light <- read.csv('tree-averages-light.csv')
tree_volumes <- read.csv('canopy-size.csv')

scaffold <- read.csv('scaffold.csv')
block_id <- distinct(select(scaffold, grower, block, id, light_id))
block_code <- mutate(block_id, block_code = paste(grower, 0, block, sep=''))
block_code[37, ]$block_code <- 'SS10'
block_code[38, ]$block_code <- 'SS12'

avg_block_code <- inner_join(tree_averages, block_code, by = c('block' = 'id'))
avg_vol_tree <- inner_join(avg_block_code, tree_volumes, 
                           by = c('block_code' = 'block', 'tree'))
avg_vol <- summarize(group_by(avg_vol_tree, block),
                     grower = unique(grower),
                     TCSA = mean(TCSA_cm2),
                     no_scaffolds = mean(no_scaffolds),
                     scaffold_l = mean(avg_scaffold_l),
                     scaffold_l_sd = mean(sd_scaffold_l),
                     scaffold_d = mean(avg_scaffold_d),
                     scaffold_d_sd = mean(sd_scaffold_d),
                     height = mean(height),
                     spread = mean(spread),
                     volume = mean(frustum),
                     top_size = mean(top_frust),
                     top_cone = mean(top_cone))

tree_light_sugar <- read.csv('light-sugar.csv')
avg_l_block_code <- inner_join(tree_averages_light, block_code, by = c('block' = 'light_id'))
avg_vol_light_tree <- inner_join(avg_l_block_code, tree_volumes, 
                                 by = c('block_code' = 'block', 'tree'))

avg_vol_light <- summarize(group_by(avg_vol_light_tree, block),
                           grower = unique(grower),
                           TCSA = mean(TCSA_cm2),
                           no_scaffolds = mean(no_scaffolds),
                           scaffold_l = mean(avg_scaffold_l),
                           scaffold_l_sd = mean(sd_scaffold_l),
                           scaffold_d = mean(avg_scaffold_d),
                           scaffold_d_sd = mean(sd_scaffold_d),
                           height = mean(height),
                           spread = mean(spread),
                           volume = mean(frustum),
                           top_size = mean(top_frust),
                           top_cone = mean(top_cone),
                           sugar = mean(avg_sugar),
                           sugar_out = mean(avg_sugar_out),
                           sugar_diff = mean(sugar_diff),
                           absorbed = mean(avg_absorbed),
                           extinction = mean(avg_extinction))

avg_vol_light_tree_alt <- left_join(avg_vol_light_tree, tree_light_sugar,
                                    by = c('grower', 'block.y' = 'block', 'tree'))
avg_vol_light_alt <- summarize(group_by(avg_vol_light_tree_alt, block),
                               sugar_alt = mean(avg_sugar.y),
                               light_alt = mean(avg_light))

avg_vol_light <- inner_join(avg_vol_light, avg_vol_light_alt)


## How does tree size affect tree architecture and canopy size?

#architecture
pdf(file="block-architecture.pdf", width= 4, height=9, family="Helvetica", 
    pointsize=14)
par(mfrow=c(3,1))
gen_plot(avg_vol$TCSA, avg_vol$scaffold_d/10,
         'TCSA [cm2]', 'Scaffold Diameter [cm]',
         expression(R^2 == 0.901), 'bottomright', 'A')

gen_plot(avg_vol$TCSA, avg_vol$scaffold_l,
         'TCSA [cm2]', 'Scaffold Length [cm]',
         expression(R^2 == 0.788), 'bottomright', 'B')

gen_plot(avg_vol$scaffold_d/10, avg_vol$scaffold_l,
         'Scaffold Diameter [cm]', 'Scaffold Length [cm]',
         expression(R^2 == 0.738), 'bottomright', 'C')
dev.off()

#canopy
pdf(file="block-canopy.pdf", width= 4, height=9, family="Helvetica", 
    pointsize=14)
par(mfrow=c(3,1))
gen_plot_poly(avg_vol$TCSA, avg_vol$height,
              'TCSA [cm2]', 'Height [cm]',
              expression(R^2 == 0.789), 'bottomright', 'A')

gen_plot_poly(avg_vol$TCSA, avg_vol$spread,
              'TCSA [cm2]', 'Canopy Spread [cm]',
              expression(R^2 == 0.543), 'bottomright', 'B')

gen_plot_poly(avg_vol$TCSA, avg_vol$volume,
              'TCSA [cm2]', 'Canopy Volume [m3]',
              expression(R^2 == 0.691), 'bottomright', 'C')
dev.off()


## How does tree size affect fruit quality?
pdf(file="block-sugar.pdf", width= 8, height=9, family="Helvetica", 
    pointsize=14)
par(mfrow=c(3,2))

gen_plot(avg_vol_light$TCSA, avg_vol_light$sugar_out, 
         'TCSA [cm2]', 'Sugar Content[Brix]',
         expression(R^2 == 0.477), 'topright', 'A')  

gen_plot(avg_vol_light$height, avg_vol_light$sugar_out,
         'Height [cm]', 'Sugar Content [Brix]',
         expression(R^2 == 0.765), 'topright', 'B')

gen_plot(avg_vol_light$volume, avg_vol_light$sugar_out,
         'Canopy Volume [m3]', 'Sugar Content [Brix]',
         expression(R^2 == 0.558), 'topright', 'C')

gen_plot(avg_vol_light$spread, avg_vol_light$sugar_out,
         'Canopy Spread [cm]', 'Sugar Content [Brix]',
         expression(R^2 == 0.523), 'topright', 'D')

gen_plot(avg_vol_light$height/avg_vol_light$TCSA, avg_vol_light$sugar_out, 
         'Height : TCSA', 'Sugar Content [Brix]',
         expression(R^2 == 0.560), 'bottomright', 'E')  

gen_plot(avg_vol_light$spread/avg_vol_light$TCSA, avg_vol_light$sugar_out, 
         'Spread : TCSA', 'Sugar Content [Brix]',
         expression(R^2 == 0.629), 'bottomright', 'F') 
dev.off()


## How does tree size afect light?

#TCSA vs. light
gen_plot(avg_vol_light$TCSA, avg_vol_light$absorbed, 
         'TCSA [cm2]', 'Absorbed Light',
         expression(R^2 == 0.000), 'topright')

gen_plot(avg_vol_light$volume, avg_vol_light$absorbed,
         'Canopy Volume [m3]', 'Absorbed Light',
         expression(R^2 == 0.015), 'topright')

## How does grower affect tree shape?
plot(avg_vol$grower, avg_vol$TCSA)
plot(avg_vol$grower, avg_vol$height)
plot(avg_vol$grower, avg_vol$no_scaffold)
plot(avg_vol$grower, avg_vol$spread)
plot(avg_vol_light$grower, avg_vol_light$sugar_out)
plot(avg_vol_light$grower, avg_vol_light$absorbed)

### Other regression

gen_plot(avg_vol_light$scaffold_l, avg_vol_light$sugar_out,
         'Scaffold Length [cm]', 'Sugar Content [Brix]', 
         expression(R^2 == 0.727), 'topright', 'B') 

gen_plot(avg_vol_light$TCSA/avg_vol_light$no_scaffold, avg_vol_light$sugar_out, 
         'TCSA : No.Scaffold', 'Sugar Content [Brix]',
         expression(R^2 == 0.446), 'topleft', 'H') 

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