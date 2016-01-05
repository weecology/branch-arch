###This script generates the per tree averages of light and sugar content (brix) in the Utah Co tarts data.

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
  
  subout <- matrix(ncol= 16, nrow = 5)
  
  for (j in 1:5){
    
    light_ind <- light_block[light_block$tree==j,]
    sugar_ind <- sugar_block[sugar_block$tree==j,]
    scaffold_id <- scaffold_block[scaffold_block$tree==j,]
    trunk <- scaffold_id[scaffold_id$scaffold==0,]
    scaffolds <- scaffold_id[scaffold_id$scaffold!=0,]
    light_pos <- light_ind[light_ind$absorbed > 0,]
    
    subout[j, 1] = i
    subout[j, 2] = j
    subout[j, 3] = trunk$diameter
    subout[j, 4] = round(pi*(trunk$diameter/20)^2, 0)
    subout[j, 5] = dim(scaffolds)[1]
    subout[j, 6] = round(mean(scaffolds$length), 0)
    subout[j, 7] = round(sd(scaffolds$length), 3)
    subout[j, 8] = round(mean(scaffolds$diameter), 0)
    subout[j, 9] = round(sd(scaffolds$diameter), 3)
    subout[j, 10] = round(mean(light_pos$absorbed), 3)
    subout[j, 11] = round(mean(light_pos$extinction), 3)
    subout[j, 12] = round(mean(light_ind$extinction[1:8]), 3)
    subout[j, 13] = round(mean(light_ind$extinction[9:16]), 3)
    subout[j, 14] = round(mean(sugar_ind$sugar, na.rm=T), 2)
    subout[j, 15] = round(mean(sugar_ind$sugar[1:4], na.rm=T), 2)
    subout[j, 16] = round((subout[j,15] - sugar_ind$sugar[5]) / subout[j,15],3)
  }
  
  if (exists('averages'))
    averages <- rbind(averages, subout)
  
  else
    averages <- subout
    colnames(averages) = c('block', 'tree', 'trunk_mm', 'TCSA_cm2', 'no_scaffolds', 
                           'avg_scaffold_l', 'sd_scaffold_l',
                           'avg_scaffold_d', 'sd_scaffold_d', 'avg_absorbed', 
                           'avg_extinction', 'avg_extinction_low', 
                           'avg_extinction_high', 'avg_sugar', 'avg_sugar_out', 
                           'sugar_diff') 
}

#write.csv(averages, "tree-averages-light.csv")

for (i in 1:38){
  
  scaffold_block <- subset(scaffold, id==i)
  
  subout <- matrix(ncol= 10, nrow = 5)
  
  for (j in 1:5){
    
    scaffold_id <- scaffold_block[scaffold_block$tree==j,]
    trunk <- scaffold_id[scaffold_id$scaffold==0,]
    scaffolds <- scaffold_id[scaffold_id$scaffold!=0,]
    
    subout[j, 1] = i
    subout[j, 2] = j
    subout[j, 3] = trunk$diameter
    subout[j, 4] = round(pi*(trunk$diameter/20)^2, 0)
    subout[j, 5] = dim(scaffolds)[1]
    subout[j, 6] = round(mean(scaffolds$length), 0)
    subout[j, 7] = round(sd(scaffolds$length), 3)
    subout[j, 8] = round(mean(scaffolds$diameter), 0)
    subout[j, 9] = round(sd(scaffolds$diameter), 3)
    subout[j, 10] = round(sum(pi*(scaffolds$diameter/20)^2), 0)

  }
  
  if (exists('averages_all'))
    averages_all <- rbind(averages_all, subout)
  
  else
    averages_all <- subout
    colnames(averages_all) = c('block', 'tree', 'trunk_mm', 'TCSA_cm2', 'no_scaffolds', 
                               'avg_scaffold_l', 'sd_scaffold_l',
                               'avg_scaffold_d', 'sd_scaffold_d', 'cum_BCSA') 
}

#write.csv(averages_all, "tree-averages-all.csv")