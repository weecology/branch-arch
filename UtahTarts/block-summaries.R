### Summarize tree-averages and block level data
library(dplyr)

biennial_index <- function(yield1, yield2, yield3, yield4){
  a <- abs(yield1 - yield2) / (yield1 + yield2)
  b <- abs(yield2 - yield3) / (yield2 + yield3)
  c <- abs(yield3 - yield4) / (yield3 + yield4)
  return(mean(c(a, b, c), na.rm=T))
}

tree_averages <- read.csv('tree-averages-all.csv')
tree_averages_light <- read.csv('tree-averages-light.csv')
tree_volumes <- read.csv('canopy-size.csv')
block_info <- read.csv('block.csv')

scaffold <- read.csv('scaffold.csv')
block_id <- distinct(select(scaffold, grower, block, id, light_id))
block_code <- mutate(block_id, block_code = paste(grower, 0, block, sep=''))
block_code[37, ]$block_code <- 'SS10'
block_code[38, ]$block_code <- 'SS12'

avg_block_code <- inner_join(tree_averages, block_code, by = c('block' = 'id'))
avg_vol_tree <- inner_join(avg_block_code, tree_volumes, 
                           by = c('block_code' = 'block', 'tree'))
avg_vol <- summarize(group_by(avg_vol_tree, block),
                     block_code = unique(block_code),
                     grower = unique(grower),
                     TCSA = mean(TCSA_cm2),
                     mass = 10^(1.83 +  1.25*log10(TCSA))*0.002,
                     cum_BCSA = mean(cum_BCSA),
                     no_scaffolds = mean(no_scaffolds),
                     scaffold_l = mean(avg_scaffold_l),
                     scaffold_l_sd = mean(sd_scaffold_l),
                     scaffold_d = mean(avg_scaffold_d),
                     scaffold_d_sd = mean(sd_scaffold_d),
                     angles = mean(avg_angle),
                     angles_sd = mean(sd_angle),
                     height = mean(height),
                     spread = mean(spread),
                     volume = mean(frustum),
                     top_size = mean(top_frust),
                     top_cone = mean(top_cone))

avg_vol <- inner_join(avg_vol, block_info, by = 'block_code')
avg_vol <- mutate(avg_vol, age = (2014-planting_year),
                  grid_size = spacing_x * spacing_y, 
                  tree_acre = round(sqrt(43560)/spacing_x * 
                                    sqrt(43560)/spacing_y, 0),
                  tree_hect = round(100 / (spacing_x*0.3048) * 
                                   100 / (spacing_y*0.3048), 0))

avg_vol <- avg_vol %>%
  rowwise() %>% 
  mutate(biennial = biennial_index(tree_yield_2012, tree_yield_2013,
                                   tree_yield_2014, tree_yield_2015))

grower_conv <- data.frame(grower.x=levels(avg_vol$grower.x), 
                          grower=as.factor(seq_along(levels(avg_vol$grower.x))))
avg_vol <- left_join(avg_vol, grower_conv)
                          
tree_light_sugar <- read.csv('light-sugar.csv')
avg_l_block_code <- inner_join(tree_averages_light, block_code, by = c('block' = 'light_id'))
avg_vol_light_tree <- inner_join(avg_l_block_code, tree_volumes, 
                                 by = c('block_code' = 'block', 'tree'))

avg_vol_light <- summarize(group_by(avg_vol_light_tree, block),
                           block_code = unique(block_code),
                           grower = unique(grower),
                           TCSA = mean(TCSA_cm2),
                           mass = 10^(1.83 +  1.25*log10(TCSA))*0.002,
                           no_scaffolds = mean(no_scaffolds),
                           scaffold_l = mean(avg_scaffold_l),
                           scaffold_l_sd = mean(sd_scaffold_l),
                           scaffold_d = mean(avg_scaffold_d),
                           scaffold_d_sd = mean(sd_scaffold_d),
                           angles = mean(avg_angle),
                           angles_sd = mean(sd_angle),
                           height = mean(height),
                           spread = mean(spread),
                           volume = mean(frustum),
                           top_size = mean(top_frust),
                           top_cone = mean(top_cone),
                           sugar = mean(avg_sugar, na.rm=T),
                           sugar_out = mean(avg_sugar_out, na.rm=T),
                           sugar_diff = mean(sugar_diff, na.rm=T),
                           absorbed_low = mean(low_light, na.rm=T),
                           absorbed = mean(avg_absorbed, na.rm=T),
                           extinction = mean(avg_extinction))

avg_vol_light_tree_alt <- left_join(avg_vol_light_tree, tree_light_sugar,
                                    by = c('grower', 'block.y' = 'block', 'tree'))
avg_vol_light_alt <- summarize(group_by(avg_vol_light_tree_alt, block),
                               sugar_alt = mean(avg_sugar.y, na.rm=T),
                               light_alt = mean(avg_light, na.rm=T))

avg_vol_light <- inner_join(avg_vol_light, avg_vol_light_alt)
avg_vol_light <- inner_join(avg_vol_light, block_info, by = 'block_code')
avg_vol_light <- mutate(avg_vol_light, age = (2014-planting_year),
                        grid_size = spacing_x * spacing_y, 
                        tree_acre = round(sqrt(43560)/spacing_x * 
                                            sqrt(43560)/spacing_y, 0),
                        tree_hect = round(100 / (spacing_x*0.3048) * 
                          100 / (spacing_y*0.3048), 0))
avg_vol_light <- left_join(avg_vol_light, grower_conv)

avg_vol_light <- avg_vol_light %>%
  rowwise() %>% 
  mutate(biennial = biennial_index(tree_yield_2012, tree_yield_2013,
                                   tree_yield_2014, tree_yield_2015))

smalls <- filter(avg_vol, TCSA < 200)
bigs <- filter(avg_vol, TCSA > 200)
scaffolds <- filter(scaffold, scaffold!=0)