### This code manipulates data to look at the within canopy level of light.

library('dplyr')

light_raw <- read.csv('light.csv')
light <- mutate(light_raw, 
                absorbed = round((light_sun - light) / light_sun, 3),
                extinction = round(log(light_sun / light), 3))
sugar <- read.csv('sugars.csv')

### point joins

light_sugar_join <- left_join(sugar, light, 
                              by = c('grower', 'block', 'tree', 'position'))
light_sugar_bottom <- select(light_sugar_join, grower, block, tree, position, 
                             sugar, absorbed, extinction) 

position <- list(c(1:18), c('N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW', 0, 0))
layers <- data.frame(bottom=c(1:8, 17), top=c(9:16,18))

sugar_top <- left_join(sugar, layers, by = c('position' = 'bottom'))
light_sugar_top_join <- left_join(sugar_top, light, 
                              by = c('grower', 'block', 'tree', 'top' = 'position'))
light_sugar_top <- select(light_sugar_top_join, grower, block, tree, position, 
                             sugar, absorbed, extinction)
light_sugar_top_out <- filter(light_sugar_top, position !=17)

### average window joins
light_top <- filter(light, position>=9 & position<=16)
light_top_avg <- mutate(light_top, 
                        avg_light = (lead(absorbed)+lag(absorbed)+absorbed)/3)
light_sugar_top_avg_join <- left_join(sugar_top, light_top_avg, 
                                  by = c('grower', 'block', 'tree', 'top' = 'position'))
light_sugar_top_avg <- select(light_sugar_top_avg_join, grower, block, tree, position, 
                          sugar, avg_light)

light_bottom <- filter(light, position<=8)
light_bottom_avg <- mutate(light_bottom, 
                        avg_light = (lead(absorbed)+lag(absorbed)+absorbed)/3)
light_sugar_bot_avg_join <- left_join(sugar, light_bottom_avg, 
                                      by = c('grower', 'block', 'tree', 'position'))
light_sugar_bot_avg <- select(light_sugar_bot_avg_join, grower, block, tree, position, 
                              sugar, avg_light)
### position subset

light_sugar_17 <- filter(light_sugar_bottom, position==17)
light_sugar_18 <- filter(light_sugar_top, position==17)

light_sugar_S <- filter(light_sugar_top, position>=3 & position<=8)
light_sugar_avg_S <- summarize(group_by(light_sugar_S, grower, block, tree),
                               avg_light = mean(absorbed),
                               avg_sugar = mean(sugar))

light_sugar_N <- filter(light_sugar_top, position==c(1,2,8))
light_sugar_avg_N <- filter(light_sugar_top_avg,  position==c(1,2,8))

###Output

#write.csv(light_sugar_avg_S, 'light-sugar.csv')

plot(light_sugar_bottom$extinction, light_sugar_bottom$sugar) 
plot(light_sugar_bottom$absorbed, light_sugar_bottom$sugar, xlim = c(0,1)) 

plot(light_sugar_top$extinction, light_sugar_top$sugar) 
plot(light_sugar_top$absorbed, light_sugar_top$sugar) 

plot(light_sugar_S$extinction, light_sugar_S$sugar) 
plot(light_sugar_S$absorbed, light_sugar_S$sugar)

plot(light_sugar_top_avg$avg_light, light_sugar_top_avg$sugar)  #avg_light from absorbed

plot(light_sugar_avg_S$avg_light, light_sugar_avg_S$avg_sugar)
S_top_light_sugar <- lm(avg_sugar~avg_light, data = light_sugar_avg_S) 
summary(S_top_light_sugar)$r.squared

top_light_sugar <- lm(sugar~avg_light, data = light_sugar_top_avg) 
summary(top_light_sugar)$r.squared

