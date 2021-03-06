### This script analysis the NC-140 Rootstock roots biomass.

library('dplyr')
library('agricolae')
library('ggplot2')
library('grid')

roots_mass   <- mutate(read.csv('RootDryMass.csv', head = T, sep = ','),
                       location = row + num / 100,
                       small = total_m - (large + medium))  # in grams

stump_mass   <- arrange(read.csv('StumpMass.csv', head = T, sep = ','),
                        location)                           # in kilograms

yield        <- arrange(read.csv('RootYield.csv', head = T, sep = ','),
                        location)
yield[yield$location == 9.23, ]$location = 9.24  # Discrepency between data

roots_yield  <- read.csv('RootstockYieldRoots.csv', head=T, sep = ',')

# Group by individual tree ----
by_location  <- group_by(roots_mass, location)

tree_tots    <- summarize(by_location, 
                          total_roots  = sum(total_m, na.rm = T),
                          total_large  = sum(large, na.rm = T),
                          total_medium = sum(medium, na.rm = T),
                          total_small  = sum(small, na.rm = T))

ids          <- c(12, 9, NA, 5, 11, 
                  10, NA, 8, NA, 3, 
                  4, NA, NA, NA, 6, 
                  7, 1, NA, 2, NA)

rootstocks   <- c('Bud.9', 'CG.6210', 'M.9', 'CG.3041', 'CG.3041', 
                  'CG.6210', 'CG.5935', 'CG.3041', 'CG.5935', 'Bud.9', 
                  'CG.6210', 'CG.5935', 'CG.5935', 'M.9', 'CG.3041', 
                  'Bud.9', 'CG.6210', 'M.9', 'Bud.9', 'M.9')

tree_totals  <- arrange(
                  mutate(tree_tots,
                         location = factor(location),
                         stump_mass = stump_mass$stump_wgt_kg * 1000,
                         id = ids, 
                         rootstock = factor(rootstocks)),
                  location)

roots_list <- c()
for (r in rootstocks){
  roots_list <- append(roots_list, rep(r, 45))
}

roots_mass  <- mutate(roots_mass,
                      location = factor(location),
                      rootstock = factor(roots_list))

  
# Group by rootstock ----
by_rootstock <- group_by(tree_totals, rootstock)

roots_totals <- arrange(
                  summarize(by_rootstock, 
                            avg_total  = mean(total_roots),
                            avg_large  = mean(total_large),
                            avg_medium = mean(total_medium),
                            avg_small  = mean(total_small),
                            avg_stump  = mean(stump_mass)),
                  rootstock)

# Group by rootstock by depth ----
within_between <- rep(c(
  rep("within", 15), rep("middle", 15), rep("between", 15)), 20)

rows_mass <- mutate(roots_mass,
                    distance = rep(c(rep(45, 5), rep(90, 5), rep(135, 5)), 60),
                    direction =  rep(c(
                      rep("within", 15), rep("middle", 15), rep("between", 15)), 
                      20))

by_rootstock_depth <- group_by(rows_mass, rootstock, 
                               direction, distance, max_depth)

roots_depth <- summarize(by_rootstock_depth,
                         total_roots = sum(total_m, na.rm=T))

# ANOVA ----
# Split plot methods as per http://www3.imperial.ac.uk/portal/pls/portallive/docs/1/1171923.PDF
# model<-aov(Glycogen~Treatment+Error(Treatment/Rat/Liver))  # Nested
# model<-aov(yield~irrigation*density*fertilizer+Error(block/irrigation/density/fertilizer))  # Split-plot
# Treatment is fixed effect NOT random
# error is biggest scale to smallest

# Rootstock
duncan_rootstock <- c()
summary_rootstock <- c()

model <- aov(stump_mass ~ rootstock, data = tree_totals)
duncan_rootstock$stump <- duncan.test(model, "rootstock")$groups
summary_rootstock$stump <- summary(model)

model <- aov(total_roots ~ rootstock, data = tree_totals)
duncan_rootstock$total_roots <- duncan.test(model, "rootstock")$groups
summary_rootstock$total_roots <- summary(model)

model <- aov(total_large ~ rootstock, data = tree_totals)
duncan_rootstock$total_large <- duncan.test(model, "rootstock")$groups
summary_rootstock$total_large <- summary(model)

model <- aov(total_medium ~ rootstock, data = tree_totals)
duncan_rootstock$total_medium <- duncan.test(model, "rootstock")$groups
summary_rootstock$total_medium <- summary(model)

model <- aov(total_small ~ rootstock, data = tree_totals)
duncan_rootstock$total_small <- duncan.test(model, "rootstock")$groups
summary_rootstock$total_small <- summary(model)

# Location by Rootstock

duncan_location <- c()
summary_location <- c()

model <- aov(total_m ~ rootstock*location, data = roots_mass)
# model <- aov(total_m ~ rootstock*location+Error(rootstock/location), data = roots_mass) GIVES ERROR
duncan_location$total_roots <- duncan.test(model, "location")$groups
summary_location$total_roots <- summary(model)

model <- aov(large ~ rootstock*location, data = roots_mass)
duncan_location$large <- duncan.test(model, "location")$groups
summary_location$large <- summary(model)

model <- aov(medium ~ rootstock*location, data = roots_mass)
duncan_location$medium <- duncan.test(model, "location")$groups
summary_location$medium <- summary(model)

model <- aov(small ~ rootstock*location, data = roots_mass)
duncan_location$small <- duncan.test(model, "location")$groups
summary_location$small <- summary(model)

# Depth by Location by Roostock

duncan_depth <- c()
summary_depth <- c()

model <- aov(total_m ~ rootstock*location*max_depth, data = roots_mass)
duncan_depth$total_roots <- duncan.test(model, "max_depth")$groups
summary_depth$total_roots <- summary(model)

model <- aov(large ~ rootstock*location*max_depth, data = roots_mass)
duncan_depth$large <- duncan.test(model, "max_depth")$groups
summary_depth$large <- summary(model)

model <- aov(medium ~ rootstock*location*max_depth, data = roots_mass)
duncan_depth$medium <- duncan.test(model, "max_depth")$groups
summary_depth$medium <- summary(model)

model <- aov(small ~ rootstock*location*max_depth, data = roots_mass)
duncan_depth$small <- duncan.test(model, "rootstock")$groups
summary_depth$small <- summary(model)

# Linear Models ----

## tree_totals vs yield
tree_results <- c()
tree_results$roots <- c('stump_mass', 'total_roots', 'total_large',
                        'total_medium', 'total_small')

for (roots in tree_results$roots){
  yield_test <- lm(yield$yield~tree_totals[roots][[1]])
  tree_results$yield <- append(tree_results$yield, 
                               round(summary.lm(yield_test)$r.squared, 3))
  
  cum_yield_test <- lm(yield$cum_yield~tree_totals[roots][[1]])
  tree_results$cum_yield <- append(tree_results$cum_yield, 
                               round(summary.lm(cum_yield_test)$r.squared, 3))
  
  no_fruit_test <- lm(yield$no_fruit~tree_totals[roots][[1]])
  tree_results$no_fruit <- append(tree_results$no_fruit, 
                               round(summary.lm(no_fruit_test)$r.squared, 3))
  
  avg_wgt_test <- lm(yield$avg_fruit_wgt~tree_totals[roots][[1]])
  tree_results$avg_wgt <- append(tree_results$avg_wgt, 
                               round(summary.lm(avg_wgt_test)$r.squared, 3))
}

## roots_totals vs roots_yield

roots_results <- c()
roots_results$roots <- c('avg_stump', 'avg_total', 'avg_large',
                        'avg_medium', 'avg_small')

for (roots in roots_results$roots){
  
  cum_yield_test <- lm(roots_yield$avg_cum_yield~roots_totals[roots][[1]])
  roots_results$cum_yield <- append(roots_results$cum_yield, 
                                   round(summary.lm(cum_yield_test)$r.squared, 3))
  
  no_fruit_test <- lm(roots_yield$avg_no_fruit~roots_totals[roots][[1]])
  roots_results$no_fruit <- append(roots_results$no_fruit, 
                                  round(summary.lm(no_fruit_test)$r.squared, 3))
  
  avg_wgt_test <- lm(roots_yield$avg_fruit_wgt~roots_totals[roots][[1]])
  roots_results$avg_wgt <- append(roots_results$avg_wgt, 
                                 round(summary.lm(avg_wgt_test)$r.squared, 3))
}