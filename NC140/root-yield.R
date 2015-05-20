### This script analysis the NC-140 Rootstock roots biomass.

library('dplyr')
library('agricolae')

roots_mass   <- mutate(read.csv('RootDryMass.csv', head = T, sep = ','),
                       location = row + num / 100,
                       small = total_m - (large + medium))  # in grams

stump_mass   <- arrange(read.csv('StumpMass.csv', head = T, sep = ','),
                        location)                           # in kilograms

yield        <- read.csv('AppleYield.csv', head = T, sep = ',') 
yield[yield$location == 9.23, ]$location = 9.24  # Discrepency between data

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

tree_totals  <- mutate(tree_tots,
                       location = factor(location),
                       stump_mass = stump_mass$stump_wgt_kg * 1000,
                       id = ids, 
                       rootstock = factor(rootstocks))

roots_list <- c()
for (r in rootstocks){
  roots_list <- append(roots_list, rep(r, 45))
}

roots_mass <- mutate(roots_mass,
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
                  avg_total)

# Analysis ----
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
duncan_depth$small <- duncan.test(model, "max_depth")$groups
summary_depth$small <- summary(model)

# Remnant code ----

#  Used to generate ids vector
# get_id <- function(location){
#   yield_row <- filter(yield, location == l)
#   if (length(yield_row[[1]])){
#     return(filter(yield, location == l)$tree)
#   } else {
#     return(NA)
#   }
# }

# ids <- c()
# for (l in tree_totals$location){ 
#   ids <- append(ids, get_id(l)) 
# }