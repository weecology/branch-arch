### This script analysis the NC-140 Rootstock roots biomass.

library('dplyr')

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
                       stump_mass = stump_mass$stump_wgt_kg * 1000,
                       id = ids, 
                       rootstock = factor(rootstocks))

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

model <- aov(total_roots ~ rootstock*location+Error(rootstock/location), data = tree_totals)
model <- aov(avg_total ~ rootstock, data = roots_totals)

model <- aov(total_roots ~ rootstock, data = tree_totals)
comparison <- duncan.test(model,"rootstock")
comparison$groups

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