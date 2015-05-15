### This script analysis the NC-140 Rootstock roots biomass.

library('dplyr')

roots_mass <- mutate(read.csv('RootDryMass.csv', head = T, sep = ','),
                     location = row + num / 100,
                     small = total_m - (large + medium))

yield <- read.csv('AppleYield.csv', head = T, sep = ',') 

locations <- select(distinct(roots_mass, location), location) 

tree_ids     <- c(2,7,12,3,
                   5,11,6,8,
                   10,1,4,9,
                   13,
                   17,15,18,
                   20,19,14)


tree_stock   <- c("Bud.9", "Bud.9", "Bud.9", "Bud.9", 
                   "CG.3041", "CG.3041", "CG.3041", "CG.3041", 
                   "CG.6210", "CG.6210", "CG.6210", "CG.6210", 
                   "M.26", 
                   "JM.8", "JM.8", "JM.8",
                   "PiAu.5683", "PiAu.5683", "PiAu.5683")

get_id <- function(location){
  yield_row <- filter(yield, location == l)
  if (length(yield_row[[1]])){
    return(filter(yield, location == l)$tree)
  } else {
    return(NA)
  }
}

ids_temp          <- c('Bud.9', 'CG.6210', NA, 'CG.3041', 'CG.3041', 'CG.6210', NA, 'CG.3041', NA, 'Bud.9', 
                  'CG.6210', NA, NA, NA, 'CG.3041', 'Bud.9', 'CG.6210', NA, NA, NA)

ids          <- c()
rootstocks   <- c()
total_roots  <- c()
total_large  <- c()
total_medium <- c()
total_small  <- c()

for (l in locations[[1]]){
  
  ids          <- append(ids, get_id(l))
  rootstocks   <- append(rootstocks, filter(yield, location == l)$tree
  total_roots  <- append(total_roots, 
                         sum(filter(roots_mass, location == l)$total_m, 
                             na.rm = T))
  
  total_large  <- append(total_large, 
                          sum(filter(roots_mass, location == l)$large, 
                              na.rm = T))
  
  total_medium <- append(total_medium, 
                         sum(filter(roots_mass, location == l)$medium, 
                             na.rm = T))
  
  total_small  <- append(total_small,
                         sum(filter(roots_mass, location == l)$small, 
                             na.rm = T))
}

tree_roots <- data.frame(
                location = locations,
                total = total_roots,
                large = total_large,
                medium = total_medium,
                small = total_small)
  