### This script evaluates regressions for morphology, roots, and yield.

library('dplyr')

# Aboveground Morphology Data ----

tree_morph <- read.csv('TreeSummary.csv')

sma <- read.csv('SMAResults.csv', sep=',', head=T) 

pred_exp <- list()
for (i in 1:27){                              # Scaling relationships
  pred_exp[[i]] <- list()
  for (j in 1:4){                             # Results output
    #   1.exponent, 2.CI-, 3.CI+, 4.R2 
    pred_exp[[i]][[j]] <- vector(length = 32)
    for (k in 1:32) {                         # Groups and individuals
      pred_exp[[i]][[j]][k] = as.numeric(strsplit(
        as.character(sma[(k+1),(i+2)]), " ")[[1]][(2*j-1)])
    }
  }
}

roots_exc   <- c(1, 2, 3, 8, 9, 11, 15, 19, 21:33)

above_trees <- mutate(
                 filter(tree_morph, rootstock == 'Bud.9' |
                        rootstock == 'CG.3041' |
                        rootstock == 'CG.6210'),
                 L_D_sub  = pred_exp[[3]][[1]][-roots_exc],
                 D_V_sub  = pred_exp[[9]][[1]][-roots_exc],
                 M_D_seg  = pred_exp[[22]][[1]][-roots_exc],
                 M_D_sub  = pred_exp[[23]][[1]][-roots_exc],
                 D_SA_sub = pred_exp[[15]][[1]][-roots_exc])


# Roots data ----

roots_mass   <- mutate(read.csv('RootDryMass.csv', head = T, sep = ','),
                       location = row + num / 100,
                       small = total_m - (large + medium))  # in grams

stump_mass   <- arrange(read.csv('StumpMass.csv', head = T, sep = ','),
                        location)                           # in kilograms

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

tree_ids  <- c(2, 7, 12, 3, 5, 11, 6, 8, 10, 1, 4, 9)

root_trees <- c()
for (i in tree_ids){
  root_trees <- rbind(root_trees, filter(tree_totals, id == i))
}


# Yield Data ----

yield     <- read.csv('AppleYield.csv', head = T, sep = ',')

yield_trees <- c()
for (i in tree_ids){
  yield_trees <- rbind(yield_trees, filter(yield, tree == i))
}


# Analysis ----

cum_yield_exp_stump   <- lm(yield_trees$cum_yield~
                            above_trees$L_D_sub + root_trees$stump_mass +
                            above_trees$L_D_sub * root_trees$stump_mass)
  # R2 == 0.843

afw_exp_stump         <- lm(yield_trees$cum_yield~
                            above_trees$D_V_sub + root_trees$stump_mass  +
                            above_trees$D_V_sub * root_trees$stump_mass)
  # R2 == 0.847

yield_exp_stump       <- lm(yield_trees$yield~
                            above_trees$L_D_sub + root_trees$stump_mass  +
                            above_trees$L_D_sub * root_trees$stump_mass)
  # R2 == 0.637

yield_exp_stump_scars <- lm(yield_trees$yield~
                            above_trees$L_D_sub + root_trees$stump_mass  +
                            above_trees$tot_no_scars / above_trees$tot_no_spurs)
  # R2 == 0.679