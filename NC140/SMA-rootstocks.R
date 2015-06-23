### This script runs SMA group and slope test to determine sign differences 
### between rootstocks for tree and branch level

library('dplyr')
library('smatr')

test_group <- function(name, formulas, data){  
  sma_row <- c(name)
  for (i in 1:length(formulas)) {
    if (is.na(formulas[i][[1]])) {  # give warning 'only first element used'
      sma_row <- c(sma_row, '_')
    } else if (formulas[i][[1]] == 'diameter_ratio ~ parent_diam') {
      test <- sma(as.formula(paste(formulas[i], '*rootstock')), 
                  log = '', data = data)
      sma_row <- c(sma_row, check_slopetest_p(test[[3]]$p))
      
    } else {
      test <- sma(as.formula(paste(formulas[i], '*rootstock')), 
                  log = 'xy', data = data)
      sma_row <- c(sma_row, check_slopetest_p(test[[3]]$p))
    }
  }
  return(sma_row)
}


check_slopetest_p <- function(slopetest_p) {
  if (slopetest_p <= 0.05) {
    return('-') # Different Slopes
  } else {
    return('X')  # Same Slope
  }
}

treesum <- read.csv("TreeSummary.csv")
branch_size <- read.csv("BranchSegments.csv")

rootstocks <- data.frame(tree      =  c(2,7,12,3,
                                        5,11,6,8,
                                        10,1,4,9,
                                        13,
                                        17,15,18,
                                        20,19,14),                      
                         
                         rootstock = c("Bud.9", "Bud.9", "Bud.9", "Bud.9", 
                                       "CG.3041", "CG.3041", "CG.3041", "CG.3041", 
                                       "CG.6210", "CG.6210", "CG.6210", "CG.6210", 
                                       "M.26", 
                                       "JM.8", "JM.8", "JM.8",
                                       "PiAu.5683", "PiAu.5683", "PiAu.5683"))

branch_size <- left_join(branch_size, rootstocks)

tree_formulas <- c(height     ~ trunk_diam,
                   max_path   ~ trunk_diam,
                   tot_length ~ trunk_diam,
                   NA,
                   NA,
                   tot_area   ~ tot_volume,
                   NA,
                   NA,
                   trunk_diam ~ tot_volume,
                   height     ~ tot_volume,
                   max_path   ~ tot_volume,
                   tot_length ~ tot_volume,
                   NA,
                   NA,
                   trunk_diam ~ tot_area,
                   height     ~ tot_area,
                   max_path   ~ tot_area,
                   tot_length ~ tot_area,
                   height     ~ tot_stem_m,
                   max_path   ~ tot_stem_m,
                   tot_length ~ tot_stem_m,
                   NA,
                   tot_stem_m ~ trunk_diam,
                   NA,
                   NA,
                   tot_stem_m ~ tot_volume,
                   NA)

branch_formulas <- c(length_cm   ~ diameter_mm, 
                     path_length ~ diameter_mm, 
                     tot_length  ~ diameter_mm,
                     area        ~ volume,
                     path_area   ~ path_volume,
                     tot_area    ~ tot_volume,
                     diameter_mm ~ volume,
                     diameter_mm ~ path_volume,
                     diameter_mm ~ tot_volume,
                     length_cm   ~ volume,
                     path_length ~ path_volume,
                     tot_length  ~ tot_volume,
                     diameter_mm ~ area,
                     diameter_mm ~ path_area,
                     diameter_mm ~ tot_area,
                     length_cm   ~ area,
                     path_length ~ path_area,
                     tot_length  ~ tot_area,
                     length_cm   ~ stem_m,
                     path_length ~ tot_stem_m,
                     tot_length  ~ tot_stem_m,
                     stem_m      ~ diameter_mm,
                     tot_stem_m  ~ diameter_mm,
                     stem_m      ~ volume,
                     stem_m      ~ path_volume,
                     tot_stem_m  ~ tot_volume,
                     diameter_ratio ~ parent_diam)


relationships <- c("L~D (Segment)", "(Path)", "(Subtree)", 
                   "SA~V (Segment)", "(Path)", "(Subtree)", 
                   "D~V(Segment)", "(Path)", "(Subtree)", 
                   "L~V (Segment)", "(Path)", "(Subtree)", 
                   "D~SA (Segment)", "(Path)", "(Subtree)", 
                   "L~SA (Segment)", "(Path)", "(Subtree)", 
                   "L~M (Segment)", "(Path)", "(Subtree)", 
                   "M~D (Segment)", "(Subtree)",
                   "M~V(Segment)", "(Path)", "(Subtree)", 
                   "D/P Ratio ~ P Diam")


# Output ----
output <- c()

## Tree Level
output <- rbind(output, test_group('tree-level', tree_formulas, treesum))

## Branch Level by group
branch_size <- mutate(branch_size, parent_diam = diameter_mm / diameter_ratio)
output <- rbind(output, test_group('branch-level', branch_formulas, branch_size))

colnames(output) <- c('group', relationships)

#write.csv(output, "RootstockSMATest.csv")