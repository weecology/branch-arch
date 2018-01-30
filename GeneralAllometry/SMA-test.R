### This script builds a table of SMA predictions (`SMAResults.csv`) at tree and
### branch levels for all, species, rootstocks, and individual groupings.
### The analysis is replicated by an earlier version of the code `SMATable.r`.

treesum <- read.csv("TreeSummary.csv", sep = ",", head=T)
branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)
library('smatr')

test_slope <- function(name, formulas, data){  
  sma_row <- c(name)
  for (i in 1:length(formulas)) {
    if (is.na(formulas[i][[1]])) {  # give warning 'only first element used'
      sma_row <- c(sma_row, '-')
    } else if (formulas[i][[1]] == 'diameter_ratio ~ parent_diam') {
      test <- sma(formulas[i][[1]], log = '', data = data)
      sma_row <- c(sma_row, params_out(test))
    } else {
      test <- sma(formulas[i][[1]], log = 'xy', data = data)
      sma_row <- c(sma_row, params_out(test))
    }
  }
  return(sma_row)
}

params_out <- function(sma_data){
  return (paste(round(sma_data$coef[[1]][2,1],2), " [ ", 
                round(sma_data$coef[[1]][2,2],2), " , ", 
                round(sma_data$coef[[1]][2,3],2), " ]; ", 
                round(sma_data$r2[[1]], 3), sep = ""))
} 

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

plus_formulas   <- c(length_cm        ~ diameter_mm, 
                     path_length_plus ~ diameter_mm, 
                     tot_length_plus  ~ diameter_mm,
                     area_plus        ~ volume_plus,
                     NA,
                     tot_area_plus    ~ tot_volume_plus,
                     diameter_mm      ~ volume_plus,
                     NA,
                     diameter_mm      ~ tot_volume_plus,
                     length_cm        ~ volume_plus,
                     path_length_plus ~ tot_volume_plus,
                     tot_length_plus  ~ tot_volume_plus,
                     diameter_mm      ~ area_plus,
                     NA,
                     diameter_mm      ~ tot_area_plus,
                     length_cm        ~ area_plus,
                     path_length_plus ~ tot_area_plus,
                     tot_length_plus  ~ tot_area_plus,
                     length_cm        ~ segment_mass,
                     path_length_plus ~ total_mass,
                     tot_length_plus  ~ total_mass,
                     segment_mass  ~ diameter_mm,
                     total_mass ~ diameter_mm,
                     segment_mass  ~ volume_plus,
                     NA,
                     total_mass ~ tot_volume_plus,
                     diameter_ratio ~ parent_diam)

groups <- c("all-tree", "cherry", "apple", "all-branch", "cherry", "apple", "Bud.9", "CG.3041", "CG.6210", "M.26", "JM.8", "PiAu.5683")
group_data <- list()
group_data[[1]] <- treesum
group_data[[2]] <- treesum[treesum$species=="cherry",]
group_data[[3]] <- treesum[treesum$species=="apple",]
group_data[[4]] <- branch_size
group_data[[5]] <- branch_size[branch_size$species=="cherry",]
group_data[[6]] <- branch_size[branch_size$species=="apple",]
group_data[[7]] <- subset(group_data[[6]], tree==2 | tree==7 | tree==12 | tree ==3)
group_data[[8]] <- subset(group_data[[6]], tree==5 | tree==11 | tree==6 | tree ==8)
group_data[[9]] <- subset(group_data[[6]], tree==10 | tree==1 | tree==4 | tree ==9)
group_data[[10]] <- subset(group_data[[6]], tree==13)
group_data[[11]] <- subset(group_data[[6]], tree==17 | tree==15 | tree==18)
group_data[[12]] <- subset(group_data[[6]], tree==20 | tree==19 | tree==14)

species <- list(list("apple",
                     c(2,7,12,3,
                       5,11,6,8,
                       10,1,4,9,
                       13,
                       17,15,18,
                       20,19,14),
                     c("Bud.9-1", "Bud.9-2", "Bud.9-3", "Bud.9-4", 
                       "CG.3041-1", "CG.3041-2", "CG.3041-3", "CG.3041-4", 
                       "CG.6210-1", "CG.6210-2", "CG.6210-3", "CG.6210-4", 
                       "M.26", 
                       "JM.8-1", "JM.8-2", "JM.8-3",
                       "PiAu.5683-1", "PiAu.5683-2", "PiAu.5683-3")),
                list("cherry", 
                     c(7,13,15,1,10),
                     c("cherry-1", "cherry-2", "cherry-3", "cherry-4", "cherry-5")))

plus <- list(c(1,1,1,1,1,1,2), c(3,5,4,13,15,14,15), 
             c("Bud.9-4+", "CG.3041-1+", "CG.6210-3+", "M.26+", "JM.8-2+", "PiAu.5683-3+", "cherry-3+"))

flow     <- c(2, 2, 2, 
              .75, .75, .75, 
              .25, .25, .25, 
              .5, .5, .5, 
              .33, .33, .33, 
              .67, .67, .67,
              .25, .25, .25,
              2.53, 2.53,
              1, 1, 1,
              1)

elastic  <- c(.67, .67, .67, 
              .625, .625, .625, 
              .375, .375, .375, 
              .25, .25, .25, 
              .6, .6, .6, 
              .4, .4, .4, 
              .25, .25, .25, 
              2.67, 2.67,
              1, 1, 1,
              0)


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

predictions <- c("2/3 - 2", "[elastic , flow]", "", 
                 "5/8 - 3/4", "", "", 
                 "3/8 - 1/4", "","", 
                 "1/4 - 1/2", "", "", 
                 "3/5 - 1/3", "", "", 
                 "2/5 - 2/3", "", "", 
                 "1/4", "", "", 
                 "8/3", "", 
                 "1", "", "", 
                 "0 - 1")

# Output ----
output <- c()

## Tree Level
for (i in 1:3){
  output <- rbind(output, 
                  test_slope(groups[i], tree_formulas, group_data[[i]]))
}

## Branch Level by group
for (i in 4:12){
  group <- mutate(group_data[[i]], parent_diam = diameter_mm / diameter_ratio)
  output <- rbind(output, 
                  test_slope(groups[i], branch_formulas, group))
}

## Branch Level by individual
for (i in 1:2){
  spp <- branch_size[branch_size$species==species[[i]][1],]
  for (j in 1:length(species[[i]][[2]])){
    ind <- spp[spp$tree==species[[i]][[2]][j],]
    ind <- mutate(ind, parent_diam = diameter_mm / diameter_ratio)
    output <- rbind(output, 
                    test_slope(species[[i]][[3]][j], branch_formulas, ind))
  }
}

## Branch Level by individual PLUS (WITH extra twig data)
for (i in 1:7){
  spp <- branch_size[branch_size$species==species[[(plus[[1]][i])]][1],]
  ind <- spp[spp$tree==plus[[2]][i],]
  ind <- mutate(ind,
                parent_diam = diameter_mm / diameter_ratio,
                segment_mass = stem_m + twig_m,
                total_mass = tot_stem_m + tot_twig_m)
  output <- rbind(output, 
                  test_slope(plus[[3]][i], plus_formulas, ind))
}
    
output <- rbind(c('predictions', predictions), output)
colnames(output) <- c('group', relationships)

SMA_test_out <- rbind(
                    output[1:6,], output[33:35,], output[44,],output[36:37,], output[7:8,],
                    output[14:17,], output[38,], output[9,], output[18,], output[39,], 
                    output[19:21,], output[10,], output[22:24,], output[40,], output[25:26,], 
                    output[41,], output[12,], output[27:28,], output[42,], output[29,], 
                    output[13,], output[30:32,], output[43,])
#write.csv(SMA_test_out, "SMAResults.csv")