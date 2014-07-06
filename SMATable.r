#This script builds a table of SMA predictions for all, species, and rootstock level.

treesum <- read.csv("TreeSummary.csv", sep = ",", head=T)
branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)

groups <- c("all-tree", "all-branch", "cherry", "apple", "Bud.9", "CG.3041", "CG.6210", "M.26", "JM.8", "PiAu.5683")
group_data <- list()
group_data[[1]] <- treesum
group_data[[2]] <- branch_size
group_data[[3]] <- branch_size[branch_size$species=="cherry",]
group_data[[4]] <- branch_size[branch_size$species=="apple",]
group_data[[5]] <- subset(group_data[[4]], tree==2 | tree==7 | tree==12 | tree ==3)
group_data[[6]] <- subset(group_data[[4]], tree==5 | tree==11 | tree==6 | tree ==8)
group_data[[7]] <- subset(group_data[[4]], tree==10 | tree==1 | tree==4 | tree ==9)
group_data[[8]] <- subset(group_data[[4]], tree==13)
group_data[[9]] <- subset(group_data[[4]], tree==17 | tree==15 | tree==18)
group_data[[10]] <- subset(group_data[[4]], tree==20 | tree==19 | tree==14)

output <- function(sma_data){
  return (paste(round(coef.sma(sma_data)[2],2), " [", round(sma(sma_data)$coef[[1]][2,2],2), ", ", 
               round(sma(sma_data)$coef[[1]][2,3],2), "], ", round(coef.sma(sma_data)[1],2), "; ", 
               round(sma(sma_data)$r2[[1]], 3), sep = ""))
}

sma_test <- matrix(nrow = 11, ncol = 5)
colnames(sma_test) = c('group', "M~D", "L~D", "L~M", "D(k+1)/D(k)")

sma_test[1,] = c("prediction", 2.67, 0.67, 0.25, 0.5)

sma_test[2,1] = groups[1]

test <- sma(log10(group_data[[1]]$tot_stem_m)~log10(group_data[[1]]$trunk_diam))
sma_test[2,2] = output(test)

test <- sma(log10(group_data[[1]]$height)~log10((group_data[[1]]$trunk_diam/2)))
sma_test[2,3] = output(test)

test <- sma(log10(group_data[[1]]$height)~log10(group_data[[1]]$tot_stem_m))
sma_test[2,4] = output(test)

sma_test[2,5] = '-'

for (i in 2:10){
  
  sma_test[(i+1),1] = groups[i]
  
  test <- sma(log10(group_data[[i]]$tot_stem_m)~log10(group_data[[i]]$diameter_mm))
  sma_test[(i+1),2] = output(test)
  
  test <- sma(log10(group_data[[i]]$path_length)~log10((group_data[[i]]$diameter_mm/2)))
  sma_test[(i+1),3] = output(test)
  
  test <- sma(log10(group_data[[i]]$path_length)~log10(group_data[[i]]$tot_stem_m))
  sma_test[(i+1),4] = output(test)
  
  dratio_temp_na <- subset(group_data[[i]], select = c(diameter_mm, diameter_ratio))
  dratio_temp <- na.omit(dratio_temp_na)
  parent_diam <- dratio_temp$diameter_mm/dratio_temp$diameter_ratio
  
  test <- sma(dratio_temp$diameter_mm~parent_diam)
  sma_test[(i+1),5] = output(test)
}
