#This script builds a table of SMA predictions for all, species, and rootstock level.

treesum <- read.csv("TreeSummary.csv", sep = ",", head=T)
branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)
library('smatr')

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
                     c(2,7,12,3,5,11,6,8,10,1,4,9,13,17,15,18,20,19,14),
                     c("Bud.9", "Bud.9", "Bud.9", "Bud.9", "CG.3041", "CG.3041",  
                       "CG.3041", "CG.3041", "CG.6210", "CG.6210", "CG.6210", 
                       "CG.6210", "M.26", "JM.8", "JM.8", "JM.8",
                       "PiAu.5683", "PiAu.5683", "PiAu.5683")),
                list("cherry", 
                     c(7,13,15,1,10),
                     "P. mahaleb"))

output <- function(sma_data){
  return (paste(round(sma(sma_data)$coef[[1]][2,1],2), " [", round(sma(sma_data)$coef[[1]][2,2],2), ", ", 
               round(sma(sma_data)$coef[[1]][2,3],2), "]; ", round(sma(sma_data)$r2[[1]], 3), sep = ""))
}     #insert for intercept value <round(coef.sma(sma_data)[1],2), "; ",>

sma_test <- matrix(nrow = 12, ncol = 18)
colnames(sma_test) = c('group', "L~D (Segment)", "(Path)", "(Subtree)", "SA~V", "D~V", 
                       "L~V (Segment)", "(Path)", "(Subtree)", "D~SA", "L~SA (Segment)", "(Path)", "(Subtree)", 
                       "L~M (Segment)", "(Path)", "(Subtree)", "M~D", "M~V")

sma_test[1,] = c("prediction", "2 - 2/3", "", "", "3/4 - 5/8", "1/4 - 3/8", "1/2 - 1/4", "", "", "1/3 - 3/5", "2/3 - 2/5",
                 "", "", "", "", "", "", "")

for (i in 1:3){
  sma_test[(i+1),1] = groups[i]
  
  test <- sma(log10(group_data[[i]]$height)~log10(group_data[[i]]$trunk_diam))
  sma_test[(i+1),2] = output(test)
  
  test <- sma(log10(group_data[[i]]$max_path)~log10(group_data[[i]]$trunk_diam))
  sma_test[(i+1),3] = output(test)
  
  test <- sma(log10(group_data[[i]]$tot_length)~log10(group_data[[i]]$trunk_diam))
  sma_test[(i+1),4] = output(test)
  
  test <- sma(log10(group_data[[i]]$tot_area)~log10(group_data[[i]]$tot_volume))
  sma_test[(i+1),5] = output(test)
  
  test <- sma(log10(group_data[[i]]$trunk_diam)~log10(group_data[[i]]$tot_volume))
  sma_test[(i+1),6] = output(test)
  
  test <- sma(log10(group_data[[i]]$height)~log10(group_data[[i]]$tot_volume))
  sma_test[(i+1),7] = output(test)
  
  test <- sma(log10(group_data[[i]]$max_path)~log10(group_data[[i]]$tot_volume))
  sma_test[(i+1),8] = output(test)
  
  test <- sma(log10(group_data[[i]]$tot_length)~log10(group_data[[i]]$tot_volume))
  sma_test[(i+1),9] = output(test)
  
  test <- sma(log10(group_data[[i]]$trunk_diam)~log10(group_data[[i]]$tot_area))
  sma_test[(i+1),10] = output(test)
  
  test <- sma(log10(group_data[[i]]$height)~log10(group_data[[i]]$tot_area))
  sma_test[(i+1),11] = output(test)
  
  test <- sma(log10(group_data[[i]]$max_path)~log10(group_data[[i]]$tot_area))
  sma_test[(i+1),12] = output(test)
  
  test <- sma(log10(group_data[[i]]$tot_length)~log10(group_data[[i]]$tot_area))
  sma_test[(i+1),13] = output(test)
  
  test <- sma(log10(group_data[[i]]$height)~log10(group_data[[i]]$tot_stem_m))
  sma_test[(i+1),14] = output(test)
  
  test <- sma(log10(group_data[[i]]$max_path)~log10(group_data[[i]]$tot_stem_m))
  sma_test[(i+1),15] = output(test)
  
  test <- sma(log10(group_data[[i]]$tot_length)~log10(group_data[[i]]$tot_stem_m))
  sma_test[(i+1),16] = output(test)
  
  test <- sma(log10(group_data[[i]]$tot_stem_m)~log10(group_data[[i]]$trunk_diam))
  sma_test[(i+1),17] = output(test)
  
  test <- sma(log10(group_data[[i]]$tot_stem_m)~log10(group_data[[i]]$tot_volume))
  sma_test[(i+1),18] = output(test)

}



for (i in 4:12){
  
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

write.csv(sma_test, "SMAResults.csv")
