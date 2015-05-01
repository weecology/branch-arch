# This script builds a table of SMA predictions for tree and branch level for 
# all NC-140 rootstocks, individuals, and branch order groups.

# Functions ----

get_sma_row <- function(name, data, plus = FALSE){
  rm_zero <- subset(data,length_cm>0 & stem_m>0)
  
  sma_row <- matrix(ncol = 28, nrow = 1)
  
  sma_row[1]  = name
  
  if (plus == FALSE){
    sma_row[2]  = output(sma(log10(data$length_cm)~log10(data$diameter_mm)))
    sma_row[3]  = output(sma(log10(data$path_length)~log10(data$diameter_mm)))
    sma_row[4]  = output(sma(log10(data$tot_length)~log10(data$diameter_mm)))
    sma_row[5]  = output(sma(log10(data$area)~log10(data$volume)))
    sma_row[6]  = output(sma(log10(data$path_area)~log10(data$path_volume)))
    sma_row[7]  = output(sma(log10(data$tot_area)~log10(data$tot_volume)))
    sma_row[8]  = output(sma(log10(data$diameter_mm)~log10(data$volume)))
    sma_row[9]  = output(sma(log10(data$diameter_mm)~log10(data$path_volume)))
    sma_row[10] = output(sma(log10(data$diameter_mm)~log10(data$tot_volume)))
    sma_row[11] = output(sma(log10(data$length_cm)~log10(data$volume)))
    sma_row[12] = output(sma(log10(data$path_length)~log10(data$path_volume)))
    sma_row[13] = output(sma(log10(data$tot_length)~log10(data$tot_volume)))
    sma_row[14] = output(sma(log10(data$diameter_mm)~log10(data$area)))
    sma_row[15] = output(sma(log10(data$diameter_mm)~log10(data$path_area)))
    sma_row[16] = output(sma(log10(data$diameter_mm)~log10(data$tot_area)))
    sma_row[17] = output(sma(log10(data$length_cm)~log10(data$area)))
    sma_row[18] = output(sma(log10(data$path_length)~log10(data$path_area)))
    sma_row[19] = output(sma(log10(data$tot_length)~log10(data$tot_area)))
    sma_row[20] = output(sma(log10(rm_zero$length_cm)~log10(rm_zero$stem_m)))
    sma_row[21] = output(sma(log10(data$path_length)~log10(data$tot_stem_m)))
    sma_row[22] = output(sma(log10(data$tot_length)~log10(data$tot_stem_m)))
    sma_row[23] = output(sma(log10(rm_zero$stem_m)~log10(rm_zero$diameter_mm)))
    sma_row[24] = output(sma(log10(data$tot_stem_m)~log10(data$diameter_mm)))
    sma_row[25] = output(sma(log10(rm_zero$stem_m)~log10(rm_zero$volume)))
    sma_row[26] = output(sma(log10(rm_zero$stem_m)~log10(rm_zero$path_volume)))
    sma_row[27] = output(sma(log10(data$tot_stem_m)~log10(data$tot_volume)))
    sma_row[28] = output(sma(log10(data$diameter_ratio)~log10(data$diameter_mm/data$diameter_ratio)))
  } else {
    sma_row[2]  = output(sma(log10(ind$length_cm)~log10(ind$diameter_mm)))
    sma_row[3]  = output(sma(log10(ind$path_length_plus)~log10(ind$diameter_mm)))
    sma_row[4]  = output(sma(log10(ind$tot_length_plus)~log10(ind$diameter_mm)))
    sma_row[5]  = output(sma(log10(ind$area_plus)~log10(ind$volume_plus)))
    sma_row[6]  = "-"
    sma_row[7]  = output(sma(log10(ind$tot_area_plus)~log10(ind$tot_volume_plus)))
    sma_row[8]  = output(sma(log10(ind$diameter_mm)~log10(ind$volume_plus)))
    sma_row[9]  = "-"
    sma_row[10] = output(sma(log10(ind$diameter_mm)~log10(ind$tot_volume_plus)))
    sma_row[11] = output(sma(log10(ind$length_cm)~log10(ind$volume_plus)))
    sma_row[12] = output(sma(log10(ind$path_length_plus)~log10(ind$tot_volume_plus)))
    sma_row[13] = output(sma(log10(ind$tot_length_plus)~log10(ind$tot_volume_plus)))
    sma_row[14] = output(sma(log10(ind$diameter_mm)~log10(ind$area_plus)))
    sma_row[15] = "-"
    sma_row[16] = output(sma(log10(ind$diameter_mm)~log10(ind$tot_area_plus)))
    sma_row[17] = output(sma(log10(ind$length_cm)~log10(ind$area_plus)))
    sma_row[18] = output(sma(log10(ind$path_length_plus)~log10(ind$tot_area_plus)))
    sma_row[19] = output(sma(log10(ind$tot_length_plus)~log10(ind$tot_area_plus)))
    sma_row[20] = output(sma(log10(rm_zero$length_cm)~log10(rm_zero$stem_m + rm_zero$twig_m)))
    sma_row[21] = output(sma(log10(ind$path_length_plus)~log10(ind$tot_stem_m + ind$tot_twig_m)))
    sma_row[22] = output(sma(log10(ind$tot_length_plus)~log10(ind$tot_stem_m + ind$tot_twig_m)))
    sma_row[23] = output(sma(log10(rm_zero$stem_m + rm_zero$twig_m)~log10(rm_zero$diameter_mm)))
    sma_row[24] = output(sma(log10(ind$tot_stem_m + ind$tot_twig_m)~log10(ind$diameter_mm)))
    sma_row[25] = output(sma(log10(rm_zero$stem_m + rm_zero$twig_m)~log10(rm_zero$volume_plus)))
    sma_row[26] = "-"
    sma_row[27] = output(sma(log10(ind$tot_stem_m + ind$tot_twig_m)~log10(ind$tot_volume_plus)))
    sma_row[28] = output(sma(log10(ind$diameter_ratio)~log10(ind$diameter_mm/ind$diameter_ratio)))
  }
  return(sma_row)
}

output <- function(sma_data){
  return (paste(round(sma(sma_data)$coef[[1]][2,1],2), " [ ",  # exponent
                round(sma(sma_data)$coef[[1]][2,2],2), " , ",  # 95% -
                round(sma(sma_data)$coef[[1]][2,3],2), " ]; ", # 95% + 
                round(sma(sma_data)$r2[[1]], 3), sep = ""))    # R2
                #round(coef.sma(sma_data)[1],2)                # intercept
} 


# Data and Set-up ----

library('smatr')

treesum <- read.csv("TreeSummary.csv", sep = ",", head=T)
branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)

groups <- c("all-branch", "Bud.9", "CG.3041", "CG.6210", "M.26", "JM.8", "PiAu.5683")
branch_data <- list()
branch_data[[1]] <- branch_size[branch_size$species=="apple",]
branch_data[[2]] <- subset(branch_data[[1]], tree==2 | tree==7 | tree==12 | tree ==3)
branch_data[[3]] <- subset(branch_data[[1]], tree==5 | tree==11 | tree==6 | tree ==8)
branch_data[[4]] <- subset(branch_data[[1]], tree==10 | tree==1 | tree==4 | tree ==9)
branch_data[[5]] <- subset(branch_data[[1]], tree==13)
branch_data[[6]] <- subset(branch_data[[1]], tree==17 | tree==15 | tree==18)
branch_data[[7]] <- subset(branch_data[[1]], tree==20 | tree==19 | tree==14)

tree_ids   <- c(2,7,12,3,
                5,11,6,8,
                10,1,4,9,
                13,
                17,15,18,
                20,19,14)

tree_names <- c("Bud.9-1", "Bud.9-2", "Bud.9-3", "Bud.9-4", 
                "CG.3041-1", "CG.3041-2", "CG.3041-3", "CG.3041-4", 
                "CG.6210-1", "CG.6210-2", "CG.6210-3", "CG.6210-4", 
                "M.26", 
                "JM.8-1", "JM.8-2", "JM.8-3",
                "PiAu.5683-1", "PiAu.5683-2", "PiAu.5683-3")

plus_ids   <- c(3,5,4,13,15,14)

plus_names <- c("Bud.9-4+", "CG.3041-1+", "CG.6210-3+", "M.26+", "JM.8-2+", "PiAu.5683-3+")

sma_test <- matrix(nrow = 2, ncol = 28)
colnames(sma_test) = c('group', "L~D (Segment)", "(Path)", "(Subtree)", 
                       "SA~V (Segment)", "(Path)", "(Subtree)", 
                       "D~V(Segment)", "(Path)", "(Subtree)", 
                       "L~V (Segment)", "(Path)", "(Subtree)", 
                       "D~SA (Segment)", "(Path)", "(Subtree)", 
                       "L~SA (Segment)", "(Path)", "(Subtree)", 
                       "L~M (Segment)", "(Path)", "(Subtree)", 
                       "M~D (Segment)", "(Subtree)", 
                       "M~V(Segment)", "(Path)", "(Subtree)", 
                       "D/P Ratio ~ P Diam")

sma_test[1, ] = c("prediction", "2 - 2/3", "", "", 
                  "3/4 - 5/8", "", "", 
                  "1/4 - 3/8", "","", 
                  "1/2 - 1/4", "", "", 
                  "1/3 - 3/5", "", "", 
                  "2/3 - 2/5", "", "", 
                  "1/4", "", "", 
                  "8/3", "", 
                  "1", "", "", 
                  "0 - 1")


# Tree Level Output ----
tree_data <- treesum[treesum$species=="apple",]

sma_test[2, 1] = 'all-tree'

test <- sma(log10(tree_data$height)~log10(tree_data$trunk_diam))
sma_test[2, 2] = output(test)

test <- sma(log10(tree_data$max_path)~log10(tree_data$trunk_diam))
sma_test[2, 3] = output(test)

test <- sma(log10(tree_data$tot_length)~log10(tree_data$trunk_diam))
sma_test[2, 4] = output(test)

sma_test[2, 5] = "-"

sma_test[2, 6] = "-"

test <- sma(log10(tree_data$tot_area)~log10(tree_data$tot_volume))
sma_test[2, 7] = output(test)

sma_test[2, 8] = "-"

sma_test[2, 9] = "-"

test <- sma(log10(tree_data$trunk_diam)~log10(tree_data$tot_volume))
sma_test[2, 10] = output(test)

test <- sma(log10(tree_data$height)~log10(tree_data$tot_volume))
sma_test[2, 11] = output(test)

test <- sma(log10(tree_data$max_path)~log10(tree_data$tot_volume))
sma_test[2, 12] = output(test)

test <- sma(log10(tree_data$tot_length)~log10(tree_data$tot_volume))
sma_test[2, 13] = output(test)

sma_test[2, 14] = "-"

sma_test[2, 15] = "-"

test <- sma(log10(tree_data$trunk_diam)~log10(tree_data$tot_area))
sma_test[2, 16] = output(test)

test <- sma(log10(tree_data$height)~log10(tree_data$tot_area))
sma_test[2, 17] = output(test)

test <- sma(log10(tree_data$max_path)~log10(tree_data$tot_area))
sma_test[2, 18] = output(test)

test <- sma(log10(tree_data$tot_length)~log10(tree_data$tot_area))
sma_test[2, 19] = output(test)

test <- sma(log10(tree_data$height)~log10(tree_data$tot_stem_m))
sma_test[2, 20] = output(test)

test <- sma(log10(tree_data$max_path)~log10(tree_data$tot_stem_m))
sma_test[2, 21] = output(test)

test <- sma(log10(tree_data$tot_length)~log10(tree_data$tot_stem_m))
sma_test[2, 22] = output(test)

sma_test[2, 23] = "-"

test <- sma(log10(tree_data$tot_stem_m)~log10(tree_data$trunk_diam))
sma_test[2, 24] = output(test)

sma_test[2, 25] = "-"

sma_test[2, 26] = "-"

test <- sma(log10(tree_data$tot_stem_m)~log10(tree_data$tot_volume))
sma_test[2, 27] = output(test)

sma_test[2, 28] = "-"


# Branch Level Output ----
## by rootstock
for (i in 1:7){
  sma_test = rbind(sma_test, get_sma_row(groups[i], branch_data[[i]]))
}  

## by individual
for (i in 1:length(tree_ids)){
  ind <- branch_data[[1]][branch_data[[1]]$tree==tree_ids[i],]
  sma_test = rbind(sma_test, get_sma_row(tree_names[i], ind))
}

## by individual PLUS (WITH extra twig data) 
for (i in 1:6){
  ind <- branch_data[[1]][branch_data[[1]]$tree==plus_ids[i],]
  sma_test = rbind(sma_test, get_sma_row(plus_names[i], ind))
}

# Sort and Write Results ----
sma_test_out <- rbind(sma_test[1:4, ], sma_test[10:13, ], sma_test[29, ], sma_test[5, ], sma_test[14, ],
                      sma_test[30, ], sma_test[15:17, ], sma_test[6, ], sma_test[18:20, ], sma_test[31, ], 
                      sma_test[21, ], sma_test[7, ], sma_test[32, ], sma_test[8, ], sma_test[23:24, ], 
                      sma_test[33, ], sma_test[25, ], sma_test[9, ], sma_test[26:28, ], sma_test[34, ])

write.csv(sma_test_out, "SMAResults.csv")



# Group by Order Branch Level Output ----
orders <- list(c(" - structure", " - fruiting"))

for (j in 4:12){
  
  orders[[2]] <- group_data[[j]][group_data[[j]]$order<3,]
  orders[[3]] <- group_data[[j]][group_data[[j]]$order>=3,]
  
  for (i in 2:3){
    
    subout <- matrix(ncol = 24, nrow = 1)
    
    subout[1] = paste(groups[j], orders[[1]][(i-1)], " (", length(orders[[i]][,1]), ")")
    
    if (length(orders[[i]][,1]) > 2){
      
      test <- sma(log10(orders[[i]]$length_cm)~log10(orders[[i]]$diameter_mm))
      subout[2] = output(test)
      
      test <- sma(log10(orders[[i]]$path_length)~log10(orders[[i]]$diameter_mm))
      subout[3] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_length)~log10(orders[[i]]$diameter_mm))
      subout[4] = output(test)
      
      test <- sma(log10(orders[[i]]$area)~log10(orders[[i]]$volume))
      subout[5] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_area)~log10(orders[[i]]$tot_volume))
      subout[6] = output(test)
      
      test <- sma(log10(orders[[i]]$diameter_mm)~log10(orders[[i]]$volume))
      subout[7] = output(test)
      
      test <- sma(log10(orders[[i]]$diameter_mm)~log10(orders[[i]]$tot_volume))
      subout[8] = output(test)
      
      test <- sma(log10(orders[[i]]$length_cm)~log10(orders[[i]]$volume))
      subout[9] = output(test)
      
      test <- sma(log10(orders[[i]]$path_length)~log10(orders[[i]]$tot_volume))
      subout[10] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_length)~log10(orders[[i]]$tot_volume))
      subout[11] = output(test)
      
      test <- sma(log10(orders[[i]]$diameter_mm)~log10(orders[[i]]$area))
      subout[12] = output(test)
      
      test <- sma(log10(orders[[i]]$diameter_mm)~log10(orders[[i]]$tot_area))
      subout[13] = output(test)
      
      test <- sma(log10(orders[[i]]$length_cm)~log10(orders[[i]]$area))
      subout[14] = output(test)
      
      test <- sma(log10(orders[[i]]$path_length)~log10(orders[[i]]$tot_area))
      subout[15] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_length)~log10(orders[[i]]$tot_area))
      subout[16] = output(test)
      
      rm_zero <- subset(orders[[i]],length_cm>0 & stem_m>0)
      test <- sma(log10(rm_zero$length_cm)~log10(rm_zero$stem_m))
      subout[17] = output(test)
      
      test <- sma(log10(orders[[i]]$path_length)~log10(orders[[i]]$tot_stem_m))
      subout[18] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_length)~log10(orders[[i]]$tot_stem_m))
      subout[19] = output(test)
      
      test <- sma(log10(rm_zero$stem_m)~log10(rm_zero$diameter_mm))
      subout[20] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_stem_m)~log10(orders[[i]]$diameter_mm))
      subout[21] = output(test)
      
      test <- sma(log10(rm_zero$stem_m)~log10(rm_zero$volume))
      subout[22] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_stem_m)~log10(orders[[i]]$tot_volume))
      subout[23] = output(test)
      
      test <- sma(log10(orders[[i]]$diameter_ratio)~log10(orders[[i]]$diameter_mm/orders[[i]]$diameter_ratio))
      subout[24] = output(test)
    }
    
    if (exists('sma_test_order'))
      sma_test_order = rbind(sma_test_order, subout)
    
    else
      sma_test_order <- subout
      colnames(sma_test_order) = c('group', "L~D (Segment)", "(Path)", "(Subtree)", "SA~V (Segment)", "(Subtree)", "D~V(Segment)", "(Subtree)", 
                                 "L~V (Segment)", "(Path)", "(Subtree)", "D~SA (Segment)", "(Subtree)", "L~SA (Segment)", "(Path)", "(Subtree)", 
                                 "L~M (Segment)", "(Path)", "(Subtree)", "M~D (Segment)", "(Subtree)", "M~V(Segment)", "(Subtree)", "D/P Ratio ~ P Diam")

  }
}

for (j in 1:5){
  spp <- branch_size[branch_size$species=="cherry",]
  ind <- spp[spp$tree==species[[2]][[2]][j],]
  orders[[2]] <- ind[ind$order<3,]
  orders[[3]] <- ind[ind$order>=3,]
  
  for (i in 2:3){
    
    subout <- matrix(ncol = 24, nrow = 1)
    
    subout[1] = paste(species[[2]][[3]][j], orders[[1]][(i-1)], " (", length(orders[[i]][,1]), ")")
    
    if (length(orders[[i]][,1]) > 2){
      
      test <- sma(log10(orders[[i]]$length_cm)~log10(orders[[i]]$diameter_mm))
      subout[2] = output(test)
      
      test <- sma(log10(orders[[i]]$path_length)~log10(orders[[i]]$diameter_mm))
      subout[3] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_length)~log10(orders[[i]]$diameter_mm))
      subout[4] = output(test)
      
      test <- sma(log10(orders[[i]]$area)~log10(orders[[i]]$volume))
      subout[5] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_area)~log10(orders[[i]]$tot_volume))
      subout[6] = output(test)
      
      test <- sma(log10(orders[[i]]$diameter_mm)~log10(orders[[i]]$volume))
      subout[7] = output(test)
      
      test <- sma(log10(orders[[i]]$diameter_mm)~log10(orders[[i]]$tot_volume))
      subout[8] = output(test)
      
      test <- sma(log10(orders[[i]]$length_cm)~log10(orders[[i]]$volume))
      subout[9] = output(test)
      
      test <- sma(log10(orders[[i]]$path_length)~log10(orders[[i]]$tot_volume))
      subout[10] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_length)~log10(orders[[i]]$tot_volume))
      subout[11] = output(test)
      
      test <- sma(log10(orders[[i]]$diameter_mm)~log10(orders[[i]]$area))
      subout[12] = output(test)
      
      test <- sma(log10(orders[[i]]$diameter_mm)~log10(orders[[i]]$tot_area))
      subout[13] = output(test)
      
      test <- sma(log10(orders[[i]]$length_cm)~log10(orders[[i]]$area))
      subout[14] = output(test)
      
      test <- sma(log10(orders[[i]]$path_length)~log10(orders[[i]]$tot_area))
      subout[15] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_length)~log10(orders[[i]]$tot_area))
      subout[16] = output(test)
      
      rm_zero <- subset(orders[[i]],length_cm>0 & stem_m>0)
      test <- sma(log10(rm_zero$length_cm)~log10(rm_zero$stem_m))
      subout[17] = output(test)
      
      test <- sma(log10(orders[[i]]$path_length)~log10(orders[[i]]$tot_stem_m))
      subout[18] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_length)~log10(orders[[i]]$tot_stem_m))
      subout[19] = output(test)
      
      test <- sma(log10(rm_zero$stem_m)~log10(rm_zero$diameter_mm))
      subout[20] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_stem_m)~log10(orders[[i]]$diameter_mm))
      subout[21] = output(test)
      
      test <- sma(log10(rm_zero$stem_m)~log10(rm_zero$volume))
      subout[22] = output(test)
      
      test <- sma(log10(orders[[i]]$tot_stem_m)~log10(orders[[i]]$tot_volume))
      subout[23] = output(test)
      
      test <- sma(log10(orders[[i]]$diameter_ratio)~log10(orders[[i]]$diameter_mm/orders[[i]]$diameter_ratio))
      subout[24] = output(test)
    }
    
    sma_test_order = rbind(sma_test_order, subout)
  }
}

sma_test_order_out <- rbind(sma_test_order[1:4,], sma_test_order[19:28,], sma_test_order[5:18,])
write.csv(sma_test_order_out, "SMAResults_Order.csv")

#For output figures as.numeric(strsplit(sma_test[2,3], " ")[[1]][1])
