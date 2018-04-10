### This script builds a table of allometry predictions (L~D, D~M)
### at branch levels for cherry and apples. The results are reported in the
### manuscript.

library('smatr')

test_slope <- function(name, formulas, data){  
  sma_row <- c(name)
  for (i in 1:length(formulas)) {
      test <- sma(formulas[i][[1]], log = 'xy', data = data)
      sma_row <- c(sma_row, params_out(test))
  }
  return(sma_row)
}

params_out <- function(sma_data){
  return (paste("a: ", round(sma_data$coef[[1]][2,1],2), " [ ", 
                round(sma_data$coef[[1]][2,2],2), " , ", 
                round(sma_data$coef[[1]][2,3],2), " ]; b: ",
                round(sma_data$coef[[1]][1,1],2), " [ ", 
                round(sma_data$coef[[1]][1,2],2), " , ", 
                round(sma_data$coef[[1]][1,3],2), " ]; ", 
                round(sma_data$r2[[1]], 3), sep = ""))
} 

branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)
group_data <- list()
group_data[[1]] <- branch_size[branch_size$species=="cherry",]
group_data[[2]] <- branch_size[branch_size$species=="apple",]

groups <- c("cherry", "apple")
relationships <- c("L~D (Segment)", "(Path)", "(Subtree)", 
                   "M~D (Segment)", "(Path)", "(Subtree)")
branch_formulas <- c(length_cm   ~ diameter_mm, 
                     path_length ~ diameter_mm, 
                     tot_length  ~ diameter_mm,
                     stem_m      ~ diameter_mm,
                     path_stem_m ~ diameter_mm,
                     tot_stem_m  ~ diameter_mm)

output <- c()
for (i in 1:2){
  output <- rbind(output, test_slope(groups[i], branch_formulas, 
                                     group_data[[i]]))
}
    
colnames(output) <- c('group', relationships)

write.csv(output, "allometries.csv")