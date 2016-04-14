# This script builds a table of SMA predictions for tree and branch level for 
# all NC-140 rootstocks, individuals, and branch order groups.

# Hort Relevant Output ---
library('smatr')

output_hort <- function(sma_data){
  return(paste(round(sma_data$coef[[1]][1,1],4),  # intercept
               round(sma_data$coef[[1]][1,2],4),  # int 95% -
               round(sma_data$coef[[1]][1,3],4),  # int 95% + 
               round(sma_data$coef[[1]][2,1],4),  # exponent
               round(sma_data$coef[[1]][2,2],4),  # exp 95% -
               round(sma_data$coef[[1]][2,3],4),  # exp 95% + 
               round(sma_data$r2[[1]], 3),        # R2
               sep = ";")) 
} 

tree_data <- read.csv("TreeSummary.csv", sep = ",", head=T)
yield <- read.csv("AppleYield.csv", sep =',', head=T)
tree_yield <- dplyr::inner_join(tree_data, yield)

sma_test_hort <- matrix(nrow = 7, ncol = 7)
colnames(sma_test_hort) = c('group', "Stem Biomass ~ TCSA", "Height ~ TCSA", 
                       "Stem Biomass ~ Height", "Stem Volume ~ TCSA",
                       "Stem Biomass ~ Stem Volume", "Cumulative Yield ~ TCSA")

sma_test_hort[, 1] <- c("All", "Bud.9", "CG.3041", "CG.6210", "M.26", "JM.8", "PiAu.5683")

hort_data <- list()
hort_data[[1]] <- tree_yield
hort_data[[2]] <- subset(hort_data[[1]], tree==2 | tree==7 | tree==12 | tree ==3)
hort_data[[3]] <- subset(hort_data[[1]], tree==5 | tree==11 | tree==6 | tree ==8)
hort_data[[4]] <- subset(hort_data[[1]], tree==10 | tree==1 | tree==4 | tree ==9)
hort_data[[5]] <- subset(hort_data[[1]], tree==13)
hort_data[[6]] <- subset(hort_data[[1]], tree==17 | tree==15 | tree==18)
hort_data[[7]] <- subset(hort_data[[1]], tree==20 | tree==19 | tree==14)

for (i in 1:7){
  group_hort <- hort_data[[i]]
  
  if(i==5){
    test <- log10((group_hort$tot_stem_m + group_hort$tot_twig_m)) / 
      log10(pi*(group_hort$trunk_diam_cm/2)^2)
    sma_test_hort[i, 2] = paste(";;;", round(test,4), ";;;", sep="")
    
    test <- log10(group_hort$height) / log10(pi*(group_hort$trunk_diam_cm/2)^2)
    sma_test_hort[i, 3] = paste(";;;", round(test,4), ";;;", sep="")
    
    test <- log10((group_hort$tot_stem_m + group_hort$tot_twig_m)) / 
      log10(group_hort$height)
    sma_test_hort[i, 4] = paste(";;;", round(test,4), ";;;", sep="")
    
    test <- log10(group_hort$tot_volume) / 
      log10(pi*(group_hort$trunk_diam_cm/2)^2)
    sma_test_hort[i, 5] = paste(";;;", round(test,4), ";;;", sep="")
    
    test <- log10((group_hort$tot_stem_m + group_hort$tot_twig_m)) / 
      log10(group_hort$tot_volume)
    sma_test_hort[i, 6] = paste(";;;", round(test,4), ";;;", sep="")
    
    test <- log10(group_hort$cum_yield) / 
      log10(pi*(group_hort$trunk_diam_cm/2)^2)
    sma_test_hort[i, 7] = paste(";;;", round(test,4), ";;;", sep="")
    
  } else {
    test <- sma(log10((tot_stem_m + tot_twig_m)) ~ log10(pi*(trunk_diam_cm/2)^2), data = group_hort)
    sma_test_hort[i, 2] = output_hort(test)
    
    test <- sma(log10(height) ~ log10(pi*(trunk_diam_cm/2)^2), data = group_hort)
    sma_test_hort[i, 3] = output_hort(test)
    
    test <- sma(log10((tot_stem_m + tot_twig_m)) ~ log10(height), data = group_hort)
    sma_test_hort[i, 4] = output_hort(test)
    
    test <- sma(log10((tot_volume)) ~ log10(pi*(trunk_diam_cm/2)^2), data = group_hort)
    sma_test_hort[i, 5] = output_hort(test)
    
    test <- sma(log10((tot_stem_m + tot_twig_m)) ~ log10(tot_volume), data = group_hort)
    sma_test_hort[i, 6] = output_hort(test)
    
    test <- sma(log10(cum_yield) ~ log10(pi*(trunk_diam_cm/2)^2), data = group_hort)
    sma_test_hort[i, 7] = output_hort(test)
  }
}  