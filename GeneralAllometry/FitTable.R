#This script builds a table of Linear and polynomial fits for all, species, and rootstock level.

treesum <- read.csv("TreeSummary.csv", sep = ",", head=T)
branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)

fit <- function(y, x){
  lm <- lm(y~x)
  poly <- lm(y~poly(x, 2, raw = T))
  return (list( c( summary(lm)$coefficients[2,1], summary(lm)$r.squared, AIC(lm), length(y)),
               c( summary(poly)$coefficients[2,1], summary(poly)$coefficients[3,1], summary(poly)$r.squared, 
                  AIC(poly), summary(poly)$coefficients[3,2])))
}

output <- function(fit_data){
  subout <- matrix (nrow = 2, ncol = 1)
  subout[1,1] = paste(round(fit_data[[1]][1],2), " ,         , ", round(fit_data[[1]][2],3), " , ", round(fit_data[[1]][3],2))
  subout[2,1] = paste(round(fit_data[[2]][1],2), " , ", round(fit_data[[2]][2],2)," , ", round(fit_data[[2]][3],3), " , ", round(fit_data[[2]][4],2))
  return (subout)
}

report <- function(fit_data){
  if(fit_data[[1]][3] < fit_data[[2]][4]){value = 3}
  else{ 
    if( (abs(fit_data[[2]][2]) - 2*fit_data[[2]][5]) > 0){value = 0}
    else{value = 1}
  }
  return (paste(value, ",", round(fit_data[[1]][2],3), ",", fit_data[[1]][4])) #return(value) for image()
}

fits_to_row <- function(subout, ind){
  subout <- cbind(subout, output(fit(log10(ind$length_cm), log10(ind$diameter_mm))))
  
  subout <- cbind(subout, output(fit(log10(ind$path_length), log10(ind$diameter_mm))))
  
  subout <- cbind(subout, output(fit(log10(ind$tot_length), log10(ind$diameter_mm))))
  
  subout <- cbind(subout, output(fit(log10(ind$area), log10(ind$volume))))
  
  subout <- cbind(subout, output(fit(log10(ind$tot_area), log10(ind$tot_volume))))
  
  subout <- cbind(subout, output(fit(log10(ind$diameter_mm), log10(ind$volume))))
  
  subout <- cbind(subout, output(fit(log10(ind$diameter_mm), log10(ind$tot_volume))))
  
  subout <- cbind(subout, output(fit(log10(ind$length_cm), log10(ind$volume))))
  
  subout <- cbind(subout, output(fit(log10(ind$path_length), log10(ind$tot_volume))))
  
  subout <- cbind(subout, output(fit(log10(ind$tot_length), log10(ind$tot_volume))))
  
  subout <- cbind(subout, output(fit(log10(ind$diameter_mm), log10(ind$area))))
  
  subout <- cbind(subout, output(fit(log10(ind$diameter_mm), log10(ind$tot_area))))
  
  subout <- cbind(subout, output(fit(log10(ind$length_cm), log10(ind$area))))
  
  subout <- cbind(subout, output(fit(log10(ind$path_length), log10(ind$tot_area))))
  
  subout <- cbind(subout, output(fit(log10(ind$tot_length), log10(ind$tot_area))))
  
  subout <- cbind(subout, output(fit(log10(ind$length_cm), log10(ind$stem_m))))
  
  subout <- cbind(subout, output(fit(log10(ind$path_length), log10(ind$tot_stem_m))))
  
  subout <- cbind(subout, output(fit(log10(ind$tot_length), log10(ind$tot_stem_m))))
  
  subout <- cbind(subout, output(fit(log10(ind$stem_m), log10(ind$diameter_mm))))
  
  subout <- cbind(subout, output(fit(log10(ind$tot_stem_m), log10(ind$diameter_mm))))
  
  subout <- cbind(subout, output(fit(log10(ind$stem_m), log10(ind$volume))))
  
  subout <- cbind(subout, output(fit(log10(ind$tot_stem_m), log10(ind$tot_volume))))
}

reports_to_col <- function(ind){
  subout <- matrix(nrow = 22, ncol = 1)
  
  subout[1,] <- report(fit(log10(ind$length_cm), log10(ind$diameter_mm)))
  
  subout[2,] <- report(fit(log10(ind$path_length), log10(ind$diameter_mm)))
  
  subout[3,] <- report(fit(log10(ind$tot_length), log10(ind$diameter_mm)))
  
  subout[4,] <- report(fit(log10(ind$area), log10(ind$volume)))

  subout[5,] <- report(fit(log10(ind$tot_area), log10(ind$tot_volume)))
  
  subout[6,] <- report(fit(log10(ind$diameter_mm), log10(ind$volume)))
  
  subout[7,] <- report(fit(log10(ind$diameter_mm), log10(ind$tot_volume)))
  
  subout[8,] <- report(fit(log10(ind$length_cm), log10(ind$volume)))
  
  subout[9,] <- report(fit(log10(ind$path_length), log10(ind$tot_volume)))
  
  subout[10,] <- report(fit(log10(ind$tot_length), log10(ind$tot_volume)))
  
  subout[11,] <- report(fit(log10(ind$diameter_mm), log10(ind$area)))
  
  subout[12,] <- report(fit(log10(ind$diameter_mm), log10(ind$tot_area)))
  
  subout[13,] <- report(fit(log10(ind$length_cm), log10(ind$area)))
  
  subout[14,] <- report(fit(log10(ind$path_length), log10(ind$tot_area)))

  subout[15,] <- report(fit(log10(ind$tot_length), log10(ind$tot_area)))
  
  subout[16,] <- report(fit(log10(ind$length_cm), log10(ind$stem_m)))
  
  subout[17,] <- report(fit(log10(ind$path_length), log10(ind$tot_stem_m)))
  
  subout[18,] <- report(fit(log10(ind$tot_length), log10(ind$tot_stem_m)))
  
  subout[19,] <- report(fit(log10(ind$stem_m), log10(ind$diameter_mm)))
  
  subout[20,] <- report(fit(log10(ind$tot_stem_m), log10(ind$diameter_mm)))
  
  subout[21,] <- report(fit(log10(ind$stem_m), log10(ind$volume)))
  
  subout[22,] <- report(fit(log10(ind$tot_stem_m), log10(ind$tot_volume)))
  
  return(subout)
}

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
                     c("Bud.9-1", "Bud.9-2", "Bud.9-3", "Bud.9-4", "CG.3041-1", "CG.3041-2",  
                       "CG.3041-3", "CG.3041-4", "CG.6210-1", "CG.6210-2", "CG.6210-3", 
                       "CG.6210-4", "M.26", "JM.8-1", "JM.8-2", "JM.8-3",
                       "PiAu.5683-1", "PiAu.5683-2", "PiAu.5683-3")),
                list("cherry", 
                     c(7,13,15,1,10),
                     c("cherry-1", "cherry-2", "cherry-3", "cherry-4", "cherry-5")))

plus <- list(c(1,1,1,1,1,1,2), c(3,5,6,13,15,19,15), 
             c("Bud.9-4+", "CG.3041-1+", "CG.6210-3+", "M.26+", "JM.8-2+", "PiAu.5683-3+", "cherry-3+"))


### FIT OUTPUT
fits_test <- matrix(nrow = 1, ncol = 23)
colnames(fits_test) = c('group', "L~D (Segment)", "(Path)", "(Subtree)", "SA~V (Segment)", "(Subtree)", "D~V(Segment)", "(Subtree)", 
                        "L~V (Segment)", "(Path)", "(Subtree)", "D~SA (Segment)", "(Subtree)", "L~SA (Segment)", "(Path)", "(Subtree)", 
                        "L~M (Segment)", "(Path)", "(Subtree)", "M~D (Segment)", "(Subtree)", "M~V(Segment)", "(Subtree)")

fits_test[1,] = c("prediction", "2 - 2/3", "", "", "3/4 - 5/8", "", "1/4 - 3/8", "", "1/2 - 1/4", "", "", "1/3 - 3/5", "", "2/3 - 2/5",
                  "", "", "", "", "", "", "", "", "")


# Tree Level Output
for (i in 1:3){
  subout <- matrix(nrow = 2, ncol = 1)
  
  subout[1,1] = paste( groups[i], "- X")
  subout[2,1] = "       - X + X^2"
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$height), log10(group_data[[i]]$trunk_diam))))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$max_path), log10(group_data[[i]]$trunk_diam))))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$tot_length), log10(group_data[[i]]$trunk_diam))))
  
  subout <- cbind(subout, rbind("-", "-"))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$tot_area), log10(group_data[[i]]$tot_volume))))
  
  subout <- cbind(subout, rbind("-", "-"))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$trunk_diam), log10(group_data[[i]]$tot_volume))))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$height), log10(group_data[[i]]$tot_volume))))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$max_path), log10(group_data[[i]]$tot_volume))))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$tot_length), log10(group_data[[i]]$tot_volume))))
  
  subout <- cbind(subout, rbind("-", "-"))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$trunk_diam), log10(group_data[[i]]$tot_area))))

  subout <- cbind(subout, output(fit(log10(group_data[[i]]$height), log10(group_data[[i]]$tot_area))))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$max_path), log10(group_data[[i]]$tot_area))))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$tot_length), log10(group_data[[i]]$tot_area))))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$height), log10(group_data[[i]]$tot_stem_m))))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$max_path), log10(group_data[[i]]$tot_stem_m))))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$tot_length), log10(group_data[[i]]$tot_stem_m))))
  
  subout <- cbind(subout, rbind("-", "-"))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$tot_stem_m), log10(group_data[[i]]$trunk_diam))))

  subout <- cbind(subout, rbind("-", "-"))
  
  subout <- cbind(subout, output(fit(log10(group_data[[i]]$tot_stem_m), log10(group_data[[i]]$tot_volume))))
  
  fits_test <- rbind(fits_test, subout)
}

# Group Branch Level Output
for (i in 4:12){
  subout <- matrix( nrow = 2, ncol = 1)
  
  subout[1,1] = paste( groups[i], "- X")
  subout[2,1] = "       - X + X^2"
  
  group <- subset(group_data[[i]], length_cm>0 & stem_m>0)
  
  fits_test <- rbind(fits_test, fits_to_row(subout, group))
}


# Individual Branch Level Output
for (i in 1:2){
  spp <- branch_size[branch_size$species==species[[i]][1],]
  for (j in 1:length(species[[i]][[2]])){
    subout <- matrix( nrow = 2, ncol = 1)
    
    subout[1,1] = paste( species[[i]][[3]][j], "- X")
    subout[2,1] = "       - X + X^2"
    
    ind <- spp[spp$tree==species[[i]][[2]][j] & spp$length_cm>0 & spp$stem_m>0,]
    
    fits_test <- rbind(fits_test, fits_to_row(subout, ind))
  }
}

# Individual Plus Output (WITH extra twig data) 
for (i in 1:7){
  subout <- matrix( nrow = 2, ncol = 1)
  
  spp <- branch_size[branch_size$species==species[[(plus[[1]][i])]][1],]
  ind <- spp[spp$tree==plus[[2]][i] & spp$length_cm>0 & spp$stem_m>0,]
  
  subout[1,1] = paste( plus[[3]][i], "- X")
  subout[2,1] = "       - X + X^2"
  
  fits_test <- rbind(fits_test, fits_to_row(subout, ind))
}

fits_test_out <- rbind(fits_test[1:11,], fits_test[64:69,], fits_test[86:87,], fits_test[70:73,], fits_test[12:15,],
                      fits_test[26:33,], fits_test[74:75,], fits_test[16:17,], fits_test[34:35,], fits_test[76:77,], 
                      fits_test[36:41,], fits_test[18:19,], fits_test[42:47,], fits_test[78:79,], fits_test[48:51,], 
                      fits_test[80:81,], fits_test[22:23,], fits_test[52:55,], fits_test[82:83,], fits_test[56:57,], 
                      fits_test[24:25,], fits_test[58:63,], fits_test[84:85,])

write.csv(fits_test_out, "FitResults.csv")

### REPORTS

# Tree Level Reports
fits_report <- matrix(nrow = 22, ncol = 3)

for (i in 1:3){
  
  fits_report[1,(4-i)] <- report(fit(log10(group_data[[i]]$height), log10(group_data[[i]]$trunk_diam)))
                            
  fits_report[2,(4-i)] <- report(fit(log10(group_data[[i]]$max_path), log10(group_data[[i]]$trunk_diam)))
   
  fits_report[3,(4-i)] <- report(fit(log10(group_data[[i]]$tot_length), log10(group_data[[i]]$trunk_diam)))
   
  fits_report[4,(4-i)] <- 4
   
  fits_report[5,(4-i)] <- report(fit(log10(group_data[[i]]$tot_area), log10(group_data[[i]]$tot_volume)))
   
  fits_report[6,(4-i)] <- 4
   
  fits_report[7,(4-i)] <- report(fit(log10(group_data[[i]]$trunk_diam), log10(group_data[[i]]$tot_volume)))
   
  fits_report[8,(4-i)] <- report(fit(log10(group_data[[i]]$height), log10(group_data[[i]]$tot_volume)))
   
  fits_report[9,(4-i)] <- report(fit(log10(group_data[[i]]$max_path), log10(group_data[[i]]$tot_volume)))
   
  fits_report[10,(4-i)] <- report(fit(log10(group_data[[i]]$tot_length), log10(group_data[[i]]$tot_volume)))
   
  fits_report[11,(4-i)] <- 4
   
  fits_report[12,(4-i)] <- report(fit(log10(group_data[[i]]$trunk_diam), log10(group_data[[i]]$tot_area)))
   
  fits_report[13,(4-i)] <- report(fit(log10(group_data[[i]]$height), log10(group_data[[i]]$tot_area)))
   
  fits_report[14,(4-i)] <- report(fit(log10(group_data[[i]]$max_path), log10(group_data[[i]]$tot_area)))
   
  fits_report[15,(4-i)] <- report(fit(log10(group_data[[i]]$tot_length), log10(group_data[[i]]$tot_area)))
  
  fits_report[16,(4-i)] <- report(fit(log10(group_data[[i]]$height), log10(group_data[[i]]$tot_stem_m)))
   
  fits_report[17,(4-i)] <- report(fit(log10(group_data[[i]]$max_path), log10(group_data[[i]]$tot_stem_m)))
   
  fits_report[18,(4-i)] <- report(fit(log10(group_data[[i]]$tot_length), log10(group_data[[i]]$tot_stem_m)))
   
  fits_report[19,(4-i)] <- 4
   
  fits_report[20,(4-i)] <- report(fit(log10(group_data[[i]]$tot_stem_m), log10(group_data[[i]]$trunk_diam)))
   
  fits_report[21,(4-i)] <- 4
   
  fits_report[22,(4-i)] <- report(fit(log10(group_data[[i]]$tot_stem_m), log10(group_data[[i]]$tot_volume)))
}

# Group Branch Level Reports
for (i in 4:12){
  group <- subset(group_data[[i]], length_cm>0 & stem_m>0)
  
  fits_report <- cbind(reports_to_col(group), fits_report)
}


# Individual Branch Level Reports
for (j in 1:2){
  spp <- branch_size[branch_size$species==species[[j]][1],]
  for (i in 1:length(species[[j]][[2]])){
    
    ind <- spp[spp$tree==species[[j]][[2]][i] & spp$length_cm>0 & spp$stem_m>0,]
    
    fits_report <- cbind(reports_to_col(ind), fits_report)
  }
}

###FIT REPORT TABLE

fits_report_out <- cbind(fits_report[,6:8], fits_report[,25], fits_report[,9:11], fits_report[,26:27],
                         fits_report[,13:16], fits_report[,28], fits_report[,17:20], fits_report[,29],
                         fits_report[,21:24], fits_report[,30:31], fits_report[,1:5], fits_report[,32:36])

write.csv(fits_report_out, 'Fits_for_fig.csv')

xlabels <- c("L~D (Segment)", "L~D (Path)", "L~D (Subtree)", "SA~V (Segment)", "SA~V (Subtree)", "D~V (Segment)", "D~V (Subtree)", 
             "L~V (Segment)", "L~V (Path)", "L~V (Subtree)", "D~SA (Segment)", "D~SA (Subtree)", "L~SA (Segment)", "L~SA (Path)", "L~SA (Subtree)", 
             "L~M (Segment)", "L~M (Path)", "L~M (Subtree)", "M~D (Segment)", "M~D (Subtree)", "M~V (Segment)", "M~V (Subtree)")

ylabels <- c("all-tree", "cherry", "apple", "all-branch", "cherry", "cherry-1", "cherry-2", "cherry-3", "cherry-4", "cherry-5", 
             "apple", "Bud.9", "Bud.9-1", "Bud.9-2", "Bud.9-3", "Bud.9-4", "CG.3041", "CG.3041-1", "CG.3041-2", "CG.3041-3", "CG.3041-4", 
             "CG.6210", "CG.6210-1", "CG.6210-2", "CG.6210-3", "CG.6210-4", "M.26", "JM.8", "JM.8-1", "JM.8-2", "JM.8-3", 
             "PiAu.5683", "PiAu.5683-1", "PiAu.5683-2", "PiAu.5683-3")

pdf(file="FitFigure.pdf", width=11, height=8,family="Helvetica", pointsize=12)
par(mar = c(2,8,8,2), cex.lab = 0.5, xpd=TRUE)
image(1:22, 1:35, fits_report_out, col = topo.colors(6), xlab="", ylab="", xaxt = 'n', yaxt = 'n', bty = 'n')
axis(2, at=35:1, ylabels, las = 2)
axis(3, at=1:22, xlabels, las = 2)
legend(-3,43, legend=c("Linear","Polynomial", "Marginal Poly", "No Data"), pch=c(15,15,15,15), bty = 'n',
       col = c("#E6FF00FF", "#4C00FFFF", "#00E5FFFF", "#FFE0B3FF"), pt.cex = 2.5)
dev.off()