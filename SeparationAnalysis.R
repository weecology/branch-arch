branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)

branch_graph <- function(x, y, test, pred_b, pred_m, labx, laby, position, R2){
  group_data_x <- list()
  group_data_x[[1]] <- x[x$species=="cherry",]
  
  apple <- x[x$species=="apple",]
  group_data_x[[2]] <- subset(apple, tree==20 | tree==19 | tree==14)
  group_data_x[[3]] <- subset(apple, tree==17 | tree==15 | tree==18)
  group_data_x[[4]] <- subset(apple, tree==13)
  group_data_x[[5]] <- subset(apple, tree==10 | tree==1 | tree==4 | tree ==9)
  group_data_x[[6]] <- subset(apple, tree==5 | tree==11 | tree==6 | tree ==8)
  group_data_x[[7]] <- subset(apple, tree==2 | tree==7 | tree==12 | tree ==3)
  
  group_data_y <- list()
  group_data_y[[1]] <- y[y$species=="cherry",]
  
  apple <- y[y$species=="apple",]
  group_data_y[[2]] <- subset(apple, tree==20 | tree==19 | tree==14)
  group_data_y[[3]] <- subset(apple, tree==17 | tree==15 | tree==18)
  group_data_y[[4]] <- subset(apple, tree==13)
  group_data_y[[5]] <- subset(apple, tree==10 | tree==1 | tree==4 | tree ==9)
  group_data_y[[6]] <- subset(apple, tree==5 | tree==11 | tree==6 | tree ==8)
  group_data_y[[7]] <- subset(apple, tree==2 | tree==7 | tree==12 | tree ==3)
  
  plot(log10(group_data_x[[1]][,3]), log10(group_data_y[[1]][,3]), 
       xlim = c((log10(min(x[,3], na.rm=T))-0.2),(log10(max(x[,3], na.rm=T))+0.2)), 
       ylim = c((log10(min(y[,3], na.rm=T))-0.2),(log10(max(y[,3], na.rm=T))+0.2)),
       xlab = labx, ylab = laby, cex.lab = 1.5, cex = 1.5, pch = 21, col = "red", bg = "red")
  points(log10(group_data_x[[2]][,3]), log10(group_data_y[[2]][,3]), cex = 1.5, pch = 21, col = "black", bg = "orange")
  points(log10(group_data_x[[3]][,3]), log10(group_data_y[[3]][,3]), cex = 1.5, pch = 21, col = "black", bg = 'yellow')
  points(log10(group_data_x[[4]][,3]), log10(group_data_y[[4]][,3]), cex = 1.5, pch = 21, col = "black", bg = 'green')
  points(log10(group_data_x[[5]][,3]), log10(group_data_y[[5]][,3]), cex = 1.5, pch = 21, col = "black", bg = 'cyan')
  points(log10(group_data_x[[6]][,3]), log10(group_data_y[[6]][,3]), cex = 1.5, pch = 21, col = "black", bg = 'blue')
  points(log10(group_data_x[[7]][,3]), log10(group_data_y[[7]][,3]), cex = 1.5, pch = 21, col = "black", bg = 'purple')
  abline(sma(test)$coef[[1]][1,1], sma(test)$coef[[1]][2,1], lwd = 3, lty = 2)
  abline(sma(test)$coef[[1]][1,2], sma(test)$coef[[1]][2,2], lwd = 3, lty = 3)
  abline(sma(test)$coef[[1]][1,3], sma(test)$coef[[1]][2,3], lwd = 3, lty = 3)
  abline(pred_b, pred_m, lwd = 3, lty = 1)
  legend(position, legend=R2, bty='n', cex=3)  
} #branch-level by spp & rootstock log10-trans

branch_graph_twigs <- function(x, y, test, pred_b, pred_m, labx, laby, position, R2){
  group_data_x <- list()
  cherry <- x[x$species=="cherry",]
  group_data_x[[1]] <- subset(cherry, tree==15)
  group_data_x[[2]] <- subset(cherry, tree!=15)
  
  apple <- x[x$species=="apple",]
  group_data_x[[3]] <- subset(apple, tree==13 | tree==14 | tree==15 | tree==3 | tree==4 | tree==5)
  group_data_x[[4]] <- subset(apple, tree==1 | tree==2 | tree>=6 & tree<=12 | tree>=16)
  
  group_data_y <- list()
  cherry <- y[y$species=="cherry",]
  group_data_y[[1]] <- subset(cherry, tree==15)
  group_data_y[[2]] <- subset(cherry, tree!=15)
  
  apple <- y[y$species=="apple",]
  group_data_y[[3]] <- subset(apple, tree==13 | tree==14 | tree==15 | tree==3 | tree==4 | tree==5)
  group_data_y[[4]] <- subset(apple, tree==1 | tree==2 | tree>=6 & tree<=12 | tree>=16)

  
  plot(log10(group_data_x[[1]][,3]), log10(group_data_y[[1]][,3]), 
       xlim = c((log10(min(x[,3], na.rm=T))-0.2),(log10(max(x[,3], na.rm=T))+0.2)), 
       ylim = c((log10(min(y[,3], na.rm=T))-0.2),(log10(max(y[,3], na.rm=T))+0.2)),
       xlab = labx, ylab = laby, cex.lab = 1.5, cex = 1.5, pch = 21, col = "red", bg = "red")
  points(log10(group_data_x[[2]][,3]), log10(group_data_y[[2]][,3]), cex = 1.5, pch = 21, col = "black", bg = "blue")
  points(log10(group_data_x[[3]][,3]), log10(group_data_y[[3]][,3]), cex = 1.5, pch = 21, col = "black", bg = 'orange')
  points(log10(group_data_x[[4]][,3]), log10(group_data_y[[4]][,3]), cex = 1.5, pch = 21, col = "black", bg = 'green')
  #abline(sma(test)$coef[[1]][1,1], sma(test)$coef[[1]][2,1], lwd = 3, lty = 2)
  #abline(sma(test)$coef[[1]][1,2], sma(test)$coef[[1]][2,2], lwd = 3, lty = 3)
  #abline(sma(test)$coef[[1]][1,3], sma(test)$coef[[1]][2,3], lwd = 3, lty = 3)
  #abline(pred_b, pred_m, lwd = 3, lty = 1)
  #legend(position, legend=R2, bty='n', cex=3)  
} #branch-level by ex/implicit log10-trans

branch_graph_twigs(subset(branch_size, select = c(species, tree, tot_area)),
             subset(branch_size, select = c(species, tree, tot_length)),
             totarea_totlength, 0, 0,"log ( Subtree Surface Area  )","log ( Subtree Length )" ,
             'bottomright', expression(R^2 == 0.391))