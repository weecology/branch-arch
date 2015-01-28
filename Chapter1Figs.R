###This script builds the figures used in the Chapter 1 publication.###

###Subtree improves R2
sma <- read.csv('SMAResults.csv', sep=',', head=T)

levels <- c(1,2,3,4,5,12)
results <- list()
for (i in 1:23){ #relationship columns
  results[[i]] <- list()
  for (j in 1:4){
    results[[i]][[j]] <- vector(length = 6)
    # 1: exponent, 2: CI-, 3: CI+, 4: R2 
    for (k in 1:6) { # group/ind rows
      results[[i]][[j]][k] = as.numeric(strsplit(as.character(sma[(levels[k]+1),(i+2)]), " ")[[1]][(2*j-1)])
    }
  }
}

exponent_R2 <- function(col_list, n, k, m, xlabel){
  
  if (length(col_list) == 2){
    min_range <- c(results[[col_list[1]]][[2]][k:m], results[[col_list[2]]][[2]][k:m], flow[n], elastic[n])
    max_range <- c(results[[col_list[1]]][[3]][k:m], results[[col_list[2]]][[3]][k:m], flow[n], elastic[n])}
  else{
    min_range <- c(results[[col_list[1]]][[2]][k:m], results[[col_list[2]]][[2]][k:m], 
                   results[[col_list[3]]][[2]][k:m], flow[n], elastic[n])
    max_range <- c(results[[col_list[1]]][[3]][k:m], results[[col_list[2]]][[3]][k:m], 
                   results[[col_list[3]]][[3]][k:m], flow[n], elastic[n])}
  
  plot(range(0,1), range(min(min_range, na.rm=T), max(max_range, na.rm=T)), 
       ylab = ylabels[n], xlab=xlabel, type = 'n',
       ylim = c(min(min_range, na.rm=T), max(max_range, na.rm=T)))
  
  if (length(col_list) == 3){
    for (i in 1:length(col_list)){
      for(j in k:m){
        points(results[[col_list[i]]][[4]][j], results[[col_list[i]]][[1]][j], pch = as.numeric(tri_points[[i]][j]), 
               bg = 'grey', cex = 2, lwd = 2.5)
        arrows(x0=results[[col_list[i]]][[4]][j], y0=results[[col_list[i]]][[2]][j], y1=results[[col_list[i]]][[3]][j], 
               code=3, angle=90, lwd=1.7, length=.08)
      }
    }
  }
  else{  
    for (i in 1:length(col_list)){
      for(j in k:m){
        points(results[[col_list[i]]][[4]][j], results[[col_list[i]]][[1]][j], pch = as.numeric(di_points[[i]][j]), 
               bg = 'grey', cex = 2, lwd = 2.5)
        arrows(x0=results[[col_list[i]]][[4]][j], y0=results[[col_list[i]]][[2]][j], y1=results[[col_list[i]]][[3]][j], 
               code=3, angle=90, lwd=1.7, length=.08)
      }
    }
  }  
  abline(h = flow[n], lwd = 2, lty = 6)
  abline(h = elastic[n], lwd = 2, lty = 2)
}

ylabels <- c("L~D", "SA~V", "D~V", "L~V", "D~SA", "L~SA", "L~M", "M~D", "M~V")
flow <- c(2, .75, .25, .5, .33, .67, NA, NA, NA)
elastic <- c(.67, .625, .375, .25, .6, .4, .25, 2.67, NA)
tri_points <- list(list(17, 24, 2, 17, 24, 2),
               list(18, 23, 5, 18, 23, 5),
               list(15, 22, 0, 15, 22, 0))
di_points <- list(list(17, 24, 2, 17, 24, 2),
                   list(15, 22, 0, 15, 22, 0))


pdf(file="ExponentR2.pdf", width= 16, height=12,family="Helvetica", pointsize=14)

par(mfrow= c(3,4), mar = c(4,5,1,1), cex.lab=1.5)
exponent_R2(c(1:3), 1, 1, 3, '')
exponent_R2(c(4:5), 2, 1, 3, '')
exponent_R2(c(6:7), 3, 1, 3, '')
plot(range(0,1), range(0,1), main = 'Tree Level', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
exponent_R2(c(8:10), 4, 1, 3, '')
exponent_R2(c(11:12), 5, 1, 3, '')
exponent_R2(c(13:15), 6, 1, 3, '')
plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
par(xpd=T)
legend('left', legend=c("", "", "", "", "", "", "All", "Cherry", "Apple"), 
       pch= c(17, 24, 2, 18, 23, 5, 15, 22, 0), pt.bg = 'grey', cex = 1.2, 
       bty = 'n', title = "Segment      Path      Subtree", title.adj = -.35, ncol = 3)
par(xpd=F)
exponent_R2(c(16:18), 7, 1, 3, '')
exponent_R2(c(19:20), 8, 1, 3, 'R2')
exponent_R2(c(21:22), 9, 1, 3, '')

par(mfrow= c(3,4), mar = c(4,5,1,1), cex.lab=1.5)
exponent_R2(c(1:3), 1, 4, 6, '')
exponent_R2(c(4:5), 2, 4, 6, '')
exponent_R2(c(6:7), 3, 4, 6, '')
plot(range(0,1), range(0,1), bty='n', main = 'Branch Level', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
exponent_R2(c(8:10), 4, 4, 6, '')
exponent_R2(c(11:12), 5, 4, 6, '')
exponent_R2(c(13:15), 6, 4, 6, '')
plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
par(xpd=T)
legend('left', legend=c("", "", "", "", "", "", "All", "Cherry", "Apple"), 
       pch= c(17, 24, 2, 18, 23, 5, 15, 22, 0), pt.bg = 'grey', cex = 1.2, 
       bty = 'n', title = "Segment     Path      Subtree", title.adj = -.35, ncol = 3)
par(xpd=F)
exponent_R2(c(16:18), 7, 4, 6, '')
exponent_R2(c(19:20), 8, 4, 6, 'R2')
exponent_R2(c(21:22), 9, 4, 6, '')

dev.off()

### Strong Relationships
branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)
library('smatr')
apple <- branch_size[branch_size$species == "apple",]
cherry <- branch_size[branch_size$species == "cherry",]


branch_graph <- function(x, y, labx, laby, cherry_point, apple_point){
  group_data_x <- list()
  group_data_x[[1]] <- x[x$species=="cherry",]
  group_data_x[[2]] <- x[x$species=="apple",]
  
  group_data_y <- list()
  group_data_y[[1]] <- y[y$species=="cherry",]  
  group_data_y[[2]] <- y[y$species=="apple",]
  
  test_all <- sma(log10(y[,3])~log10(x[,3]))
  test_cherry <- sma(log10(group_data_y[[1]][,3])~log10(group_data_x[[1]][,3]))
  test_apple <- sma(log10(group_data_y[[2]][,3])~log10(group_data_x[[2]][,3]))
  
  plot(log10(group_data_x[[1]][,3]), log10(group_data_y[[1]][,3]), 
       xlim = c((log10(min(x[,3], na.rm=T))-0.2),(log10(max(x[,3], na.rm=T))+0.2)), 
       ylim = c((log10(min(y[,3], na.rm=T))-0.2),(log10(max(y[,3], na.rm=T))+0.2)),
       xlab = labx, ylab = laby, cex.lab = 1.5, cex = 1, pch = cherry_point, lwd = 1.5, bg = "grey")
  points(log10(group_data_x[[2]][,3]), log10(group_data_y[[2]][,3]), cex = 1, pch = apple_point, lwd = 1.5)
  segments(log10(min(x[,3], na.rm=T))-0.1, 
           (sma(test_all)$coef[[1]][2,1]*log10(min(x[,3], na.rm=T)))+sma(test_all)$coef[[1]][1,1]-0.1, 
           log10(max(x[,3], na.rm=T))+0.1,
           (sma(test_all)$coef[[1]][2,1]*log10(max(x[,3], na.rm=T)))+sma(test_all)$coef[[1]][1,1]+0.1,
           lwd = 4, lty = 2)
  segments(log10(min(group_data_x[[1]][,3], na.rm=T))-0.1, 
           (sma(test_cherry)$coef[[1]][2,1]*log10(min(group_data_x[[1]][,3], na.rm=T)))+sma(test_cherry)$coef[[1]][1,1]-0.1, 
           log10(max(group_data_x[[1]][,3], na.rm=T))+0.1,
           (sma(test_cherry)$coef[[1]][2,1]*log10(max(group_data_x[[1]][,3], na.rm=T)))+sma(test_cherry)$coef[[1]][1,1]+0.1,
           lwd = 4, lty = 3)
  segments(log10(min(group_data_x[[2]][,3], na.rm=T))-0.1, 
           (sma(test_apple)$coef[[1]][2,1]*log10(min(group_data_x[[2]][,3], na.rm=T)))+sma(test_apple)$coef[[1]][1,1]-0.1, 
           log10(max(group_data_x[[2]][,3], na.rm=T))+0.1,
           (sma(test_apple)$coef[[1]][2,1]*log10(max(group_data_x[[2]][,3], na.rm=T)))+sma(test_apple)$coef[[1]][1,1]+0.1,
           lwd = 4, lty = 3)
  #abline(sma(test_all)$coef[[1]][1,1], sma(test_all)$coef[[1]][2,1], lwd = 3, lty = 2)
  #abline(sma(test_cherry)$coef[[1]][1,1], sma(test_cherry)$coef[[1]][2,1], lwd = 3, lty = 3)
  #abline(sma(test_apple)$coef[[1]][1,1], sma(test_apple)$coef[[1]][2,1], lwd = 3, lty = 3)
}

##Scaling Relationships

pdf(file="ScalingData.pdf", width=12, height=24, family="Helvetica", pointsize=12)
par(mfrow = c(9,3))

###Length  ~ Diameter
length_zeros = branch_size[branch_size$length_cm > 0,]
branch_graph(subset(length_zeros, select = c(species, tree, diameter_mm)),
             subset(length_zeros, select = c(species, tree, length_cm)),
             "log ( Diameter  )", "log ( Segment Length )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, diameter_mm)),
             subset(branch_size, select = c(species, tree, path_length)),
             "log ( Diameter  )", "log ( Path Length )", 23, 5)

branch_graph(subset(branch_size, select = c(species, tree, diameter_mm)),
             subset(branch_size, select = c(species, tree, tot_length)),
             "log ( Diameter  )","log ( Subtree Length )", 22, 0)

###Surface Area ~ Volume 
branch_graph(subset(branch_size, select = c(species, tree, volume)),
             subset(branch_size, select = c(species, tree, area)),
             "log ( Segment Volume  )","log ( Segment Surface Area )", 24, 2)

plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')

branch_graph(subset(branch_size, select = c(species, tree, tot_volume)),
             subset(branch_size, select = c(species, tree, tot_area)),
             "log ( Subtree Volume  )","log ( Subtree Surface Area )", 22, 0)

### Diameter ~ Volume
branch_graph(subset(branch_size, select = c(species, tree, volume)),
             subset(branch_size, select = c(species, tree, diameter_mm)),
             "log ( Segment Volume  )","log ( Diameter )", 24, 2)

plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')

branch_graph(subset(branch_size, select = c(species, tree, tot_volume)),
             subset(branch_size, select = c(species, tree, diameter_mm)),
             "log ( Subtree Volume  )","log ( Diameter )", 22, 0)

### Length ~ Volume
length_zeros = branch_size[branch_size$length_cm > 0,]
branch_graph(subset(length_zeros, select = c(species, tree, volume)),
             subset(length_zeros, select = c(species, tree, length_cm)),
             "log ( Segment Volume  )", "log ( Segment Length )", 24, 2)

branch_graph(subset(length_zeros, select = c(species, tree, tot_volume)),
             subset(length_zeros, select = c(species, tree, path_length)),
             "log ( Subtree Volume  )", "log ( Path Length )", 23, 5)

branch_graph(subset(branch_size, select = c(species, tree, tot_volume)),
             subset(branch_size, select = c(species, tree, tot_length)),
             "log ( Subtree Volume  )","log ( Subtree Length )", 22, 0)

### Diameter ~ Surface Area
branch_graph(subset(branch_size, select = c(species, tree, area)),
             subset(branch_size, select = c(species, tree, diameter_mm)),
             "log ( Segment Surface Area  )","log ( Diameter )", 24, 2)

plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')

branch_graph(subset(branch_size, select = c(species, tree, tot_area)),
             subset(branch_size, select = c(species, tree, diameter_mm)),
             "log ( Subtree Surface Area  )","log ( Diameter )", 22, 0)

### Length ~ Surface Area
branch_graph(subset(length_zeros, select = c(species, tree, area)),
             subset(length_zeros, select = c(species, tree, length_cm)),
             "log ( Segment Surface Area  )", "log ( Segment Length )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, tot_area)),
             subset(branch_size, select = c(species, tree, path_length)),
             "log ( Subtree Surface Area  )","log ( Path Length )", 23, 5)

branch_graph(subset(branch_size, select = c(species, tree, tot_area)),
             subset(branch_size, select = c(species, tree, tot_length)),
             "log ( Subtree Surface Area  )","log ( Subtree Length )", 22, 0)

### Length ~ Mass
mass_zero <- length_zeros[length_zeros$stem_m > 0,]
branch_graph(subset(mass_zero, select = c(species, tree, stem_m)),
             subset(mass_zero, select = c(species, tree, length_cm)),
             "log ( Segment Stem Mass  )", "log ( Segment Length )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, tot_stem_m)),
             subset(branch_size, select = c(species, tree, path_length)),
             "log ( Subtree Stem Mass  )", "log ( Path Length )", 23, 5)

branch_graph(subset(branch_size, select = c(species, tree, tot_stem_m)),
             subset(branch_size, select = c(species, tree, path_length)),
             "log ( Subtree Stem Mass  )", "log ( Subtree Length )", 22, 0)

### Mass ~ Diameter
mass_zero <- branch_size[branch_size$stem_m > 0,]
branch_graph(subset(mass_zero, select = c(species, tree, diameter_mm)),
             subset(mass_zero, select = c(species, tree, stem_m)),
             "log ( Diameter  )", "log ( Segment Stem Mass )", 24, 2)

plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')

branch_graph(subset(branch_size, select = c(species, tree, diameter_mm)),
             subset(branch_size, select = c(species, tree, tot_stem_m)),
             "log ( Diameter  )", "log ( Total Stem Mass )", 22, 0)

### Mass ~ Volume (Wood Density) 
mass_zero <- branch_size[branch_size$stem_m > 0,]
branch_graph(subset(mass_zero, select = c(species, tree, volume)),
             subset(mass_zero, select = c(species, tree, stem_m)),
             "log ( Segment Volume  )","log ( Segment Stem Mass )", 24, 2)

plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')

totmass_totvolume <- sma(log10(branch_size$tot_stem_m)~log10(branch_size$tot_volume))
branch_graph(subset(branch_size, select = c(species, tree, tot_volume)),
             subset(branch_size, select = c(species, tree, tot_stem_m)),
             "log ( Subtree Volume  )","log ( Subtree Mass )", 22, 0)
dev.off()

## Linear vs. Poly

