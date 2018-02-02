### This script generates multiple figures that report the raw data for each of 
### the relationships investigated for each individual tree. Data is reported at
### branch segment and subtree levels. The figures appear as Fig S3 in the 
### manuscript.
 

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
       xlab = labx, ylab = laby, cex.lab = 1.35, cex.axis = 1.15, pch = cherry_point, lwd = 1.5, bg = "grey")
  points(log10(group_data_x[[2]][,3]), log10(group_data_y[[2]][,3]), cex = 1, pch = apple_point, lwd = 1.5)
  segments(log10(min(x[,3], na.rm=T))-0.1, 
           (sma(test_all)$coef[[1]][2,1]*log10(min(x[,3], na.rm=T)))+sma(test_all)$coef[[1]][1,1]-0.1, 
           log10(max(x[,3], na.rm=T))+0.1,
           (sma(test_all)$coef[[1]][2,1]*log10(max(x[,3], na.rm=T)))+sma(test_all)$coef[[1]][1,1]+0.1,
           lwd = 4, lty = 2, col='grey')
  segments(log10(min(group_data_x[[1]][,3], na.rm=T))-0.1, 
           (sma(test_cherry)$coef[[1]][2,1]*log10(min(group_data_x[[1]][,3], na.rm=T)))+sma(test_cherry)$coef[[1]][1,1]-0.1, 
           log10(max(group_data_x[[1]][,3], na.rm=T))+0.1,
           (sma(test_cherry)$coef[[1]][2,1]*log10(max(group_data_x[[1]][,3], na.rm=T)))+sma(test_cherry)$coef[[1]][1,1]+0.1,
           lwd = 4, lty = 3, col='grey')
  segments(log10(min(group_data_x[[2]][,3], na.rm=T))-0.1, 
           (sma(test_apple)$coef[[1]][2,1]*log10(min(group_data_x[[2]][,3], na.rm=T)))+sma(test_apple)$coef[[1]][1,1]-0.1, 
           log10(max(group_data_x[[2]][,3], na.rm=T))+0.1,
           (sma(test_apple)$coef[[1]][2,1]*log10(max(group_data_x[[2]][,3], na.rm=T)))+sma(test_apple)$coef[[1]][1,1]+0.1,
           lwd = 4, lty = 3, col='grey')
  #abline(sma(test_all)$coef[[1]][1,1], sma(test_all)$coef[[1]][2,1], lwd = 3, lty = 2)
  #abline(sma(test_cherry)$coef[[1]][1,1], sma(test_cherry)$coef[[1]][2,1], lwd = 3, lty = 3)
  #abline(sma(test_apple)$coef[[1]][1,1], sma(test_apple)$coef[[1]][2,1], lwd = 3, lty = 3)
}


pdf(file="FigS3.pdf", width=12, height=18, family="Helvetica", pointsize=18)
par(mfrow = c(5,4))

###Length  ~ Diameter
length_zeros = branch_size[branch_size$length_cm > 0,]
branch_graph(subset(length_zeros, select = c(species, tree, diameter_mm)),
             subset(length_zeros, select = c(species, tree, length_cm)),
             "log ( Segment Diameter  )", "log ( Segment Length )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, diameter_mm)),
             subset(branch_size, select = c(species, tree, tot_length)),
             "log ( Proximal Diameter  )","log ( Subtree Length )", 22, 0)

###Surface Area ~ Volume 
branch_graph(subset(branch_size, select = c(species, tree, volume)),
             subset(branch_size, select = c(species, tree, area)),
             "log ( Segment Volume  )","log ( Segment Area )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, tot_volume)),
             subset(branch_size, select = c(species, tree, tot_area)),
             "log ( Subtree Volume  )","log ( Subtree Area )", 22, 0)

### Diameter ~ Volume
branch_graph(subset(branch_size, select = c(species, tree, volume)),
             subset(branch_size, select = c(species, tree, diameter_mm)),
             "log ( Segment Volume  )","log ( Segment Diameter )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, tot_volume)),
             subset(branch_size, select = c(species, tree, diameter_mm)),
             "log ( Subtree Volume  )","log ( Proximal Diameter )", 22, 0)

### Length ~ Volume
length_zeros = branch_size[branch_size$length_cm > 0,]
branch_graph(subset(length_zeros, select = c(species, tree, volume)),
             subset(length_zeros, select = c(species, tree, length_cm)),
             "log ( Segment Volume  )", "log ( Segment Length )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, tot_volume)),
             subset(branch_size, select = c(species, tree, tot_length)),
             "log ( Subtree Volume  )","log ( Subtree Length )", 22, 0)

### Diameter ~ Surface Area
branch_graph(subset(branch_size, select = c(species, tree, area)),
             subset(branch_size, select = c(species, tree, diameter_mm)),
             "log ( Segment Area  )","log ( Segment Diameter )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, tot_area)),
             subset(branch_size, select = c(species, tree, diameter_mm)),
             "log ( Subtree Area  )","log ( Proximal Diameter )", 22, 0)

### Length ~ Surface Area
branch_graph(subset(length_zeros, select = c(species, tree, area)),
             subset(length_zeros, select = c(species, tree, length_cm)),
             "log ( Segment Area  )", "log ( Segment Length )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, tot_area)),
             subset(branch_size, select = c(species, tree, tot_length)),
             "log ( Subtree Area  )","log ( Subtree Length )", 22, 0)

### Length ~ Mass
mass_zero <- length_zeros[length_zeros$stem_m > 0,]
branch_graph(subset(mass_zero, select = c(species, tree, stem_m)),
             subset(mass_zero, select = c(species, tree, length_cm)),
             "log ( Segment Mass  )", "log ( Segment Length )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, tot_stem_m)),
             subset(branch_size, select = c(species, tree, path_length)),
             "log ( Subtree Mass  )", "log ( Subtree Length )", 22, 0)

### Mass ~ Diameter
mass_zero <- branch_size[branch_size$stem_m > 0,]
branch_graph(subset(mass_zero, select = c(species, tree, diameter_mm)),
             subset(mass_zero, select = c(species, tree, stem_m)),
             "log ( Segment Diameter  )", "log ( Segment Mass )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, diameter_mm)),
             subset(branch_size, select = c(species, tree, tot_stem_m)),
             "log ( Proximal Diameter  )", "log ( Subtree Mass )", 22, 0)

### Mass ~ Volume (Wood Density) 
mass_zero <- branch_size[branch_size$stem_m > 0,]
branch_graph(subset(mass_zero, select = c(species, tree, volume)),
             subset(mass_zero, select = c(species, tree, stem_m)),
             "log ( Segment Volume  )","log ( Segment Mass )", 24, 2)

branch_graph(subset(branch_size, select = c(species, tree, tot_volume)),
             subset(branch_size, select = c(species, tree, tot_stem_m)),
             "log ( Subtree Volume  )","log ( Subtree Mass )", 22, 0)
dev.off()

