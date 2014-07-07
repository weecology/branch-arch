# Presented are two major data tables in the orchard scaling project (2012-2013) conducted at Kaysville Research Farm, USU.
# Five 24 year old tart cherry (Prunus cerasus Montmorency, P. mahaleb) from one block and 
# 19 8 year old apples (Malus domestica "Golden Delicious") from one block with various rootstocks were sampled.


## BranchSegments.csv ("branch_size") contains the size and orientation data for all trees sampled at a "branch"-level.

branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)
 
# Terminal compenents of the tree (i.e., "twigs") were colected as twig counts and bulk mass. Fields are:
# # # species - species common name [apple, cherry]
# # # tree - individual tree ID; for apple [1-15,17-20], for cherry [1,7,10,13,15]
# # # branch - branch-level ID per individual tree [integer]
# # # date - date of collection in Julian days [integer]
# # # parent - branch ID of predecessing branch [integer]
# # # order - describes successing branch as "continuing" in relative diameter (order stays constant) 
# # # # # or as "daughter" stepwise diminishing in relative diameter (parent order + 1) [integer]
# # # bearing - compass direction of branch at basipetal node [0 - 359]
# # # opp_length_cm - length of horizontal traverse of branch as if branch is hypotenuse to calculate declination [float]
# # # declination - angle from horizontal of branch [0 (horizontal) - 90 (vertical)]
# # # length_cm - length of branch in centimeters [float]
# # # path_length - total length of branch and longest continuous length of succeding branches [float]
# # # diameter_mm - diameter at midpoint of branch in millimeters [float]
# # # root_dia - daimeter at rootstock juncture for trunk segments in millimeters [float]
# # # no_twigs - number of twig segments on branch (i.e., branching unit with multiple spurs attached) [integer]
# # # no_spurs - number of spur segments on branch (i.e., branching unit with entirely apical growth) [integer]
# # # no_scars - number of pruning scars on branch (i.e., locations of removed branching segments) [integer]
# # # stem_m - mass of stem in grams [float]
# # # tot_stem_m - total mass of succeding branches in grams [float]
# # # twig_m - bulk mass of twigs in grams [float]
# # # leaf_m - bulk mass of leaves in grams [float]
# # # flower_m - bulk mass of flowers in grams [float]
# # # length_ratio - ratio of daughter / parent branch lengths [float]
# # # path_ratio	- ratio of daughter / parent branch path lengths [float]
# # # diameter_ratio - ratio of daughter / parent branch diameter [float]
# # # mass_ratio - ratio of daughter / parent branch stem mass [float]
# # # summass_ratio - ratio of daughter / parent branch total mass of succeding branches [float]


## TreeSummary.csv ("tree_sum") contains the size and orientation data for all trees sampled at a "tree"-level.

treesum <- read.csv("TreeSummary.csv", sep = ",", head=T)

# Fields are:
# # # X - Numerical ID of tree ***sorted by species, rootstock, and trunk diameter NOT YET***.
# # # species - species common name [apple, cherry]
# # # tree - individual tree ID; for apple [1-15,17-20], for cherry [1,7,10,13,15]
# # # rootstock - individual rootstock name [Bud.9, CG.3041, CG.6210, M.26, JM.8, PiAu.5683]
# # # trunk_diam - diameter at 15cm above graft union of trunck in millimeters [float]
# # # height - height of individual [float]
# # # canopy_volume - volume of the canopy as calculated by average radius in VolumeEstimates.py [float]
# # # tot_stem_m - total mass of stems [float]
# # # tot_twig_m - total mass of twigs [float]
# # # tot_leaf_m - total mass of leaves [float]
# # # tot_no_branch - total number of branch segments [float]
# # # tot_no_twigs - total number of twig segments [float]
# # # tot_no_spurs - total number of spurs [float]
# # # tot_no_scars - total number of scars [float]
# # # avg_length_ratio - average branch-level ratio of daughter / parent branch lengths [float]
# # # avg_diameter_ratio - average branch-level ratio of daughter / parent branch diameter [float]
# # # avg_mass_ratio - average branch-level ratio of daughter / parent branch mass [float]

#Data is analysed using Standardized Major Axis (SMA; aka reduced major axis) with R-package 'smatr'

library('smatr')

#graph generation set-up

tree_graph <- function(x, y, test, pred_b, pred_m, labx, laby, position, R2){
  plot(log10(x[1:19]), log10(y[1:19]), 
       xlim = c((log10(min(x))-0.2),(log10(max(x))+0.2)), ylim = c((log10(min(y))-0.2),(log10(max(y))+0.2)),
       xlab = labx, ylab = laby, cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
  points(log10(x[20:24]), log10(y[20:24]), cex = 2.5, pch = 19, col = "red", bg = "red")
  abline(coef.sma(test)[1], coef.sma(test)[2], lwd = 3, lty = 2)
  abline(sma(test)$coef[[1]][1,2], sma(test)$coef[[1]][2,2], lwd = 3, lty = 3)
  abline(sma(test)$coef[[1]][1,3], sma(test)$coef[[1]][2,3], lwd = 3, lty = 3)
  abline(pred_b, pred_m, lwd = 3, lty = 1)
  legend(position, legend=R2, bty='n', cex=3)  
} #tree-level figures
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
  abline(coef.sma(test)[1], coef.sma(test)[2], lwd = 3, lty = 2)
  abline(sma(test)$coef[[1]][1,2], sma(test)$coef[[1]][2,2], lwd = 3, lty = 3)
  abline(sma(test)$coef[[1]][1,3], sma(test)$coef[[1]][2,3], lwd = 3, lty = 3)
  abline(pred_b, pred_m, lwd = 3, lty = 1)
  legend(position, legend=R2, bty='n', cex=3)  
} #branch-level log10-trans
branch_graph2 <- function(x, y, test, pred_b, pred_m, labx, laby, position, R2){
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
  
  plot(group_data_x[[1]][,3], group_data_y[[1]][,3], 
       xlim = c((min(x[,3], na.rm=T)-0.2),(max(x[,3], na.rm=T)+0.2)), 
       ylim = c((min(y[,3], na.rm=T)-0.2),(max(y[,3], na.rm=T)+0.2)),
       xlab = labx, ylab = laby, cex.lab = 1.5, cex = 1.5, pch = 21, col = "red", bg = "red")
  points(group_data_x[[2]][,3], group_data_y[[2]][,3], cex = 1.5, pch = 21, col = "black", bg = "orange")
  points(group_data_x[[3]][,3], group_data_y[[3]][,3], cex = 1.5, pch = 21, col = "black", bg = 'yellow')
  points(group_data_x[[4]][,3], group_data_y[[4]][,3], cex = 1.5, pch = 21, col = "black", bg = 'green')
  points(group_data_x[[5]][,3], group_data_y[[5]][,3], cex = 1.5, pch = 21, col = "black", bg = 'cyan')
  points(group_data_x[[6]][,3], group_data_y[[6]][,3], cex = 1.5, pch = 21, col = "black", bg = 'blue')
  points(group_data_x[[7]][,3], group_data_y[[7]][,3], cex = 1.5, pch = 21, col = "black", bg = 'purple')
  abline(coef.sma(test)[1], coef.sma(test)[2], lwd = 3, lty = 2)
  abline(sma(test)$coef[[1]][1,2], sma(test)$coef[[1]][2,2], lwd = 3, lty = 3)
  abline(sma(test)$coef[[1]][1,3], sma(test)$coef[[1]][2,3], lwd = 3, lty = 3)
  abline(pred_b, pred_m, lwd = 3, lty = 1)
  legend(position, legend=R2, bty='n', cex=3)  
} #branch-level no log-trans



### Predicted Relationships


## Mass vs. Diameter [Predicted: M ~ D^8/3; Niklas & Spatz, 2004]

# Tree Level [Estimated: 2.49 +/- 0.18, R2 = 0.974]
# I would consider this spot on!!! It's just a little shy of the 8/3 buckling, which is within the statistical limits.

Diameter_Mass <- sma(log10(treesum$tot_stem_m)~log10(treesum$trunk_diam), slope.test = 2.67)
tree_graph(treesum$trunk_diam, treesum$tot_stem_m, Diameter_Mass, 0, 2.67,
           "log ( Trunk Diameter  )", "log ( Total Stem Mass )", 'bottomright', expression(R^2 == 0.974))

# Branch Level [Estimated 2.48 +/- 0.05, R2 = 0.905]
# Again, this is just shallaw of the prediction, and begins to show a signal of pruning (i.e., mass removal)!

diameter_mass <- sma(log10(branch_size$tot_stem_m)~log10(branch_size$diameter_mm))
branch_graph(subset(branch_size, select = c(species, tree, diameter_mm)),
             subset(branch_size, select = c(species, tree, tot_stem_m)),
             diameter_mass, 0, 2.67, "log ( Diameter  )", "log ( Total Stem Mass )",
             'bottomright', expression(R^2 == 0.905))


## Length vs. Diameter [Predicted: L ~ (D/2)^2/3; Price, Enquist & Savage, 2007]

# Tree Level [Estimated: 0.74 +/- 0.12, R2 = 0.849] 
# The statistical result again includes the 2/3 expectation, but suggests closer to a 3/4. I don't have an explanation just yet.

Diameter_Height <- sma(log10(treesum$height)~log10((treesum$trunk_diam/2)))
tree_graph((treesum$trunk_diam/2), treesum$height, Diameter_Height, coef.sma(Diameter_Height)[1], 0.67,
           "log ( Trunk Diameter  )", "log ( Height )", 'bottomright', expression(R^2 == 0.849))


# Branch Level [Estimated: 0.86 +/- 0.04, R2 = 0.448]
# This is even more beyond the 2/3 expectaiton, with more length per diameter. Might be an artifact of vigorous shoots.

diameter_path <- sma(log10(branch_size$path_length)~log10(branch_size$diameter_mm))
branch_graph(subset(branch_size, select = c(species, tree, diameter_mm)),
             subset(branch_size, select = c(species, tree, path_length)),
             diameter_path, coef.sma(diameter_path)[1], 0.67, "log ( Diameter  )", "log ( Path Length )",
             'bottomright', expression(R^2 == 0.442))


## Length vs. Mass [Predicted: L~M^1/4; Niklas & Enquist, 2001]

# Tree Level [Estimated: 0.30 +/- 0.05, R2 = 0.849]
# Again, the statistics include both reasonable options of 1/4 and 1/3. Hmm.

Height_Mass <- sma(log10(treesum$height)~log10(treesum$tot_stem_m))
tree_graph(treesum$tot_stem_m, treesum$height, Height_Mass, coef.sma(Height_Mass)[1], 0.25,
           "log ( Total Stem Mass  )", "log ( Height )", 'bottomright', expression(R^2 == 0.849))


# Branch Level [Estimated: 0.35 +/- 0.1, R2 = 0.632]
# And, again the branch level pushes beyong the upper limit of the expectation. Apple and cherry are not low density.


length_mass <- sma(log10(branch_size$path_length)~log10(branch_size$tot_stem_m))
branch_graph(subset(branch_size, select = c(species, tree, tot_stem_m)),
             subset(branch_size, select = c(species, tree, path_length)),
             length_mass, coef.sma(length_mass)[1], 0.25, "log ( Total Stem Mass  )", "log ( Path Length )",
             'bottomright', expression(R^2 == 0.632))


## daVinci's Rule of Area Preservation [Predicted: D(k+1)/D(k) = 0.5, McCulloh & Sperry, 2005; Price, Enquist & Savage, 2007]

# Area Decreasing Result [Estimate: 0.68 +/- 0.03, R2 = 0.569, extreme end of reasonalbe values]
# Major pruning signal! Stems are fatter at the bottom than expected due to removal of end shoots.

dratio_temp_na <- subset(branch_size, select = c(species, tree, diameter_mm, diameter_ratio))
dratio_temp <- na.omit(dratio_temp_na)
parent_diam <- data.frame(species = dratio_temp$species, tree = dratio_temp$tree,
                          ratio = dratio_temp$diameter_mm/dratio_temp$diameter_ratio)

diameter_ratio <- sma(dratio_temp$diameter_mm~parent_diam$ratio)
branch_graph2(parent_diam, dratio_temp, diameter_ratio, 0, 0.5, "Parent Diameter", "Daughter Diameter",
             'topleft', expression(R^2 == 0.632))


## Length Ratio [Predicted: L(k+1)/L(k) = 0.5; Price, Enquist & Savage, 2007]

# Segment Length [Estimate: 1.41 +/- 0.1, R2 = 0.038, data incompatible]
# My data is not detailed enought to get at this accurately. I ingnored small twig branching events.

sratio_temp_na <- subset(branch_size, select = c(species, tree, length_cm, length_ratio))
sratio_temp <- na.omit(sratio_temp_na)
parent_length <- data.frame(species = sratio_temp$species, tree = sratio_temp$tree,
                            ratio = sratio_temp$length_cm/sratio_temp$length_ratio)


length_ratio <- sma(sratio_temp$length_cm~parent_length$ratio)
branch_graph2(parent_length, sratio_temp, length_ratio, 0, 0.5, "Parent Stem Length", 
             "Daughter Stem Length",'bottomright', expression(R^2 == 0.038))


# Path Length [Estimate: 0.80 +/- 0.03, R2 = 0.545, still extreme end of reasonalbe values]
# This is closer, but I'm still not sure it hits the mark.

lratio_temp_na <- subset(branch_size, select = c(species, tree, path_length, path_ratio))
lratio_temp <- na.omit(lratio_temp_na)
parent_path <- data.frame(species = lratio_temp$species, tree = lratio_temp$tree,
                          ratio = lratio_temp$path_length/lratio_temp$path_ratio)



path_ratio <- sma(lratio_temp$path_length~parent_path$ratio)
branch_graph2(parent_path, lratio_temp, path_ratio, 0, 0.5, "Parent Path Length", 
             "Daughter Path Length",'topleft', expression(R^2 == 0.545))


### Additional Relationships

# trunk diameter vs. canopy volume [Estimate: 1.04 +/- 0.2, R2 = 0.7162] 
# This might be indicative of the D ~ M(leaves).

Diameter_Volume <- sma(log10(treesum$canopy_volume)~log10(treesum$trunk_diam))
tree_graph(treesum$trunk_diam, treesum$canopy_volume, Diameter_Volume, coef.sma(Diameter_Volume)[1], 1,
           "log ( Trunk Diameter  )", "log ( Canopy Volume )", 'bottomright', expression(R^2 == 0.716))


# trunk diameter vs. rank [Estimate: 2.04 +/- 0.5, R2 = 0.698] 
# This might be indicative of the furcation ratio = 2.

Diameter_Rank <- sma(log10(treesum$tot_no_branch + treesum$tot_no_twigs + treesum$tot_no_spurs)~log10(treesum$trunk_diam))
tree_graph(treesum$trunk_diam, (treesum$tot_no_branch + treesum$tot_no_twigs + treesum$tot_no_spurs), 
           Diameter_Rank, 0, 0,
           "log ( Trunk Diameter  )", "log ( Rank )", 'bottomright', expression(R^2 == 0.698))

# rank vs. tot stem mass [Estimate: 1.22 +/- 0.25, R2 = 0.782] 
# Rank can be a good approximation for mass.

Rank_Mass <- sma(log10(treesum$tot_stem_m)~log10(treesum$tot_no_branch + treesum$tot_no_twigs + treesum$tot_no_spurs))
tree_graph((treesum$tot_no_branch + treesum$tot_no_twigs + treesum$tot_no_spurs), treesum$tot_stem_m, 
           Rank_Mass, 0, 0,
           "log ( Rank )", "log ( Total Stem Mass )", 'bottomright', expression(R^2 == 0.782))