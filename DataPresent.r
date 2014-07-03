# Presented are two major data tables in the orchard scaling project (2012-2013) conducted at Kaysville Research Farm, USU.
# Five 24 year old tart cherry (Prunus cerasus Montmorency, P. mahaleb) from one block and 
# 19 8 year old apples (Malus domestica "Golden Delicious") from one block with various rootstocks were sampled.

branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)

# BranchSegments.csv ("branch_size") contains the size and orientation data for all trees sampled at a "branch"-level. 
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


treesum <- read.csv("TreeSummary.csv", sep = ",", head=T)

# TreeSummary.csv ("tree_sum") contains the size and orientation data for all trees sampled at a "tree"-level. 
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


### Predicted Relationships


## Mass vs. Diameter [Predicted: M ~ D^8/3; Niklas & Spatz, 2004]

# Tree Level [Estimated: 2.49 +/- 0.18, R2 = 0.974]

Diameter_Mass <- sma(log10(treesum$tot_stem_m)~log10(treesum$trunk_diam))
plot(log10(treesum$trunk_diam[1:19]), log10(treesum$tot_stem_m[1:19]), xlim = c(1.5,2.5), ylim = c(3,5.5),
     xlab = "log ( Trunk Diameter  )", ylab = "log ( Total Stem Mass )", cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
points(log10(treesum$trunk_diam[20:24]), log10(treesum$tot_stem_m[20:24]), cex = 2.5, pch = 19, col = "red", bg = "red")
abline(coef.sma(Diameter_Mass)[1], coef.sma(Diameter_Mass)[2], lwd = 3, lty = 3)
abline(0, 2.667, lwd = 3, lty = 1)
legend('bottom', legend=expression(R^2 == 0.974), bty='n', cex=3)

# Branch Level [Estimated 2.47 +/- 0.05, R2 = 0.897]

clean_mass <- branch_size[branch_size$tot_stem_m > 0,] #rm.na mass
clean_diam <- clean_mass[clean_mass$diameter_mm > 0,]  #rm.na diamter

diameter_mass <- sma(log10(clean_diam$tot_stem_m)~log10(clean_diam$diameter_mm))
plot(log10(clean_diam$diameter_mm), log10(clean_diam$tot_stem_m), #xlim = c(0,7), ylim = c(0,12),
     xlab = "log ( Diameter  )", ylab = "log ( Total Stem Mass )", cex = 2, pch = 19, col = "black")
abline(coef.sma(diameter_mass)[1], coef.sma(diameter_mass)[2], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.897), bty='n', cex=3)

## Length vs. Diameter [Predicted: L ~ (D/2)^2/3; Price, Enquist & Savage, 2007]

# Tree Level [Estimated: 0.74 +/- 0.12, R2 = 0.849] 

Diameter_Height <- sma(log10(treesum$height)~log10((treesum$trunk_diam/2)))
plot(log10((treesum$trunk_diam[1:19]/2)), log10(treesum$height[1:19]), #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Trunk Diameter  )", ylab = "log ( Height )", cex = 2, pch = 19, col = "black")
points(log10((treesum$trunk_diam[20:24]/2)), log10(treesum$height[20:24]), cex = 1.5, pch = 23, col = "red", bg = "red")
abline(coef.sma(Diameter_Height)[1], coef.sma(Diameter_Height)[2], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.849), bty='n', cex=3)


## Length vs. Mass [Predicted: L~M^1/4; Niklas & Enquist, 2001]

# Tree Level [Estimated: 0.30 +/- 0.05, R2 = 0.849]

Height_Mass <- sma(log10(treesum$height)~log10(treesum$tot_stem_m))
plot(log10(treesum$tot_stem_m[1:19]), log10(treesum$height[1:19]), #xlim = c(0,6), ylim = c(0,3),
     xlab = "log ( Total Stem Mass  )", ylab = "log ( Height )", cex = 2, pch = 19, col = "black")
points(log10(treesum$tot_stem_m[20:24]), log10(treesum$height[20:24]), cex = 1.5, pch = 23, col = "red", bg = "red")
abline(coef.sma(Height_Mass)[1], coef.sma(Height_Mass)[2], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.849), bty='n', cex=3)


## Area Preservation [McCulloh & Sperry, 2005; Price, Enquist & Savage, 2007]



### Additional Relationships

#trunk diameter vs. canopy volume [Estimate: 1.04 +/- 0.2, R2 = 0.7162]

Diameter_Volume <- sma(log(treesum$canopy_volume)~log(treesum$trunk_diam))
plot(log(treesum$trunk_diam[1:19]), log(treesum$canopy_volume[1:19]), #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Trunk Diameter  )", ylab = "log ( Canopy Volume )", cex = 2, pch = 19, col = "black")
points(log(treesum$trunk_diam[20:24]), log(treesum$canopy_volume[20:24]), cex = 1.5, pch = 23, col = "red", bg = "red")
abline(coef.sma(Diameter_Volume)[1], coef.sma(Diameter_Volume)[2], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.716), bty='n', cex=3)

#trunk diameter vs. rank [Estimate: 2.04 +/- 0.5, R2 = 0.698)

Diameter_Rank <- sma(log(treesum$tot_no_branch + treesum$tot_no_twigs + treesum$tot_no_spurs)~log(treesum$trunk_diam))
plot(log(treesum$trunk_diam[1:19]), log(treesum$tot_no_branch[1:19] + treesum$tot_no_twigs[1:19] + treesum$tot_no_spurs[1:19]), 
	xlim = c(0,6), ylim = c(0,8), xlab = "log ( Trunk Diameter  )", ylab = "log ( Rank )", cex = 2, pch = 19, col = "black")
points(log(treesum$trunk_diam[20:24]), log(treesum$tot_no_branch[20:24] + treesum$tot_no_twigs[20:24] + treesum$tot_no_spurs[20:24]), 
	cex = 1.5, pch = 23, col = "red", bg = "red")
abline(coef.sma(Diameter_Rank)[1], coef.sma(Diameter_Rank)[2], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.698), bty='n', cex=3)

#rank vs. tot stem mass

Rank_Mass <- lm(log(treesum$tot_stem_m)~log(treesum$tot_no_branch + treesum$tot_no_twigs + treesum$tot_no_spurs))
plot(log(treesum$tot_no_branch[1:19] + treesum$tot_no_twigs[1:19] + treesum$tot_no_spurs[1:19]), 
	log(treesum$tot_stem_m[1:19]), xlim = c(0,7), ylim = c(0,12),
     	xlab = "log ( Height  )", ylab = "log ( Total Stem Mass )", cex = 2, pch = 19, col = "black")
points(log(treesum$tot_no_branch[20:24] + treesum$tot_no_twigs[20:24] + treesum$tot_no_spurs[20:24]), 
	log(treesum$tot_stem_m[20:24]), cex = 1.5, pch = 23, col = "red", bg = "red")
abline(summary(Rank_Mass)$coef[1,1], summary(Rank_Mass)$coef[2,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.776), bty='n', cex=3)


### Branch Level



### Yield


