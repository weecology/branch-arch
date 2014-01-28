# There are three major data tables in the orchard scaling project (2012-2013) conducted at Kaysville Research Farm, USU.
# Five 24 year old tart cherry (Prunus cerasus Montmorency, P. mahaleb) from one block and 
# 19 8 year old apples (Malus domestica "Golden Delicious") from one block with various rootstocks were sampled.

branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)

# BranchSegments.csv contains the size and orientation data ("branch_size") for all trees sampled at a "branch"-level. 
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
# # # diameter_mm - diameter at midpoint of branch in millimeters [float]
# # # root_dia - daimeter at rootstock juncture for trunk segments in millimeters [float]
# # # no_twigs - number of twig segments on branch (i.e., branching unit with multiple spurs attache) [integer]
# # # no_spurs - number of spur segments on branch (i.e., branching unit with entirely apical growth) [integer]
# # # no_scars - number of pruning scars on branch (i.e., locations of removed branching segments) [integer]
# # # stem_m - mass of stem in grams [float]
# # # tot_stem_m0 - total mass of succeding branches in grams from first programming trial [float]
# # # tot_stem_m - total mass of succeding branches in grams [float]
# # # twig_m - bulk mass of twigs in grams [float]
# # # leaf_m - bulk mass of leaves in grams [float]
# # # flower_m - bulk mass of flowers in grams [float]

branch_arch <- read.csv("TreeReconstruction.csv", sep = ',', header = T)

# TreeReconstruction.csv contains the data of 1 cherry and 6 apples for branch architecture ("branch_arch").
# This is the file that holds spacially explicit and twig level data. Fields are as follows:
# # # species - species common name [apple, cherry]
# # # tree - individual tree ID; for apple [1-15,17-20], for cherry [1,7,10,13,15]
# # # branch - branch-level ID per individual tree [integer]
# # # parent - branch ID of predecessing branch [integer]
# # # order - describes successing branch as "continuing" in relative diameter (order stays constant)
# # # # # or as "daughter" stepwise diminishing in relative diameter (parent order + 1) [integer]
# # # bearing - compass direction of branch at basipetal node [0 - 359]
# # # declination - angle from horizontal of branch [0 (horizontal) - 90 (vertical)]
# # # length_cm - length of branch in centimeters [float]
# # # diameter_mm - diameter at midpoint of branch in millimeters [float]
# # # angle - the direction that twig segments leave a branch segment in "O'clocks" [1-12]
# # # parent_dist - location of twig segment on parent branch segment 
# # # # # as distance from basipetal end of parent branch segment [float]
# # # spur - indicates if twig segment is classified as spur (i.e., branching unit with entirely apical growth) [y,n]
# # # rank_twig - total number of succeding branches (1 denotes a terminal twig segment) [integer]

treesum <- read.csv("TreeSummary.csv", sep = ",", head=T)

# # # X - updated ID of tree sorted by species, trunk diameter, and rootstock.
# # # species -
# # # tree -
# # # rootstock -
# # # trunk_diam -
# # # height - 
# # # canopy_volume -
# # # tot_stem_m -
# # # tot_twig_m -
# # # tot_leaf_m -
# # # tot_no_branch -
# # # tot_no_twigs - 
# # # tot_no_spurs -
# # # tot_no_scars -

### Tree Level

#trunk diameter vs total stem mass

Diameter_Mass <- lm(log(treesum$tot_stem_m)~log(treesum$trunk_diam))
plot(log(treesum$trunk_diam[1:19]), log(treesum$tot_stem_m[1:19]), xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Trunk Diameter  )", ylab = "log ( Total Stem Mass )", cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
points(log(treesum$trunk_diam[20:24]), log(treesum$tot_stem_m[20:24]), cex = 2.5, pch = 19, col = "red", bg = "red")
abline(summary(Diameter_Mass)$coef[1,1], summary(Diameter_Mass)$coef[2,1], lwd = 3, lty = 3)
abline(0, 2.667, lwd = 3, lty = 1)
legend('bottomright', legend=expression(R^2 == 0.975), bty='n', cex=3)

#trunk diameter vs. height

Diameter_Height <- lm(log(treesum$height)~log(treesum$trunk_diam))
plot(log(treesum$trunk_diam[1:19]), log(treesum$height[1:19]), #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Trunk Diameter  )", ylab = "log ( Height )", cex = 2, pch = 19, col = "black")
points(log(treesum$trunk_diam[20:24]), log(treesum$height[20:24]), cex = 1.5, pch = 23, col = "red", bg = "red")
abline(summary(Diameter_Height)$coef[1,1], summary(Diameter_Height)$coef[2,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.668 + 0.843), bty='n', cex=3)

#trunk diameter vs. canopy volume

Diameter_Volume <- lm(log(treesum$canopy_volume)~log(treesum$trunk_diam))
plot(log(treesum$trunk_diam[1:19]), log(treesum$canopy_volume[1:19]), #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Trunk Diameter  )", ylab = "log ( Canopy Volume )", cex = 2, pch = 19, col = "black")
points(log(treesum$trunk_diam[20:24]), log(treesum$canopy_volume[20:24]), cex = 1.5, pch = 23, col = "red", bg = "red")
abline(summary(Diameter_Volume)$coef[1,1], summary(Diameter_Volume)$coef[2,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.604), bty='n', cex=3)

#trunk diameter vs. rank

Diameter_Rank <- lm(log(treesum$tot_no_branch + treesum$tot_no_twigs + treesum$tot_no_spurs)~log(treesum$trunk_diam))
plot(log(treesum$trunk_diam[1:19]), log(treesum$tot_no_branch[1:19] + treesum$tot_no_twigs[1:19] + treesum$tot_no_spurs[1:19]), 
	xlim = c(0,6), ylim = c(0,8), xlab = "log ( Trunk Diameter  )", ylab = "log ( Rank )", cex = 2, pch = 19, col = "black")
points(log(treesum$trunk_diam[20:24]), log(treesum$tot_no_branch[20:24] + treesum$tot_no_twigs[20:24] + treesum$tot_no_spurs[20:24]), 
	cex = 1.5, pch = 23, col = "red", bg = "red")
abline(summary(Diameter_Rank)$coef[1,1], summary(Diameter_Rank)$coef[2,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.695), bty='n', cex=3)

#height vs. total stem mass

Height_Mass <- lm(log(treesum$tot_stem_m)~log(treesum$height))
plot(log(treesum$height[1:19]), log(treesum$tot_stem_m[1:19]), xlim = c(0,6), ylim = c(0,3),
     xlab = "log ( Height  )", ylab = "log ( Total Stem Mass )", cex = 2, pch = 19, col = "black")
points(log(treesum$height[20:24]), log(treesum$tot_stem_m[20:24]), cex = 1.5, pch = 23, col = "red", bg = "red")
abline(summary(Height_Mass)$coef[1,1], summary(Height_Mass)$coef[2,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.662), bty='n', cex=3)

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

#branch diameter vs. total succeeding mass

clean_mass <- branch_size[branch_size$tot_stem_m > 0,]
clean_diam <- clean_mass[clean_mass$diameter_mm > 0,]

diameter_mass <- lm(log(clean_diam$tot_stem_m)~log(clean_diam$diameter_mm))
plot(log(clean_diam$diameter_mm), log(clean_diam$tot_stem_m), xlim = c(0,7), ylim = c(0,12),
     	xlab = "log ( Diameter  )", ylab = "log ( Total Stem Mass )", cex = 2, pch = 19, col = "black")
abline(summary(diameter_mass)$coef[1,1], summary(diameter_mass)$coef[2,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.790), bty='n', cex=3)

#length ratio

#branch diameter vs. rank

### Yield

Diameter_Yield <- lm(log(treesum$yield[1:19])~log(treesum$trunk_diam[1:19]))
plot(log(treesum$trunk_diam[1:19]), log(treesum$yield[1:19]), #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Trunk Diameter  )", ylab = "log ( Yield )", cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
abline(summary(Diameter_Yield)$coef[1,1], summary(Diameter_Yield)$coef[2,1], lwd = 3, lty = 3)
legend('bottomleft', legend=expression(R^2 == 0.193), bty='n', cex=3)


Spurs_Yield <- lm(treesum$yield[1:19]~(treesum$tot_no_scars[1:19]/treesum$tot_no_spurs[1:19]))
plot(treesum$tot_no_scars[1:19]/treesum$tot_no_spurs[1:19], treesum$yield[1:19], #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Scars / Spurs )", ylab = "log ( Yield )")
#abline(summary(Spurs_Yield)$coef[1,1], summary(Spurs_Yield)$coef[3,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.594), bty='n', cex=3)

Volume_Yield <- lm(log(treesum$yield[1:19])~log(treesum$canopy_volume[1:19]))
plot(log(treesum$canopy_volume[1:19]), log(treesum$yield[1:19]), #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Canopy Volume  )", ylab = "log ( Yield )")
abline(summary(Volume_Yield)$coef[1,1], summary(Volume_Yield)$coef[2,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.278), bty='n', cex=3)

Pf_Yield <- lm(treesum$yield[1:19])~log(treesum$canopy_volume[1:19]))
plot(treesum$Pf[1:19], treesum$yield[1:19]), #xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Pf  )", ylab = "log ( Yield )")
abline(summary(Pf_Yield)$coef[1,1], summary(Pf_Yield)$coef[2,1], lwd = 3, lty = 3)
legend('topleft', legend=expression(R^2 == 0.278), bty='n', cex=3)

PredYield<-glm(treesum$yield[1:19]~log(treesum$trunk_diam[1:19])+treesum$Pf[1:19]+
		treesum$tot_no_scars[1:19]/treesum$tot_no_spurs[1:19], data=treesum, family = gaussian())
YieldFit<-lm(predict(fit, type="response")~1+treesum$yield[1:19])
plot(treesum$yield[1:19],predict(fit, type="response"),
	xlab = "Observed Yield", ylab = "Predicted Yield", cex.lab = 1.5, cex = 2.5, pch = 19, col = "black")
abline(0, 1, lwd = 3, lty = 3)
legend('bottomright', legend=expression(R^2 == 0.409), bty='n', cex=3)
