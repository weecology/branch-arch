# There are two major data tables in the orchard scaling project (2012-2013) conducted at Kaysville Research Farm, USU.
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

#trunk diameter vs total tree mass

#trunk diameter vs. height

#trunk diameter vs. canopy volume

#height vs. total tree mass

#length ratio

#branch diameter vs. rank

#branch diameter vs. total succeeding mass


