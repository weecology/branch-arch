# Presented is TreeReconstruction.csv from the orchard scaling project (2012-2013) conducted at Kaysville Research Farm, USU.
# One 24 year old tart cherry (Prunus cerasus Montmorency, P. mahaleb) and 
# 6 8 year old apples (Malus domestica "Golden Delicious") from one block each with a different rootstocks 
# were sampled at a twig level for branch architecture.

branch_arch <- read.csv("TreeReconstruction.csv", sep = ',', header = T)

# TreeReconstruction.csv contains the data of 1 cherry and 6 apples for branch architecture ("branch_arch").
# This file contains spacially explicit and twig level data. Fields are as follows:
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