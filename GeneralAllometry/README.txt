This file describes the data and analysis files that contribute to the results
reported in "Process-based allometry describes the influence of management on
orchard tree aboveground architecture" coauthored by Zachary Brym and
Morgan Ernest. 

NB: Additional files remain in the repository that are not reported in the 
paper, but have contributed to the data management and analysis process. Many of
those files are referenced here and include commenting in the file to describe
its purpose and procedure.

## Data

Data was gathered between 2012-2013 at Utah State University Kaysville Research 
Farm, in Kaysville, Utah. We sampled five 24 year old tart cherry (Prunus 
cerasus Montmorency, P. mahaleb) from one block and 19 8 year old apples (Malus
domestica "Golden Delicious") from one block with various rootstocks were
sampled.

`BranchSegments.csv` contains raw and calculated data for all trees
sampled at a branch-level.

species - species common name [apple, cherry]
tree - individual tree ID; for apple [1-15,17-20], for cherry [1,7,10,13,15]
branch - branch-level ID per individual tree [integer]
date - date of collection in Julian days [integer]
parent - branch ID of immediately basal branch [integer]
no_daughters - number of remaining branches distal to branch [integer]
order - describes continuity of branch nodes, i.e. order stays constant if next branch "continues" in relative diameter
length_cm - length of branch in centimeters [float]
path_length - total length of branch and longest continuous length of distil branches [float]
tot_length - sum of all lengths of distil branches, i.e. "subtree" [float]
diameter_mm - diameter at midpoint of branch in millimeters [float]
area - branch segment surface area calculated by `PathsAndTotsCalculation.R`
path_area - surface area of longest branch path by `PathsAndTotsCalculation.R`
tot_area - surface area of subtree by `PathsAndTotsCalculation.R`
volume - branch segment volume calculated by `PathsAndTotsCalculation.R`
path_volume - volume of longest branch path by `PathsAndTotsCalculation.R`
tot_volume - volume of subtree by `PathsAndTotsCalculation.R`
path_length_plus - path_length for subset of trees with terminal twig lengths by `PathsAndTotsCalculation.R`
tot_length_plus - tot_length for subset of trees with terminal twig lengths by `PathsAndTotsCalculation.R`
area_plus - path_area for subset of trees with terminal twig lengths by `PathsAndTotsCalculation.R`
tot_area_plus - tot_area for subset of trees with terminal twig lengths by `PathsAndTotsCalculation.R`
volume_plus - path_volume for subset of trees with terminal twig lengths by `PathsAndTotsCalculation.R`
tot_volume_plus - tot_volume for subset of trees with terminal twig lengths by `PathsAndTotsCalculation.R`
no_twigs - number of twig segments on branch (i.e., branching unit with multiple spurs attached) [integer]
tot_no_twigs - number of twigs for subtree by `PathsAndTotsCalculation.R`
no_spurs - number of spur segments on branch (i.e., branching unit with entirely apical growth) [integer]
no_scars - number of pruning scars on branch (i.e., locations of removed branching segments) [integer]
stem_m - mass of stem in grams [float]
tot_stem_m - subtree stem mass calculated by `PathsAndTotsCalculation.R`
twig_m - bulk mass of twigs in grams [float]
tot_twig_m - subtree twig mass calculated by `PathsAndTotsCalculation.R`
leaf_m - bulk mass of leaves in grams [float]
flower_m - bulk mass of flowers in grams [float]
length_ratio - ratio of daughter / parent branch lengths by `RatioCalculation.R`
path_ratio - ratio of daughter / parent branch path lengths by `RatioCalculcation.R`
diameter_ratio - ratio of daughter / parent branch diameter by `RatioCalculation.R`
mass_ratio - ratio of daughter / parent branch stem mass by `RatioCalculation.R`
summass_ratio - ratio of daughter / parent branch total mass of subtree by `RatioCalculation.R`
area_ratio - ratio of daughter / parent branch surface area by `RatioCalculation.R`
bearing - compass direction of branch at basipetal node [0 - 359]
opp_length_cm - length of horizontal traverse of branch as if branch is hypotenuse to calculate declination [float]
declination - angle from horizontal of branch [0 (horizontal) - 90 (vertical)]
root_dia - daimeter at rootstock juncture for trunk segments in millimeters [float]

`TreeSummary.csv` contains calculated data for all trees sampled at a individual 
tree-level as calculated by `TreeLevelSummary.R`.

species - species common name [apple, cherry]
tree - individual tree ID; for apple [1-15,17-20], for cherry [1,7,10,13,15]
rootstock - species, trade name, or code for individual tree rootstock
trunk_diam - diameter of the trunk in mm taken 15 cm above graft union [float]
height - overall height of the tree estimated at trunk in m [float]
max_path - path length of the trunk segment [float]
tot_length - subtree branch length of the trunk segment [float]
tot_area - subtree branch surface area of the trunk segment [float]
tot_volume - subtree branch volume of the trunk segment [float] 
Pf - path fraction of tree; data transferred from `PathFractionsBranch.csv` as calculated by `PathFraction.r`
Mf - mass fraction of tree; data transferred from `PathFractionsBranch.csv` as calculated by `PathFraction.r`
canopy_volume - canopy volume estimated as a sphere of average canopy radius; data transferred from `VolumeEstimates.csv` as calculated by `VolumeEstimates.py`
tot_stem_m - total mass of stems in grams [float]
tot_twig_m - total mass of twigs in grams [float]
tot_leaf_m - total mass of leaves in grams [float]
tot_no_branch - total number of branch segments [integer]
tot_no_twigs - total number of twigs [integer]
tot_no_spurs - total number of spurs [integer]
tot_no_scars - total number of scars [integer]
avg_length_ratio - average ratio of daughter / parent branch lengths [float]
avg_diameter_ratio - average ratio of daughter / parent branch diameters [float]
avg_mass_ratio - average ratio of daughter / parent branch mass [float]

## Analysis

`SMA-test.R
