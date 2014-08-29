###This script finds the parent segment of each branch and calculates length, diameter, and biomass ratios.

branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)

ratios <- matrix(nrow = length(branch_size[,1]), ncol = 9)
colnames(ratios) <- c("species", "tree", "length_ratio", "path_ratio", "diameter_ratio", "mass_ratio", 
                      "summass_ratio", "no_daughters", "area_ratio")

for (i in 1:length(branch_size[,1])){
  
  ratios[i,1] = branch_size$species[i]
  ratios[i,2] = branch_size$tree[i]
  spp <- branch_size[branch_size$species == branch_size$species[i],]
  tree <- spp[spp$tree == branch_size$tree[i],]
  
  if (branch_size$branch[i] > 1){
    parent <- tree[tree$branch == branch_size$parent[i],]
    ratios[i,3] = round(branch_size$length_cm[i] / parent$length_cm,3)
    ratios[i,4] = round(branch_size$path_length[i] / parent$path_length,3)
    ratios[i,5] = round(branch_size$diameter_mm[i] / parent$diameter_mm,3)
    ratios[i,6] = round(branch_size$stem_m[i] / parent$stem_m,3)
    ratios[i,7] = round(branch_size$tot_stem_m[i] / parent$tot_stem_m,3)
  }

  daughters <- tree[tree$parent == branch_size$branch[i],]
  ratios[i,8] = length(daughters[,1])
  if (length(daughters[,1]) > 0)
    daughter_A <- vector(length = length(daughters[,1]))
    for (j in 1:length(daughters[,1])){
      daughter_A[j] = pi * (daughters$diameter_mm[j]/2)^2
    }
    ratios[i,9] = sum(daughter_A)/(pi * (branch_size$diameter_mm[i]/2)^2)
}

#write.csv(ratios, "ratios.csv")
