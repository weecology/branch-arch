###This script finds the parent segment of each branch and calculates length, diameter, and biomass ratios.

branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)

ratios <- matrix(nrow = 829, ncol = 5)
colnames(ratios) <- c("species", "tree", "length_ratio", "diameter_ratio", "mass_ratio")

for (i in 1:829){
  if (branch_size$branch[i] > 1){
    spp <- branch_size[branch_size$species == branch_size$species[i],]
    tree <- spp[spp$tree == branch_size$tree[i],]
    parent <- tree[tree$branch == branch_size$parent[i],]
    ratios[i,1] = branch_size$species[i]
    ratios[i,2] = branch_size$tree[i]
    ratios[i,3] = round(branch_size$length_cm[i] / parent$length_cm,3)
    ratios[i,4] = round(branch_size$diameter_mm[i] / parent$diameter_mm,3)
    ratios[i,5] = round(branch_size$stem_m[i] / parent$stem_m,3)
  }
}

write.csv(ratios, "ratios.csv")
