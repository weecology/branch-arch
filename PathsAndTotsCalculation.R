branch <- read.csv("BranchSegments.csv", sep=',', head=T)
twig <- read.csv("TreeReconstruction.csv", sep=',', head=T)

species <- list(list("apple",
                     c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20)),
                list("cherry", 
                     c(1,7,10,13,15)))


### Calculates total stem mass (subtree) above branch node

for (m in 1:2){
  spp <- branch[branch$species==species[[m]][1],]
  for (n in 1:length(species[[m]][[2]])){
    tree <- spp[spp$tree==species[[m]][[2]][n],]
    for (j in length(tree[,1]):1){
         daughters <- tree[tree$parent==tree$branch[j],]
         if (length(daughters[,1]) > 0)
           tree$tot_stem_m[j] = tree$stem_m[j] + sum(daughters$tot_stem_m, na.rm = TRUE)
         else
           tree$tot_stem_m[j] = tree$stem_m[j]     
    }
    
    tree_masses = data.frame(tree = tree$tree, branch = tree$branch, mass = tree$tot_stem_m)
    
    if (exists('masses_out'))
      masses_out = rbind(masses_out, tree_masses)
    else
      masses_out <- tree_masses
  }
}

write.csv(masses_out,"Sum_masses.csv")


### Calculates total twig mass (subtree) above branch node

for (m in 1:2){
  spp <- branch[branch$species==species[[m]][1],]
  for (n in 1:length(species[[m]][[2]])){
    tree <- spp[spp$tree==species[[m]][[2]][n],]
    for (j in length(tree[,1]):1){
      daughters <- tree[tree$parent==tree$branch[j],]
      if (length(daughters[,1]) > 0)
        tree$tot_twig_m[j] = tree$twig_m[j] + sum(daughters$tot_twig_m, na.rm = TRUE)
      else
        tree$tot_twig_m[j] = tree$twig_m[j]
    }
    
    twig_masses = data.frame(tree = tree$tree, branch = tree$branch, tot_twig_m = tree$tot_twig_m)
    
    if (exists('twig_m_out'))
      twig_m_out = rbind(twig_m_out, twig_masses)
    else
      twig_m_out <- twig_masses
  }
}

write.csv(twig_m_out,"Twig_masses.csv")


### Calculates path length above branch node

for (m in 1:2){
  spp <- branch[branch$species==species[[m]][1],]
  for (n in 1:length(species[[m]][[2]])){
    tree <- spp[spp$tree==species[[m]][[2]][n],]
    for (j in length(tree[,1]):1){
      daughters <- tree[tree$parent==tree$branch[j],]
      if (length(daughters[,1]) > 0){
        tree$path_length[j] = tree$length_cm[j] + max(daughters$path_length, na.rm = TRUE)
        }
      else{
        twig_spp <- twig[twig$species==species[[m]][1],]
        twig_tree <- twig_spp[twig_spp$tree==species[[m]][[2]][n],]
        twig_daughters <- twig_tree[twig_tree$parent==tree$branch[j],]
        end_twig <- twig_daughters[twig_daughters$parent_dist==tree$length_cm[j],]
        if (length(end_twig[,1])>0)
          tree$path_length[j] = tree$length_cm[j] + max(end_twig$length_cm)
        else
          tree$path_length[j] = tree$length_cm[j]
        }     
    }
    
    tree_path = data.frame(tree = tree$tree, branch = tree$branch, path = tree$path_length)
    
    if (exists('paths_out'))
      paths_out = rbind(paths_out, tree_path)
    else
      paths_out <- tree_path
  }
}

write.csv(paths_out,"Paths.csv")


### Calculates total stem length (subtree) above branch node

for (m in 1:2){
  spp <- branch[branch$species==species[[m]][1],]
  for (n in 1:length(species[[m]][[2]])){
    tree <- spp[spp$tree==species[[m]][[2]][n],]
    for (j in length(tree[,1]):1){
      daughters <- tree[tree$parent==tree$branch[j],]
      if (length(daughters[,1]) > 0){
        tree$tot_length[j] = tree$length_cm[j] + sum(daughters$tot_length, na.rm = TRUE)
        }
      else{
        twig_spp <- twig[twig$species==species[[m]][1],]
        twig_tree <- twig_spp[twig_spp$tree==species[[m]][[2]][n],]
        twig_daughters <- twig_tree[twig_tree$parent==tree$branch[j],]
        if (length(twig_daughters[,1])>0)
          tree$tot_length[j] = tree$length_cm[j] + sum(twig_daughters$length_cm)
        else
          tree$tot_length[j] = tree$length_cm[j]
        }
    }
    
    tree_lengths = data.frame(tree = tree$tree, branch = tree$branch, tot_length = tree$tot_length)
    
    if (exists('lengths_out'))
      lengths_out = rbind(lengths_out, tree_lengths)
    else
      lengths_out <- tree_lengths
  }
}

write.csv(lengths_out,"Sum_lengths.csv")
