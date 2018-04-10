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


### Calculates total number of terminal twigs above branch node

for (m in 1:2){
  spp <- branch[branch$species==species[[m]][1],]
  for (n in 1:length(species[[m]][[2]])){
    tree <- spp[spp$tree==species[[m]][[2]][n],]
    for (j in length(tree[,1]):1){
      daughters <- tree[tree$parent==tree$branch[j],]
      if (length(daughters[,1]) > 0)
        tree$tot_no_twigs[j] = tree$no_twigs[j] + sum(daughters$tot_no_twigs, na.rm = TRUE)
      else
        tree$tot_no_twigs[j] = tree$no_twigs[j]
    }
    
    no_twig = data.frame(tree = tree$tree, branch = tree$branch, tot_no_twigs = tree$tot_no_twigs)
    
    if (exists('no_twig_out'))
      no_twig_out = rbind(no_twig_out, no_twig)
    else
      no_twig_out <- no_twig
  }
}


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

### Calculates path length, area, and volume above branch node.

for (m in 1:2){
  spp <- branch[branch$species==species[[m]][1],]
  for (n in 1:length(species[[m]][[2]])){
    tree <- spp[spp$tree==species[[m]][[2]][n],]
    for (j in length(tree[,1]):1){
      daughters <- tree[tree$parent==tree$branch[j],]
      if (length(daughters[,1]) > 0){
        max_daughter <- daughters[daughters$path_length==max(daughters$path_length, na.rm = TRUE),]
        tree$path_length[j] = tree$length_cm[j] + max_daughter$path_length[1]
        tree$path_mass[j] = tree$stem_m[j] + max_daughter$path_mass[1]
        tree$path_area[j] = round(2 * pi * (tree$diameter_mm[j]/2) * tree$length_cm[j],1) + max_daughter$path_area[1]
        tree$path_volume[j] = round(pi * (tree$diameter_mm[j]/2)^2 * tree$length_cm[j],1) + max_daughter$path_volume[1]
      }
      else{
        tree$path_length[j] = tree$length_cm[j]
        tree$path_mass[j] = tree$stem_m[j]
        tree$path_area[j] = round(2 * pi * (tree$diameter_mm[j]/2) * tree$length_cm[j],1)
        tree$path_volume[j] = round(pi * (tree$diameter_mm[j]/2)^2 * tree$length_cm[j],1)
      }
    }
    
    tree_path = data.frame(tree = tree$tree, branch = tree$branch, path_length = tree$path_length, 
                           path_mass=tree$path_mass, path_area = tree$path_area, 
                           path_volume = tree$path_volume)
    
    if (exists('paths_out'))
      paths_out = rbind(paths_out, tree_path)
    else
      paths_out <- tree_path
  }
}

### Calculates total stem (subtree) length, surface area, and volume above branch node.

for (m in 1:2){
  spp <- branch[branch$species==species[[m]][1],]
  for (n in 1:length(species[[m]][[2]])){
    tree <- spp[spp$tree==species[[m]][[2]][n],]
    for (j in length(tree[,1]):1){
      daughters <- tree[tree$parent==tree$branch[j],]
      tree$area[j] = round(2 * pi * (tree$diameter_mm[j]/2) * tree$length_cm[j],1)
      tree$volume[j] = round(pi * (tree$diameter_mm[j]/2)^2 * tree$length_cm[j],1)
      if (length(daughters[,1]) > 0){
        tree$tot_length[j] = tree$length_cm[j] + sum(daughters$tot_length, na.rm = TRUE)
        tree$tot_area[j] = tree$area[j] + sum(daughters$tot_area, na.rm = TRUE)
        tree$tot_volume[j] = tree$volume[j] + sum(daughters$tot_volume, na.rm = TRUE)
        }
      else{
        tree$tot_length[j] = tree$length_cm[j]
      }
    }
    
    tree_lengths = data.frame(tree = tree$tree, branch = tree$branch, tot_length = tree$tot_length,
                              area = tree$area, tot_area = tree$tot_area, volume = tree$volume,
                              tot_volume = tree$tot_volume)
    
    if (exists('lengths_out'))
      lengths_out = rbind(lengths_out, tree_lengths)
    else
      lengths_out <- tree_lengths
  }
}


### Calculates path length above branch node (includes extra twig data when available).

for (m in 1:2){
  spp <- branch[branch$species==species[[m]][1],]
  for (n in 1:length(species[[m]][[2]])){
    tree <- spp[spp$tree==species[[m]][[2]][n],]
    for (j in length(tree[,1]):1){
      daughters <- tree[tree$parent==tree$branch[j],]
      if (length(daughters[,1]) > 0){
        tree$path_length_plus[j] = tree$length_cm[j] + max(daughters$path_length, na.rm = TRUE)
        }
      else{
        twig_spp <- twig[twig$species==species[[m]][1],]
        twig_tree <- twig_spp[twig_spp$tree==species[[m]][[2]][n],]
        twig_daughters <- twig_tree[twig_tree$parent==tree$branch[j],]
        end_twig <- twig_daughters[twig_daughters$parent_dist==tree$length_cm[j],]
        if (length(end_twig[,1])>0)
          tree$path_length_plus[j] = tree$length_cm[j] + max(end_twig$length_cm)
        else
          tree$path_length_plus[j] = tree$length_cm[j]
        }     
    }
    
    tree_path_plus = data.frame(tree = tree$tree, branch = tree$branch, path_plus = tree$path_length_plus)
    
    if (exists('paths_out_plus'))
      paths_out_plus = rbind(paths_out_plus, tree_path_plus)
    else
      paths_out_plus <- tree_path_plus
  }
}



### Calculates total stem (subtree) length, surface area, and volume above branch node (includes extra twig data when available).

for (m in 1:2){
  spp <- branch[branch$species==species[[m]][1],]
  for (n in 1:length(species[[m]][[2]])){
    tree <- spp[spp$tree==species[[m]][[2]][n],]
    for (j in length(tree[,1]):1){
      daughters <- tree[tree$parent==tree$branch[j],]
      tree$area_plus[j] = round(2 * pi * (tree$diameter_mm[j]/2) * tree$length_cm[j],1)
      tree$volume_plus[j] = round(pi * (tree$diameter_mm[j]/2)^2 * tree$length_cm[j],1)
      if (length(daughters[,1]) > 0){
        tree$tot_length_plus[j] = tree$length_cm[j] + sum(daughters$tot_length, na.rm = TRUE)
        tree$tot_area_plus[j] = tree$area[j] + sum(daughters$tot_area, na.rm = TRUE)
        tree$tot_volume_plus[j] = tree$volume[j] + sum(daughters$tot_volume, na.rm = TRUE)
      }
      else{
        twig_spp <- twig[twig$species==species[[m]][1],]
        twig_tree <- twig_spp[twig_spp$tree==species[[m]][[2]][n],]
        twig_daughters <- twig_tree[twig_tree$parent==tree$branch[j],]

        if (length(twig_daughters[,1])>0){
          tree$tot_length_plus[j] = tree$length_cm[j] + sum(twig_daughters$length_cm)
          tree$tot_area_plus[j] = tree$area[j] + round(2 * pi * sum(twig_daughters$length_cm),1) #assuming twig diameter = 2mm
          tree$tot_volume_plus[j] = tree$volume[j] + round(pi * sum(twig_daughters$length_cm),1)
        }
        else{
          tree$tot_length_plus[j] = tree$length_cm[j]
          tree$tot_area_plus[j] = tree$area[j]
          tree$tot_volume_plus[j] = tree$volume[j]
        }
      }
    }
    
    tree_lengths_plus = data.frame(tree = tree$tree, branch = tree$branch, 
                              tot_length_plus = tree$tot_length_plus, area_plus = tree$area_plus, 
                              tot_area_plus = tree$tot_area_plus, volume_plus = tree$volume_plus,
                              tot_volume_plus = tree$tot_volume_plus)
    
    if (exists('lengths_out_plus'))
      lengths_out_plus = rbind(lengths_out_plus, tree_lengths_plus)
    else
      lengths_out_plus <- tree_lengths_plus
  }
}

tots_out = data.frame(tree = branch$tree, branch = branch$branch, path_length = paths_out$path_length, tot_length = lengths_out$tot_length, 
                      area = lengths_out$area, path_area = paths_out$path_area, tot_area = lengths_out$tot_area, 
                      volume = lengths_out$volume, path_volume = paths_out$path_volume,
                      tot_volume = lengths_out$tot_volume, tot_stem_m = masses_out$mass, 
                      tot_no_twigs = no_twig_out$tot_no_twigs, tot_twig_m = twig_m_out$tot_twig_m,
                      path_plus = paths_out_plus$path_plus, tot_length_plus = lengths_out_plus$tot_length_plus, 
                      area_plus = lengths_out_plus$area_plus, tot_area_plus = lengths_out_plus$tot_area_plus, 
                      volume_plus = lengths_out_plus$volume_plus, tot_volume_plus = lengths_out_plus$tot_volume_plus) 

write.csv(tots_out,"PathAndTotals.csv")
