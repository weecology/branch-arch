# This script summarizes individual tree-level data.

data <- read.csv("BranchSegments.csv", sep = ",", head=T)
volume <- read.csv("VolumeEstimates.csv", sep=",", head=T)
Pf <- read.csv("PathFractionsBranch.csv", sep=",", head=T)
Mf <- read.csv("MassFractions.csv", sep=",", head=T)

species <- list(list("apple",
                     c(2,7,12,3,5,11,6,8,10,1,4,9,13,17,15,18,20,19,14),
                     c("Bud.9", "Bud.9", "Bud.9", "Bud.9", "CG.3041", "CG.3041",  
                       "CG.3041", "CG.3041", "CG.6210", "CG.6210", "CG.6210", 
                       "CG.6210", "M.26", "JM.8", "JM.8", "JM.8",
                       "PiAu.5683", "PiAu.5683", "PiAu.5683")),
                list("cherry", 
                     c(7,13,15,1,10),
                     "P. mahaleb"))

for (i in 1:2){
  spp <- data[data$species==species[[i]][1],]
  spp_pf <- Pf[Pf$species==species[[i]][1],]
  spp_mf <- Mf[Mf$species==species[[i]][1],]
  spp_volume <- volume[volume$species==species[[i]][1],]
  subout <- matrix(ncol = 22, nrow = length(species[[i]][[2]]))
  colnames(subout) <- c("species", "tree", "rootstock", "trunk_diam", "height", "max_path", "tot_length", "tot_area", 
                        "tot_volume", "Pf", "Mf", "canopy_volume", "tot_stem_m", "tot_twig_m", "tot_leaf_m", 
                        "tot_no_branch", "tot_no_twigs", "tot_no_spurs", "tot_no_scars", 
                        "avg_length_ratio", "avg_diameter_ratio", "avg_mass_ratio")
  
  for (j in 1:length(species[[i]][[2]])){
    ind <- spp[spp$tree==species[[i]][[2]][j],]
    ind_pf <- spp_pf[spp_pf$tree==species[[i]][[2]][j],]
    ind_mf <- spp_mf[spp_mf$tree==species[[i]][[2]][j],]
    ind_volume <- spp_volume[spp_volume$tree==species[[i]][[2]][j],]
    trunk <- ind[ind$branch==1,] 
    
    subout[j,1] = species[[i]][[1]]
    subout[j,2] = species[[i]][[2]][j]
    
    if (i==1)
      subout[j,3] = species[[i]][[3]][j]
    else
      subout[j,3] = species[[i]][[3]][1]
    
    subout[j,4]  = trunk$diameter_mm
    subout[j,5]  = round(ind_volume$height, digits = 1)
    subout[j,6]  = trunk$path_length
    subout[j,7]  = trunk$tot_length
    subout[j,8]  = trunk$tot_area
    subout[j,9]  = trunk$tot_volume
    subout[j,10]  = ind_pf$Pf
    subout[j,11]  = ind_mf$Mf
    subout[j,12]  = round(ind_volume$avg_r, digits = 1)
    subout[j,13]  = sum(ind$stem_m, na.rm = T)
    subout[j,14]  = sum(ind$twig_m, na.rm = T)
    subout[j,15]  = sum(ind$leaf_m, na.rm = T)
    subout[j,16] = length(ind$branch)
    subout[j,17] = sum(ind$no_twigs, na.rm = T)
    subout[j,18] = sum(ind$no_spurs, na.rm = T)
    subout[j,19] = sum(ind$no_scars, na.rm = T)
    subout[j,20] = round(mean(ind$length_ratio, na.rm=T),3)
    subout[j,21] = round(mean(ind$diameter_ratio, na.rm=T),3)
    subout[j,22] = round(mean(ind$mass_ratio, na.rm=T),3)
  }  
  if (i==1)
    trees_temp <- subout
  else
    trees_out = rbind(trees_temp, subout)
}
 
write.csv(trees_out,"TreeSummary.csv")