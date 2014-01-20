# This script summarizes individual tree-level data.
data <- read.csv("BranchSegments.csv", sep = ",", head=T)

species <- list(list("apple",
                     c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20),
                     c("CG.6210", "Bud.9", "Bud.9", "CG.6210", "CG.3041",
                       "CG.3041", "Bud.9", "CG.3041", "CG.6210", "CG.6210",
                       "CG.3041", "Bud.9", "M.26", "PiAu.5683", "JM.8",
                       "JM.8", "JM.8", "PiAu.5683", "PiAu.5683")),
                list("cherry", 
                     c(1,7,10,13,15),
                     "P. mahaleb"))

for (i in 1:2){
  spp <- data[data$species==species[[i]][1],]
  subout <- matrix(ncol = 12, nrow = length(species[[i]][[2]]))
  colnames(subout) <- c("species", "tree", "rootstock", "trunk_diam", "height", "canopy_volume",
                       "tot_stem_m", "tot_twig_m", "tot_leaf_m", 
                       "tot_no_branch", "tot_no_twigs", "tot_no_scars")
  
  for (j in 1:length(species[[i]][[2]])){
    ind <- spp[spp$tree==species[[i]][[2]][j],]
    trunk <- ind[ind$branch==1,] 
    
    subout[j,1] = species[[i]][[1]]
    subout[j,2] = species[[i]][[2]][j]
    
    if (i==1)
      subout[j,3] = species[[i]][[3]][j]
    else
      subout[j,3] = species[[i]][[3]][1]
    
    subout[j,4] = trunk$diameter_mm
    
    
    subout[j,7] = sum(ind$stem_m, na.rm = T)
    
  if (i==1)
    trees_temp <- subout
  else
    trees_out = rbind(trees_temp, subout)
  }
}
 
Diameter_Mass <- lm(log(as.numeric(trees_out[,7]))~log(as.numeric(trees_out[,4])))
plot(log(as.numeric(trees_out[,4])), log(as.numeric(trees_out[,7])), xlim = c(0,6), ylim = c(0,12),
     xlab = "log ( Trunk Diameter  )", ylab = "log ( Total Stem Mass )")
abline(summary(Diameter_Mass)$coef[1,1], summary(Diameter_Mass)$coef[2,1], lwd = 3, lty = 3)
abline(0, 2.667, lwd = 3, lty = 1)
legend('topleft', legend=expression(R^2 == 0.975), bty='n', cex=3)

     



    
    
    
    