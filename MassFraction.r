data <- read.csv("BranchSegments.csv", sep=',', head=T)

sum_up_mass <- function(start_nodes){
  total_mass = total_mass + sum(subdata$stem_m[start_nodes])
  daughters <- subdata[subdata$parent==c(start_nodes[[1]]),]
	if (length(daughters[,1]) > 0){
    total_mass + sum(daughters$stem_m)
    return(sum_up_mass(daughters$branch))}
	else
		return(total_mass)
}

sum_down_mass <- function(start_node){
	twig <- subdata[subdata$branch==start_node,]
	total_mass = total_mass + as.numeric(as.character(twig$stem_m))
	if (twig$parent==0)
		return(total_mass)
	else
		return(total_mass + sum_mass(twig$parent))
}

get_mass_fraction <- function(start_twig){ 
	#get mass requires the assignment of a global variable subdata
	paths <- c()
	for (i in start_twig:length(subdata[,1])){
		total_mass = 0
		path_mass = sum_mass(subdata$branch[i])
		paths <- c(paths,path_mass)
	}
	mass_fraction = mean(paths)/max(paths)
	return( c(round(mass_fraction, digits = 3), round(mean(paths), digits = 3), round(sd(paths), digits = 3), 
			as.integer(length(paths))))
}

cherry_trees <- c(1,7,10,15)
spp <- data[data$species=="cherry",]

#Sum mass
for (i in 1:4){
	ind <- spp[spp$tree==cherry_trees[i],]
  subdata <- ind[-1,]
	subout = data.frame(tree = subdata$tree, branch = subdata$branch, tot_stem_m = 0)
	for (j in 1:length(subdata[,1])){
        total_mass = 0
    		subout$tot_stem_m[j] = sum_up_mass(list(j))
  }
  if (exists('trees_out'))
      trees_out = rbind(trees_out, subout)
  else
      trees_out <- subout
}

#Mass Fraction
mass_fractions <- matrix(ncol=6, nrow=4)
colnames(mass_fractions) <- c("species", "tree", "Mf", "avg_mass", "sd_mass", "no_stems")


for (j in 1:4){
  tree <- spp[spp$tree==cherry_trees[j],]
  twig_start = 2
  
  mass_fractions[j,1] = "cherry"
  mass_fractions[j,2] = cherry_trees[j]
  total_mass = 0
  subdata <- tree
  mass_fractions[j, 3:6] = c(get_mass_fraction(twig_start))
} 


#write.csv(mass_fractions,"MassFractions.csv")