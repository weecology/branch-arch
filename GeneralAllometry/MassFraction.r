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
		return(total_mass + sum_down_mass(twig$parent))
}

get_mass_fraction_up <- function(start_twig){ 
	#get mass requires the assignment of a global variable subdata
	paths <- c()
	for (i in start_twig:length(subdata[,1])){
	
		total_mass = 0
		path_mass = sum_up_mass(subdata$branch[i])
		paths <- c(paths,path_mass)
	
	}
	mass_fraction = mean(paths)/max(paths)
	return( c(round(mass_fraction, digits = 3), max(paths), round(mean(paths), digits = 3), round(sd(paths), digits = 3), 
			as.integer(length(paths))))
}

get_mass_fraction <- function(start_twig){ 
  #get mass requires the assignment of a global variable subdata
  paths <- c()
  for (i in start_twig:length(subdata[,1])){
    if (!(i %in% subdata$parent)){
    total_mass = 0
    path_mass = sum_down_mass(subdata$branch[i])
    paths <- c(paths,path_mass)
    }
  }
  mass_fraction = mean(paths)/max(paths)
  return( c(round(mass_fraction, digits = 3), max(paths), round(mean(paths), digits = 3), round(sd(paths), digits = 3), 
            as.integer(length(paths))))
}

#Mass Fraction
mass_fractions <- matrix(ncol=7, nrow=24)
colnames(mass_fractions) <- c("species", "tree", "Mf", "max_path", "avg_mass", "sd_mass", "no_stems")

apple_trees <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20)
cherry_trees <- c(1,7,10,13,15)

spp <- data[data$species=="apple",]
for (j in 1:length(apple_trees)){
  subdata <- spp[spp$tree==apple_trees[j],]
  
  mass_fractions[j,1] = "apple"
  mass_fractions[j,2] = apple_trees[j]
  total_mass = 0
  paths <- c()
  for (i in 1:length(subdata[,1])){
    if (!(i %in% subdata$parent)){
      total_mass = 0
      path_mass = sum_down_mass(subdata$branch[i])		
      paths <- c(paths,path_mass)
    }
  }
  mass_fraction = mean(paths)/max(paths)
  mass_fractions[j,3] = round(mass_fraction, digits = 3)
  mass_fractions[j,4] = max(paths)
  mass_fractions[j,5] = round(mean(paths), digits = 3) 
  mass_fractions[j,6] = round(sd(paths), digits = 3)
  mass_fractions[j,7] = as.integer(length(paths))
}

spp <- data[data$species=="cherry",]
for (j in 1:length(cherry_trees)){
  subdata <- spp[spp$tree==cherry_trees[j],]
  
  mass_fractions[(j+19),1] = "cherry"
  mass_fractions[(j+19),2] = cherry_trees[j]
  total_mass = 0
  paths <- c()
  for (i in 1:length(subdata[,1])){
    if (!(i %in% subdata$parent)){
      total_mass = 0
      path_mass = sum_down_mass(subdata$branch[i])  	
      paths <- c(paths,path_mass)
    }
  }
  mass_fraction = mean(paths)/max(paths)
  mass_fractions[(j+19),3] = round(mass_fraction, digits = 3)
  mass_fractions[(j+19),4] = max(paths)
  mass_fractions[(j+19),5] = round(mean(paths), digits = 3) 
  mass_fractions[(j+19),6] = round(sd(paths), digits = 3)
  mass_fractions[(j+19),7] = as.integer(length(paths))
}

write.csv(mass_fractions,"MassFractions.csv")


###UNUSED CODE
for (j in 1:4){
  tree <- spp[spp$tree==cherry_trees[j],]
  twig_start = 2
  
  mass_fractions[j,1] = "cherry"
  mass_fractions[j,2] = cherry_trees[j]
  total_mass = 0
  subdata <- tree
  mass_fractions[j, 3:6] = c(get_mass_fraction_up(twig_start))
} 

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

