data <- read.csv("TreeReconstruction.csv", sep=',', head=T)

sum_lengths <- function(start_node){
	twig <- subdata[subdata$branch==start_node,]
	total_length = total_length + as.numeric(as.character(twig$length_cm))
	if (twig$parent==0)
		return(total_length)
	else
		return(total_length + sum_lengths(twig$parent))
}

correct_length <- function(start_node){
	twig <- subdata[subdata$branch==start_node,]
	parent <- subdata[subdata$branch==twig$parent,]	
  return((as.numeric(as.character(parent$length_cm)) - as.numeric(as.character(twig$parent_dist))))
}

get_path_fraction <- function(start_twig){ 
	#get path fraction requires the assignment of a global variable subdata
	paths <- c()
	for (i in start_twig:length(subdata[,1])){
		total_length = 0
		path_length = sum_lengths(subdata$branch[i]) - correct_length(subdata$branch[i])
		paths <- c(paths,path_length)
	}
	path_fraction = mean(paths)/max(paths)
	return( c(round(path_fraction, digits = 3), round(mean(paths), digits = 3), round(sd(paths), digits = 3), 
			as.integer(length(paths))))
}


#Path Fraction
path_fractions <- matrix(ncol=7, nrow=28)
colnames(path_fractions) <- c("species", "tree", "group", "Pf", "avg_path", "sd_path", "No_Twigs")

apple_trees <- c(3,4,5,13,14,15)

spp <- data[data$species=="apple",]
for (j in 1:6){
  tree <- spp[spp$tree==apple_trees[j],]
  twig_start <- length(tree[tree$parent_dist==0,][,1])+1
  no_scars <- tree[tree$length_cm!=0,]
  no_spurs <- tree[tree$spur!='Y',]
  just_twigs <- no_spurs[no_spurs$length_cm!=0,]
  
  path_fractions[(4*j-3):(4*j), 1] = "apple"
  path_fractions[(4*j-3):(4*j), 2] = apple_trees[j]
  total_length = 0
  subdata <- tree
  path_fractions[(4*j-3), 3:7] = c("All", get_path_fraction(twig_start))
  total_length = 0
  subdata <- no_scars
  path_fractions[(4*j-2), 3:7] = c("No scars", get_path_fraction(twig_start))
  total_length = 0
  subdata <- no_spurs
  path_fractions[(4*j-1), 3:7] = c("No spurs", get_path_fraction(twig_start))
  total_length = 0
  subdata <- just_twigs
  path_fractions[(4*j), 3:7] = c("Twigs only", get_path_fraction(twig_start))
} 

tree <- data[data$species=="cherry",]
twig_start <- length(tree[tree$parent_dist==0,][,1])+1
no_scars <- tree[tree$length_cm!=0,]
no_spurs <- tree[tree$spur!='Y',]
just_twigs <- no_spurs[no_spurs$length_cm!=0,]

path_fractions[25:28, 1] = "cherry"
path_fractions[25:28, 2] = 15
total_length = 0
subdata <- tree
path_fractions[25, 3:7] = c("All", get_path_fraction(twig_start))
total_length = 0
subdata <- no_scars
path_fractions[26, 3:7] = c("No scars", get_path_fraction(twig_start))
total_length = 0
subdata <- no_spurs
path_fractions[27, 3:7] = c("No spurs", get_path_fraction(twig_start))
total_length = 0
subdata <- just_twigs
path_fractions[28, 3:7] = c("Twigs only", get_path_fraction(twig_start))

#write.csv(path_fractions,"PathFractions.csv")

#no_broken <- data[data$broken!='y',]
#just_whole_twigs <- just_twigs[just_twigs$broken!='y',]