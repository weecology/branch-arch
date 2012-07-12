data <- read.csv("WholeTree15.csv", sep=',', head=T)

sum_lengths <- function(start_node){
	twig <- subdata[subdata$node==start_node,]
	total_length = 0
	total_length = total_length + twig$length_cm
	if (twig$attach==0)
		return(total_length)
	else
		return(total_length + sum_lengths(twig$attach))
}

correct_length <- function(start_node){
	twig <- subdata[subdata$node==start_node,]
	parent <- subdata[subdata$node==twig$attach,]	
	if (twig$parent_dist>0)
		return((parent$length_cm - twig$parent_dist))
	else
		return(0) 
}

get_path_fraction <- function(){ 
	#get pat fraction requires the assignment of a global variable subdata
	paths <- c()
	for (i in 266:length(subdata[,1])){
		total_length = 0
		path_length = sum_lengths(subdata$node[i]) - correct_length(subdata$node[i])
		paths <- c(paths,path_length)
	}
	path_fraction = mean(paths)/max(paths)
	return( c(round(path_fraction, digits = 3), 
			as.integer(length(paths))))
}


#Path Fraction
path_fractions <- matrix(ncol=3, nrow=6)
colnames(path_fractions) <- c("Group", "Pf", "No_Twigs")

no_spurs <- data[data$spur!='y',]
no_scars <- data[data$length_cm!=0,]
no_broken <- data[data$broken!='y',]
just_twigs <- no_spurs[no_spurs$length_cm!=0,]
just_whole_twigs <- just_twigs[just_twigs$broken!='y',]

subdata = data
path_fractions[1,] = c("All", get_path_fraction())
subdata = no_spurs
path_fractions[2,] = c("No spurs", get_path_fraction())
subdata = no_scars
path_fractions[3,] = c("No scars", get_path_fraction())
subdata = no_broken
path_fractions[4,] = c("No broken", get_path_fraction())
subdata = just_twigs
path_fractions[5,] = c("Twigs only", get_path_fraction())
subdata = just_whole_twigs
path_fractions[6,] = c("Whole Twigs", get_path_fraction())
