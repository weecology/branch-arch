###This file summarizes data for the Branching Architecture analysis.

branch <- read.csv("BranchSegments.csv", sep=',', header=T)
twig <- read.csv("TwigSegments.csv", sep=',', header=T)


hist(branch$order)
hist(branch$length_cm, breaks=30) #gamma dist
hist(log(branch$length_cm), breaks=30) #log normal

plot(log(branch$length_cm), log(branch$diameter_mm))
volumelm <- lm(log(branch$diameter)~log(branch$length_cm)) #NS R^2 = 0.003
trees = c(1,7,10,15)

##Branching Ratio
#all 
no_branched <- list()
avg_ratio <- matrix(ncol=2, nrow=15)
for(i in trees){
	tree <- branch[branch$tree==i,]
	no_branched[[i]] <- matrix(ncol=3, nrow=(length(tree[,1])-1))
	for(j in 1:(length(tree[,1])-1)){
		no_branched[[i]][j,1] = j
		no_branched[[i]][j,2] = (length(tree[tree$parent==j,][,1])-1)
		order <- subset(tree, branch==j, select = order)
		no_branched[[i]][j,3] = order[,1]
	}
	tree_lim <- no_branched[[i]][no_branched[[i]][,2]>0,]
	avg_ratio[i,1] = i
	avg_ratio[i,2] = mean(tree_lim[,2])
}
	
#by order
avg_ratio_order <- matrix(ncol=7, nrow=15)
for (i in trees){
	tree_lim <- no_branched[[i]][no_branched[[i]][,2]>1,]
	avg_ratio_order[i,1] = i
	for (j in unique(tree_lim[,3])){
		order <- subset(tree_lim, tree_lim[,3]==j)
		avg_ratio_order[i,(j+1)] = mean(order[,2])
	}
}

##Twigs Summary for 15
branches15 <- length(branch[branch$tree==15,][,1])
summary_twigs <- matrix(ncol=4, nrow=(branches15-1))
for (i in 1:(branches15-1)){
	sub_twig <- twig[twig$parent==i,]
	summary_twigs[i,1] = i
	summary_twigs[i,3] = length(sub_twig[sub_twig$spur=='y',][,1]) #spurs
	summary_twigs[i,4] = length(sub_twig[sub_twig$length==0,][,1]) #scars
	summary_twigs[i,2] =length(sub_twig[,1]-(summary_twigs[i,3]+summary_twigs[i,4])) #twigs
}

write.csv(summary_twigs,"TwigSummary15.csv")