###This file summarizes data for the Branching Architecture analysis.

branch <- read.csv("BranchSegments.csv", sep=',', header=T)
twig <- read.csv("TwigSegments.csv", sep=',', header=T)
tree15 <- read.csv("WholeTree15.csv", sep=',', header=T)
branches <- tree15[1:265,]


hist(branch$order)
hist(branch$length_cm, breaks=30) #gamma dist
hist(log(branch$length_cm), breaks=30) #log normal

plot(log(branch$length_cm), log(branch$diameter_mm))
volumelm <- lm(log(branch$diameter_mm)~log(branch$length_cm)) #NS R^2 = 0.003
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
	summary_twigs[i,2] = length(sub_twig[,1]-(summary_twigs[i,3]+summary_twigs[i,4])) #twigs
}

write.csv(summary_twigs,"TwigSummary15.csv")

#length vs. twigs/scars/stems/order/rank
plot(branch$length_cm, branch$twigs)
plot(branch$length_cm, branch$scars)
plot(branch$length_cm, branch$spurs)
plot(tree15$order, tree15$length_cm)
plot(log(branches$rank), branches$length_cm) #What could be causing this separation?

#diameter vs. twigs/scars/stems/order/rank
plot(branch$diameter_mm, branch$twigs) #NS
plot(branch$diameter_mm, branch$scars) #NS
plot(branch$diameter_mm, branch$spurs) #NS
plot(tree15$order, tree15$diameter_mm) #NS
plot(log(branches$diameter_mm), log(branches$rank)) #S!
rank_v_diameter <- lm(log(branches$rank)~log(branches$diameter_mm)) #R2 = 0.7959

#For poster
plot(log(branches$diameter_mm), log(branches$rank), xlab="log D", ylab="No. of supported twigs", font.lab=4, cex.lab=1.5)
abline(-0.88,1.52)


#order vs. twigs/scars/stems
plot(branch$order, branch$twigs)
plot(branch$order, branch$scars)
plot(branch$order, branch$spurs)

#rank vs. order
plot(branches$order,log(branches$rank))
g_rank_v_order <- glm(log(branches$rank)~branches$order, family = gaussian)
rank_v_order <- lm(log(branches$rank)~branches$order) #R2 = 0.4153

#vertical accumulation of stems/leaves/mass (by scaffold)