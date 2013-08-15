###This file summarizes data for the Branching Architecture analysis.

data <- read.csv("TreeReconstruction.csv", sep = ',', header = T)

#branch twig no.
apple_trees <- c(3,5,4,13,15,14)
segments <- matrix(nrow = 7, ncol = 3)
par(mfrow=c(3,3))
for (j in 1:6){
  spp <- data[data$species=="apple",]
  tree <- spp[spp$tree==apple_trees[j],]
  branches <- tree[tree$parent_dist==0,]
  segments[j,1] = apple_trees[j]
  segments[j,2] = length(branches[,1])
  segments[j,3] = length(tree[,1]) - length(branches[,1])
}
tree <- data[data$species=="cherry",]
branches <- tree[tree$parent_dist==0,]
segments[7,1] = 51
segments[7,2] = length(branches[,1])
segments[7,3] = length(tree[,1]) - length(branches[,1])
#####

#hist(order)
apple_trees <- c(3,5,4,13,15,14)
par(mfrow=c(3,3))
for (j in 1:6){
  spp <- data[data$species=="apple",]
  tree <- spp[spp$tree==apple_trees[j],]
  hist(tree$order)
}
tree <- data[data$species=="cherry",]
hist(tree$order)
#####

#hist(length)  
apple_trees <- c(3,5,4,13,15,14)
par(mfrow=c(3,3))
for (j in 1:6){
  spp <- data[data$species=="apple",]
  tree <- spp[spp$tree==apple_trees[j],]
  hist(tree$length_cm, breaks=30, freq = F)
}
tree <- data[data$species=="cherry",]
hist(tree$length_cm, breaks=30, freq = F)
#####

#hist(twig_length)  
apple_trees <- c(3,5,4,13,15,14)
par(mfrow=c(3,3))
for (j in 1:6){
  spp <- data[data$species=="apple",]
  tree <- spp[spp$tree==apple_trees[j],]
  twig <- tree[tree$parent_dist!=0,]
  hist(twig$length_cm, breaks=20, freq = F, xlim = c(0,200))
}
twig <- tree[tree$parent_dist!=0,]
hist(twig$length_cm, breaks=20, freq = F, xlim = c(0,200))
#####

#hist(log(branch$length_cm), breaks=30) #log normal
apple_trees <- c(3,5,4,13,15,14)
par(mfrow=c(3,3))
for (j in 1:6){
  spp <- data[data$species=="apple",]
  tree <- spp[spp$tree==apple_trees[j],]
  hist(log(tree$length_cm), breaks=30)
}
tree <- data[data$species=="cherry",]
hist(log(tree$length_cm), breaks=30)
#####

#plot(log(length),log(diameter))                     NS
apple_trees <- c(3,5,4,13,15,14)
volumelm <- vector(length = 7)
par(mfrow=c(3,3))
for (j in 1:6){
  spp <- data[data$species=="apple",]
  tree <- spp[spp$tree==apple_trees[j],]
  branch <- tree[tree$parent_dist==0,]
  plot(log(branch$length_cm), log(branch$diameter_mm))
  volumelm[j] = summary(lm(log(branch$diameter_mm)~log(branch$length_cm)))$r.squared
}
tree <- data[data$species=="cherry",]
branch <- tree[tree$parent_dist==0,]
plot(log(branch$length_cm), log(branch$diameter_mm))
volumelm[7] = summary(lm(log(branch$diameter_mm)~log(branch$length_cm)))$r.squared

#hist(declination)
apple_trees <- c(3,5,4,13,15,14)
par(mfrow=c(3,3))
for (j in 1:6){
  spp <- data[data$species=="apple",]
  tree <- spp[spp$tree==apple_trees[j],]
  branch <- tree[tree$parent_dist==0,]
  hist(branch$declination, breaks=10, freq=F, xlim=c(0,100))
}
tree <- data[data$species=="cherry",]
branch <- tree[tree$parent_dist==0,]
hist(branch$declination, breaks=20, freq=F, xlim=c(0,100))
#####

#hist(bearing)
apple_trees <- c(3,5,4,13,15,14)
par(mfrow=c(3,3))
for (j in 1:6){
  spp <- data[data$species=="apple",]
  tree <- spp[spp$tree==apple_trees[j],]
  branch <- tree[tree$parent_dist==0,]
  hist(branch$bearing, breaks=10, freq=F, xlim=c(0,360))
}
tree <- data[data$species=="cherry",]
branch <- tree[tree$parent_dist==0,]
hist(branch$bearing, breaks=20, freq=F, xlim=c(0,360))
#####

#plot(order, declination)                            S!
apple_trees <- c(3,5,4,13,15,14)
order_declination <- list()
par(mfrow=c(2,2), ps = 28, pch= 19)
for (j in 1:6){
  spp <- data[data$species=="apple",]
  tree <- spp[spp$tree==apple_trees[j],]
  branch <- tree[tree$parent_dist==0,]
  order_declination[[j]] <- lm(branch$declination~branch$order)
  plot(branch$order, branch$declination, cex = 2, ylim = c(-10,100),
       ylab = "branch angle", xlab = "branch order", 
       main = round(summary(order_declination[[j]])$r.squared, digits = 3))
  abline(a = summary(order_declination[[j]])$coef[1,1], 
         b = summary(order_declination[[j]])$coef[2,1], lwd = 3, lty = 3)
}
tree <- data[data$species=="cherry",]
branch <- tree[tree$parent_dist==0,]
order_declination[[7]] <- lm(branch$declination~branch$order)
plot(branch$order, branch$declination, cex = 2, ylim = c(-10,100),
     ylab = "branch angle", xlab = "branch order", 
     main = round(summary(order_declination[[7]])$r.squared, digits = 3))
abline(a = summary(order_declination[[7]])$coef[1,1], 
       b = summary(order_declination[[7]])$coef[2,1], lwd = 3, lty = 3)


order_dec_r <- vector(length = 7)
for (i in 1:7){ order_dec_r[i] = summary(order_declination[[i]])$r.squared }
#####

#plot(log(diameter), log(rank))                       S!
apple_trees <- c(3,5,4,13,15,14)
diameter_rank <- list()
par(mfrow=c(2,2), ps = 28, pch= 19)
for (j in 1:6){
  spp <- data[data$species=="apple",]
  tree <- spp[spp$tree==apple_trees[j],]
  branch <- tree[tree$parent_dist==0,]
  diameter_rank[[j]] <- lm(log(branch$rank)~log(branch$diameter_mm))
  plot(log(branch$diameter_mm), log(branch$rank), cex = 2, ylim = c(2,8), xlim = c(2,6),
       ylab = "log ( no. supported twigs )", xlab = "log ( branch diameter )", 
       main = paste(round(summary(diameter_rank[[j]])$r.squared, digits = 3), ",",
                    round(summary(diameter_rank[[j]])$coef[2,1], digits = 2)))
  abline(a = summary(diameter_rank[[j]])$coef[1,1], b = summary(diameter_rank[[j]])$coef[2,1], lwd = 3, lty = 3)

}
tree <- data[data$species=="cherry",]
branch <- tree[tree$parent_dist==0,]
diameter_rank[[7]] <- lm(log(branch$rank)~log(branch$diameter_mm))
plot(log(branch$diameter_mm), log(branch$rank), cex = 2, ylim = c(2,8), xlim = c(2,6),
     ylab = "log ( no. supported twigs )", xlab = "log ( branch diameter )", 
     main = paste(round(summary(diameter_rank[[7]])$r.squared, digits = 3), ",",
                  round(summary(diameter_rank[[7]])$coef[2,1], digits = 2)))
abline(a = summary(diameter_rank[[7]])$coef[1,1], b = summary(diameter_rank[[7]])$coef[2,1], lwd = 3, lty = 3)

diameter_rank_r <- vector(length = 7)
for (i in 1:7){ diameter_rank_r[i] = summary(diameter_rank[[i]])$r.squared }
#####
plot(log(branches$diameter_mm), log(branches$rank)) #S!
rank_v_diameter <- lm(log(branches$rank)~log(branches$diameter_mm))

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

t<-hist(twig$length)
mean(twig$length, na.rm=T) #21.223
median(twig$length, na.rm=T) #15


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

