branch <- read.csv("BranchSegments.csv", sep=',', head=T)
twig <- read.csv("TreeReconstruction.csv", sep=',', head=T)

species <- c("apple", "cherry")
apple_trees <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20)
cherry_trees <- c(1,7,10,13,15)

spp <- branch[branch$species=="apple",]
tree <- spp[spp$tree==1,]
path_length = vector(length = length(tree[,1]))
for (j in length(tree[,1]):1){
     daughters <- tree[tree$parent==tree$branch[j],]
     if (length(daughters[,1]) > 0){
       tree$path_length[j] = tree$length_cm[j] + sum(daughters$path_length)}
     else{
       tree$path_length[j] = tree$length_cm[j]}     
}


###Find end lengths INCOMPLETE
apple_trees <- c(3,4,5,13,14,15)

spp <- twig[twig$species=="apple",]
tree <- spp[spp$tree==3,]
end <- matrix(ncol=3, nrows = ???)
for (j in 1:13){
  length <- as.numeric(tree$length_cm[j])
  daughters <- tree[tree$parent==j,]
  end_twig <- daughters[daughters$parent_dist==length,]
  if (length(end_twig[,1])>0){
    end[j] = max(end_twig$length_cm)}
  else{
    end[j] = 0}
}
  
