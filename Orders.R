#Maintains observed order for branches and generates order for twigs
data <- read.csv("TreeReconstruction.csv", sep=',', head=T)

apple_trees <- c(3,4,5,13,14,15)

spp <- data[data$species=="apple",]
for (j in 1:2){
  if (j==7) 
    tree <- data[data$species=="cherry",]
  else 
    tree <- spp[spp$tree==apple_trees[j],]

  subout = data.frame(branch = tree$branch, parent = tree$parent, order = 0)
  branch_end <- length(tree[tree$parent_dist==0,][,1])
  twig_start <- length(tree[tree$parent_dist==0,][,1])+1
  
  for (i in 1:branch_end){
    subout$order[i] = tree$order[i]
  }
  for (i in twig_start:length(tree[,1])){
      parent = tree$parent[i]
      subout$order[i] = tree$order[parent]+1     
  }
  
  if (exists('output'))
    output = rbind(output, subout)
  else
    output <- subout
}
  
write.csv(output, "Orders.csv")

#Recursion attempt
get_rank <- function(start_node){
	for (i in 1:length(start_node)){
		daughters <- data[data$attach==start_node,]
		rank = rank + length(daughters[,1])
		if (length(daughters[,1])==0)
			if (i == length(start_node))
				return(rank)
		else
			return(rank + get_rank(daughters$node[i]))
		}
}

get_rank_alt <- function(start_node){
	for (i in length(node)){
		new_nodes <- subset(data, attach==node[i], select=node)
		nodes = c(node, c(new_nodes))
		tier_nodes = c(tier_nodes, c(new_nodes))
	}
	if (length(tier_nodes)==0)
		return (length(nodes))
	else
		return (get_rank_alt(c(tier_nodes)))
}

rank=1
tier_nodes=0
node=c(69)
rank = get_rank_alt(node)
