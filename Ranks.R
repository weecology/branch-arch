data <- read.csv("WholeTree15.csv", sep=',', head=T)

n=data[,1] #branch ID
a=data[,2] #attachment ID (parent)
d=data[,4]#*1000 #diameter, convert as needed
l=data[,3]#*100 #length, ditto
r=0 
tree=data.frame(n=n,a=a,r=r,d=d,l=l,x1=NA,y1=NA,x2=NA,y2=NA) #define the tree
twg=stack(tree[,1:2]) #put node and attach together
twg=unique(twg$values,fromLast=T) #find unique numbers. Starting from the last effectively cuts out parent node numbers from the "node" side. Zero marks the differnce between node and attaches
cut=which(twg==0) #find the position of the zero
prnt=twg[(cut+1):length(twg)] #parents after the zero
twg=twg[1:(cut-1)] #Twigs before the zero

#####assign ranks, 1 twig at a time#######
for(i in 1:length(twg)){ 
  ma=twg[i] #mother
  mai=which(tree$n==ma) #mother's index
  tree$r[mai]=tree$r[mai]+1 #increase rank
  while(ma>0){
    ma=tree$a[mai] #find mother's mother (attachment)
    mai=which(tree$n==ma) #new index
    tree$r[mai]=tree$r[mai]+1 #up the rank
  }
}

write.csv(tree, "treetemp.csv")

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


#Twig order

orders <- vector(length = 2812)
for (i in 266:3077){
  parent = data$attach[i]
  orders[(i-265)] = data$order[parent]+1
}