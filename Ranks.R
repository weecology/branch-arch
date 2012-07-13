data <- read.csv("WholeTree15.csv", sep=',', head=T)

get_rank <- function(start_node){
	daughters <- data[data$attach==start_node,]
	rank = rank + length(daughters[,1])
	if (length(daughters[,1])>0)
		return(rank)
	else
		for (i in 1:length(daughters[,1])){
			return(rank + get_rank(daughters$node[i]))
		}
}


#Rank

rank=1
node=1
get_rank(node)