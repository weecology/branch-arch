dat=read.csv(file=file.choose())
#n=dat$node
#a=dat$attach
#d=dat$pdiam.mm
#l=dat$l.cm
n=dat[,1] #branch ID
a=dat[,2] #attachment ID (parent)
d=dat[,3]#*1000 #diameter, convert as needed
l=dat[,4]#*100 #length, ditto
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
##########################################	

######get path lengths and path fraction##
node=tree$n
attach=tree$a
rnk=tree$r #supported twigs
le=tree$l #length
tot=length(twg) #total number of twigs
pth=numeric(tot) #will hold all path lengths
twg=node[rnk==1] #make a list of all twigs
ma=twg #start with twigs
pth=le[ma] #start summing path length
lst=1:length(twg) #indices for twigs
while(sum(ma)>0){
	#ma=ifelse(ma>0,attach[ma],0) #find mother's mother (attachment)
	ma[ma>0]=attach[ma[ma>0]]
	lst=which(ma>0)
	pth[lst]=pth[lst]+le[ma[lst]] #add to path until reach the ground
	#lwmd[lst]=lwmd[lst]+le[ma[lst]]*d[ma[lst]]
}
#print(tree$d[tree$a==0])
print(tot)
print(mean(pth)/max(pth)) #path fraction
#########################################

tree$pth=0
tree$pth[twg]=pth #add paths to data frame
#write.csv(tree,"Oak (Qgamb) arch w pth & rnk.csv",row.names=F)
