gc()
rm(list=ls())
library(rjags)
library(coda)
##Bring in data
#change directory if needed
data=read.csv("/Users/atredenn/Documents/Projects/SSDE-Project/R_Code/Allometry_2010_Mali/FINAL/MaliSavanna_TreeAllometry_Data_NoPTSU.csv")
names(data) = tolower(names(data))
data = data[data$aggregated_leaf_wt>-9000,] #take out bad data

##Set up matrix to store Species, LogDiameter, LogLength, and LogLeafM. NOTE: species coded as follows: 1 = C. geitynophyllum, 3 = D. microcarpum, 4 = C. glutinosum.
Y = matrix(nrow=length(data[,1]), ncol=4)

Y[,1] = log10(data$length)
Y[,2] = log10(data$total_mass)
Y[,3] = log10(data$aggregated_wood_wt)
Y[,4] = log10(data$aggregated_leaf_wt)

LogDiameter = log10(data$diameter)

##Set up positive definite matrix for derivition of Omega precision matrix
R = structure(.Data=c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), .Dim=c(4,4))

## Assign "N's" for indexing	
N=length(data$species)
Nsp = 3
x<-seq(0,2000, 20)/1000
Ntree=c(10,5,10)
beta.spp.diff <- matrix(nrow=3,ncol=4)
alpha.spp.diff <- matrix(nrow=3,ncol=4)

##Set-up for call to JAGS
data.J = list(R=R, N=N, Nsp=Nsp, Y=Y, SP=data$species, tree=data$tree, Ntree=Ntree, LogDiameter=LogDiameter, x=x, beta.spp.diff=beta.spp.diff, alpha.spp.diff=alpha.spp.diff)

inits=list(
	list(Omega = R),
	list(Omega = R),
	list(Omega = R)
)

vars <- c("mu.alpha", "mu.beta", "alpha.species", "beta.species", "Sigma", "p.mu", "p.fit",  "spp.var", "ind.var", "betalength.coge", "betamass.coge", "betawoodm.coge", "betaleafm.coge", "betalength.demi", "betamass.demi", "betawoodm.demi", "betaleafm.demi", "betalength.cogl", "betamass.cogl", "betawoodm.cogl", "betaleafm.cogl", "beta.spp.diff", "alpha.spp.diff")


#for production runs: 50,000 adapt; 200,000 burn-in; 500,000 samples
jm = jags.model("/Users/atredenn/Documents/Projects/SSDE-Project/R_Code/Allometry_2010_Mali/FINAL/JAGS_AllDataAllometry_MaliSavanna.R", data=data.J, inits, n.chains=length(inits), n.adapt=50000)

update(jm, n.iter=200000)

zm = coda.samples(jm, variable.names=vars, n.iter=1000000, thin=10)
df = as.data.frame(rbind(zm[[1]], zm[[2]], zm[[3]]))

zm.quantile	=summary(zm)$quantile
zm.stat=summary(zm)$stat
write.csv(zm.quantile, file="/users/atredenn/desktop/fullAllomModel_SizeOnly_Branch_Quants-6-17-2012.csv")
write.csv(zm.stat, file="/users/atredenn/desktop/fullAllomModel_SizeOnly_Branch_Stats-9-17-2012.csv")

# zj = jags.samples(jm, variable.names=vars, n.iter=10000, n.thin=20)

save(list=c("zm", "df"), file="/Users/atredenn/Desktop/AllDataAllometry_OUTPUT.RData")

h <- heidel.diag(zm)
h.write <- as.data.frame(rbind(h[[1]], h[[2]], h[[3]]))
write.csv(h.write, file="/users/atredenn/desktop/fullAllomModel_SizeOnly_Branch_Heidel-6-19-2012-1.csv")

g <- matrix(NA, nrow=nvar(zm), ncol=2)
for (v in 1:nvar(zm)) {
    g[v,] <- gelman.diag(zm[,v])$psrf
	}

write.csv(g, file="/users/atredenn/desktop/fullAllomModel_SizeOnly_Branch_Gelman-6-19-2012.csv")










