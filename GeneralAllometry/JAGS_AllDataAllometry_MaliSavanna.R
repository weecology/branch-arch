model{

#########################################################
# This is JAGS code to accompany Tredennick et al. 20xx #
######################################################### 
	

########## Term definitions #############
# Nsp = number of species				#
# Ntree = number of trees				#
# m = trait counter						#
# k = species counter					#
# i = observation counter				#
#########################################


##HYPERPRIORS

	#define the 4x4 precision matrix in Equation 4
	Omega[1:4, 1:4] ~ dwish(R[1:4,1:4], 4) #R refers to the positive definite matrix and 4 is the degrees of freedom
	Sigma[1:4, 1:4] <- inverse(Omega[1:4, 1:4])
	
	#alpha (normalizing constant) and beta (scaling exponent) priors 
	#define prior distributions of scaling parameters for each trait m (where m = length, aboveground mass, stem mass, or leaf mass)
	for (m in 1:4){
		#define 'global' prior distribution as uniform/uniformative
		mu.alpha[m] ~ dunif(-10,10)
		mu.beta[m]~ dunif(-10,10)
		
		#define precision terms for global distributions
		#species-level error
		sig.alpha[m] ~ dunif(0,2)
		tau.alpha[m] <- 1/sig.alpha[m]^2
		sig.beta[m] ~ dunif(0,2)
		tau.beta[m] <- 1/sig.beta[m]^2
		
		#individual-level error
		sig.alpha.ind[m] ~ dunif(0,2)
		tau.alpha.ind[m] <- 1/sig.alpha.ind[m]^2
		sig.beta.ind[m] ~ dunif(0,2)
		tau.beta.ind[m] <- 1/sig.beta.ind[m]^2
		
		#Compute variance terms
		spp.var[m] <- sig.beta[m]^2	
		ind.var[m] <- sig.beta.ind[m]^2
		
		} #close variable loop (m)
	
	
##HIERARCHICAL PRIORS

	#Prior for precision of latent Rho based on Elzinga et al. (2005) error rate (see Price et al. 2009); "Measurement 	error rates of 5% of tree diameter or greater may be expected in dbh measurements on as many as 5% of measured 	trees." According to Price et al., "...choose lognormal prior for sigD that gives E(sigD) = 0.021 and P(sigD > 0.05) 	= 0.054; Note, sigD describes the measurement error on the log-scale and thus can be interpretted as the 	"multiplicative" or "percent" error rate."
	sigD ~ dlnorm(-4.135,2)
	tauD <- pow(sigD,-2)
	
	#Priors for alpha and beta
	for(k in 1:Nsp){
		for (m in 1:4){
			#These are species specific (species k, trait m)
			alpha.species[k,m] ~ dnorm(mu.alpha[m], tau.alpha[m])
			beta.species[k,m] ~ dnorm(mu.beta[m], tau.beta[m])
			
			for(i in 1:Ntree[k]){
				#these are tree specific (tree i, species k, trait m)
				alpha[i,k,m] ~ dnorm(alpha.species[k,m], tau.alpha.ind[m])
				beta[i,k,m] ~ dnorm(beta.species[k,m], tau.beta.ind[m])
				
				}
			} #close variable loop (m)
		} #close species loop (k)


##LIKELIHOOD and DATA MODEL


for (i in 1:N){	
	##Berkson model for "true"/latent log diameter (see Price et al. 2009)
	Lrho[i] ~ dnorm(LogDiameter[i], tauD)

	##Define mean vector, i.e the scaling model where alpha is the species-specific normalizing constant and beta is species-specific scaling exponent.
	for(m in 1:4){
		mu.all[i,m] <- alpha[tree[i],SP[i],m] + beta[tree[i],SP[i],m]*Lrho[i] 
				
		#generate new mu for poseterior predictive check
		mu.new[i,m] <- alpha[tree[i],SP[i],m] + beta[tree[i],SP[i],m]*Lrho[i]
	}
	
	##The likelihood is a multivariate normal distribution with mean mu and precision matrix Omega. Data are as 	follows: Y[,1] = loglength, Y[,2] = logleafmass, and Y[,3] = logwoodmass.
	Y[i,1:4] ~ dmnorm(mu.all[i,1:4], Omega[1:4,1:4])
	#generate new data for posterior predictive check
	Y.new[i, 1:4] ~ dmnorm(mu.new[i,1:4], Omega[1:4,1:4])
	
	} #close observation (i) loop



# Posterior predictive checks
for (i in 1:5){
		betalength.coge[i]<-beta[i,2,1]
		betamass.coge[i]<-beta[i,2,2]
		betawoodm.coge[i]<-beta[i,2,3]
		betaleafm.coge[i]<-beta[i,2,4]
		}
for (i in 1:10){
		betalength.demi[i]<-beta[i,1,1]
		betamass.demi[i]<-beta[i,1,2]
		betawoodm.demi[i]<-beta[i,1,3]
		betaleafm.demi[i]<-beta[i,1,4]
		}
for (i in 1:10){
		betalength.cogl[i]<-beta[i,3,1]
		betamass.cogl[i]<-beta[i,3,2]
		betawoodm.cogl[i]<-beta[i,3,3]
		betaleafm.cogl[i]<-beta[i,3,4]
		}


# Posterior predictive checks
for (m in 1:4){
	p.mu[m] <- step(mean(Y[,m])-mean(Y.new[,m]))
	p.sd[m] <- step(sd(Y[,m])-sd(Y.new[,m]))
	for (i in 1:N){
		sq[i,m] <- (Y[i,m]-mu.all[i,m])^2
		sq.new[i,m] <- (Y.new[i,m]-mu.all[i,m])^2
	}
	fit[m] <- sum(sq[,m])
	fit.new[m] <- sum(sq.new[,m])
	p.fit[m] <-step(fit[m]-fit.new[m])
	
	
	beta.spp.diff[1,m] <- beta.species[1,m] - beta.species[2,m]
	beta.spp.diff[2,m] <- beta.species[1,m] - beta.species[3,m]
	beta.spp.diff[3,m] <- beta.species[2,m] - beta.species[3,m]
	
	
	alpha.spp.diff[1,m] <- alpha.species[1,m] - alpha.species[2,m]
	alpha.spp.diff[2,m] <- alpha.species[1,m] - alpha.species[3,m]
	alpha.spp.diff[3,m] <- alpha.species[2,m] - alpha.species[3,m]
	}
	
	
}
