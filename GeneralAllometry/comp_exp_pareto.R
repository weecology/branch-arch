#Generates AICc values to determine best-fit of histrogram data.

branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)

get_mle_expo = function(num_list){
  # Obtain the MLE parameter of the exponential distribution
  return(1 / mean(num_list))
}

get_mle_pareto = function(num_list){
  # Obtain the MLE parameter of the loglinear distribution
  x_m = min(num_list)
  alpha = length(num_list) / sum(log(num_list) - log(x_m))
  return(alpha)
}

get_aicc_expo_pareto = function(num_list_orig){
  # Return the AICc values of the exponential and the loglinear distributions
  # Exponential distribution has 1 parameter while loglinear has 2
  num_list = num_list_orig[!is.na(num_list_orig)]
  par_exp = get_mle_expo(num_list)
  alpha = get_mle_pareto(num_list)
  loglik_exp = sum(dexp(num_list, par_exp, log = T))
  loglik_pareto = sum(log(alpha) + alpha * log(min(num_list)) - (alpha + 1) * log(num_list))
  AICc_exp = 2 * 1 - 2 * loglik_exp + 2 * 1 * 2 / (length(num_list) - 1 - 1)
  AICc_pareto = 2 * 2 - 2 * loglik_pareto + 2 * 2 * 3 / (length(num_list) - 2 - 1)
  return(c(AICc_exp, AICc_pareto))
}


#Species groups - branch level
groups <- c("all-branch", "cherry", "apple")
group_data <- list()
group_data[[1]] <- branch_size
group_data[[2]] <- branch_size[branch_size$species=="cherry",]
group_data[[3]] <- branch_size[branch_size$species=="apple",]

for (i in 1:3){
  subout <- matrix(nrow = 1, ncol = 7)
  subout[1] = groups[i]

  rm_zeros <- group_data[[i]][group_data[[i]]$diameter_mm>0 & group_data[[i]]$length_cm>0 & group_data[[i]]$stem_m>0,]
  
  subout[2] = paste(round(get_aicc_expo_pareto(rm_zeros$diameter_mm)[1],0), " , ", 
                    round(get_aicc_expo_pareto(rm_zeros$diameter_mm)[2],0))
  
  subout[3] = paste(round(get_aicc_expo_pareto(rm_zeros$length_cm)[1],0), " , ", 
                    round(get_aicc_expo_pareto(rm_zeros$length_cm)[2],0))
  
  subout[4] = paste(round(get_aicc_expo_pareto(rm_zeros$path_length)[1],0), " , ", 
                    round(get_aicc_expo_pareto(rm_zeros$path_length)[2],0))
  
  subout[5] = paste(round(get_aicc_expo_pareto(rm_zeros$tot_length)[1],0), " , ", 
                    round(get_aicc_expo_pareto(rm_zeros$tot_length)[2],0))
  
  subout[6] = paste(round(get_aicc_expo_pareto(rm_zeros$stem_m)[1],0), " , ", 
                    round(get_aicc_expo_pareto(rm_zeros$stem_m)[2],0))
  
  subout[7] = paste(round(get_aicc_expo_pareto(rm_zeros$tot_stem_m)[1],0), " , ", 
                    round(get_aicc_expo_pareto(rm_zeros$tot_stem_m)[2],0))
  
  if (exists('hist_fit'))
    hist_fit = rbind(hist_fit, subout)
  
  else
    hist_fit <- subout
    colnames(hist_fit) <- c('group', 'diameter', 'length', 'path', 'subtree', 'mass', 'tot_mass')
}

#Individuals - branch level
species <- list(list("apple",
                     c(2,7,12,3,5,11,6,8,10,1,4,9,13,17,15,18,20,19,14),
                     c("Bud.9-1", "Bud.9-2", "Bud.9-3", "Bud.9-4", "CG.3041-1", "CG.3041-2",  
                       "CG.3041-3", "CG.3041-4", "CG.6210-1", "CG.6210-2", "CG.6210-3", 
                       "CG.6210-4", "M.26", "JM.8-1", "JM.8-2", "JM.8-3",
                       "PiAu.5683-1", "PiAu.5683-2", "PiAu.5683-3")),
                list("cherry", 
                     c(7,13,15,1,10),
                     c("cherry-1", "cherry-2", "cherry-3", "cherry-4", "cherry-5")))

for (i in 1:2){
  spp <- branch_size[branch_size$species==species[[i]][1],]
  for (j in 1:length(species[[i]][[2]])){
    subout <- matrix(nrow = 1, ncol = 7)
    
    subout[1] = species[[i]][[3]][j]
    
    rm_zeros <- spp[spp$tree==species[[i]][[2]][j] & spp$diameter_mm>0 & spp$length_cm>0 & spp$stem_m>0,]
    
    subout[2] = paste(round(get_aicc_expo_pareto(rm_zeros$diameter_mm)[1],0), " , ", 
                      round(get_aicc_expo_pareto(rm_zeros$diameter_mm)[2],0))
    
    subout[3] = paste(round(get_aicc_expo_pareto(rm_zeros$length_cm)[1],0), " , ", 
                      round(get_aicc_expo_pareto(rm_zeros$length_cm)[2],0))
    
    subout[4] = paste(round(get_aicc_expo_pareto(rm_zeros$path_length)[1],0), " , ", 
                      round(get_aicc_expo_pareto(rm_zeros$path_length)[2],0))
    
    subout[5] = paste(round(get_aicc_expo_pareto(rm_zeros$tot_length)[1],0), " , ", 
                      round(get_aicc_expo_pareto(rm_zeros$tot_length)[2],0))
    
    subout[6] = paste(round(get_aicc_expo_pareto(rm_zeros$stem_m)[1],0), " , ", 
                      round(get_aicc_expo_pareto(rm_zeros$stem_m)[2],0))
    
    subout[7] = paste(round(get_aicc_expo_pareto(rm_zeros$tot_stem_m)[1],0), " , ", 
                      round(get_aicc_expo_pareto(rm_zeros$tot_stem_m)[2],0))
    
    hist_fit = rbind(hist_fit, subout)
  }
}

write.csv(hist_fit, 'HistResults.csv')