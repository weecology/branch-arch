# This script analyses the Yield ~ exp relationship

# Functions ---
library('dplyr')
library('stringr')
library('agricolae')

get_data <- function(){
  # Compiles data for computation from the visually oriented SMAResults file
  
  sma <- read.csv("SMAResults.csv", sep=",", head=T) 
  
  input_data <- list()
  for (i in 1:27){                              # Scaling relationships
    input_data[[i]] <- list()
    for (j in 1:7){                             # Results output
      #  1. Intercept, 2. Int CI-, 3. Int CI+, 
      #  4. Exponent,  5. Exp CI-, 6. Exp CI+, 7.R2 
      input_data[[i]][[j]] <- vector(length = 32)
      for (k in 1:32) {                         # Groups and individuals
        input_data[[i]][[j]][k] = as.numeric(str_split(
                                    sma[(k+1),(i+2)], ";")[[1]][j])
      }
    }
  }
  return(input_data)
}

mark_significance <- function(r2_value){
  if (r2_value >= 0.85) {
    return(paste("***", r2_value))
  } else if (r2_value >= 0.6) {
    return(paste("**", r2_value))
  } else if (r2_value >= 0.4) {
    return(paste("*", r2_value))
  } else {
    return("")
  }
}

init <- function(){
  
  tree_ids     <<- c(2,7,12,3,  # used with tree names as reference for 
                    5,11,6,8,   # tree_order, roots_exc, and roots_loc 
                    10,1,4,9,
                    13,
                    17,15,18,
                    20,19,14)
  
  
  tree_names   <<- c("Bud.9-1", "Bud.9-2", "Bud.9-3", "Bud.9-4", 
                     "CG.3041-1", "CG.3041-2", "CG.3041-3", "CG.3041-4", 
                    "CG.6210-1", "CG.6210-2", "CG.6210-3", "CG.6210-4", 
                    "M.26", 
                    "JM.8-1", "JM.8-2", "JM.8-3",
                    "PiAu.5683-1", "PiAu.5683-2", "PiAu.5683-3")
  
  rootstocks   <<- c("Bud.9", "Bud.9", "Bud.9", "Bud.9", 
                     "CG.3041", "CG.3041", "CG.3041", "CG.3041", 
                     "CG.6210", "CG.6210", "CG.6210", "CG.6210", 
                     "M.26", 
                     "JM.8", "JM.8", "JM.8",
                     "PiAu.5683", "PiAu.5683", "PiAu.5683")
  
  tree_order   <<- c(2,7,12,3,
                     5,11,6,8,
                     10,1,4,9,
                     13,
                     16,15,17,  # tree_ids - 1 after 16
                     19,18,14)
  
  roots_loc    <<- c(3, 9, 15, 21, 23, 28)  
    # Gets rootstock level values
   
  roots_exc    <<- c(1, 2, 3, 8, 9, 11, 15, 19, 22, 23, 26, 28, 32)
    # Gets tree level values
  
  relationships  <<- c("L~D (Segment)", "L~D (Path)", "L~D (Subtree)", 
                       "SA~V (Segment)", "SA~V (Path)", "SA~V (Subtree)", 
                       "D~V (Segment)", "D~V (Path)", "D~V (Subtree)", 
                       "L~V (Segment)", "L~V (Path)", "L~V (Subtree)", 
                       "D~SA (Segment)", "D~SA (Path)", "D~SA (Subtree)", 
                       "L~SA (Segment)", "L~SA (Path)", "L~SA (Subtree)", 
                       "L~M (Segment)", "L~M (Path)", "L~M (Subtree)", 
                       "M~D (Segment)", "M~D (Subtree)", 
                       "M~V (Segment)", "M~V (Path)", "M~V (Subtree)", 
                       "D/P Ratio ~ P Diam")
  
}  

# Data initialization ----
sma_res <- get_data()

tree_sum <- read.csv("TreeSummary.csv")
canopy_volumes <- read.csv("VolumeEstimates.csv") %>%
  dplyr::filter(species == "apple") %>%
  dplyr::select(tree, triangles)
tree_sum <- inner_join(tree_sum, canopy_volumes)
  
yield <- read.csv("AppleYield.csv", sep =',', head=T)

roots_yield <- read.csv("RootstockYieldMorph.csv", sep =',', head=T)

init()


### Analysis

# ANOVA ----
summary_rootstock <- c()
duncan_rootstock  <- c()

x = 2
for (y in yield[, 3:dim(yield)[2]]){
  x = x + 1
  model <- aov(y[tree_order] ~ as.factor(rootstocks))
  summary_rootstock[['yield']][[colnames(yield)[x]]] <- summary(model)
  test  <- duncan.test(model, "as.factor(rootstocks)")
  duncan_rootstock[['yield']][[colnames(yield)[x]]]  <- test$groups
}

yield_eff <- yield$cum_yield[tree_order] / (pi*(tree_sum$trunk_diam/20)^2)
model <- aov(yield_eff ~ as.factor(rootstocks))
summary_rootstock[['yield']][['yield_eff']] <- summary(model)
test  <- duncan.test(model, "as.factor(rootstocks)")
duncan_rootstock[['yield']][['yield_eff']]  <- test$groups

yield_area <- yield$cum_yield[tree_order] / (pi*((tree_sum$canopy_spread)/2)^2)
model <- aov(yield_area ~ as.factor(rootstocks))
summary_rootstock[['yield']][['yield_area']] <- summary(model)
test  <- duncan.test(model, "as.factor(rootstocks)")
duncan_rootstock[['yield']][['yield_area']]  <- test$groups


yield_path <- yield$cum_yield[tree_order] / tree_sum$max_path
model <- aov(yield_path ~ as.factor(rootstocks))
summary_rootstock[['yield']][['yield_path']] <- summary(model)
test  <- duncan.test(model, "as.factor(rootstocks)")
duncan_rootstock[['yield']][['yield_path']]  <- test$groups

yield_spread <- yield$cum_yield[tree_order] / (tree_sum$canopy_spread*100)
model <- aov(yield_spread ~ as.factor(rootstocks))
summary_rootstock[['yield']][['yield_spread']] <- summary(model)
test  <- duncan.test(model, "as.factor(rootstocks)")
duncan_rootstock[['yield']][['yield_spread']]  <- test$groups

yield_scars <- yield$cum_yield[tree_order] / tree_sum$tot_no_scars
model <- aov(yield_scars ~ as.factor(rootstocks))
summary_rootstock[['yield']][['yield_scars']] <- summary(model)
test  <- duncan.test(model, "as.factor(rootstocks)")
duncan_rootstock[['yield']][['yield_scars']]  <- test$groups

x = 2
for (morph in tree_sum[, 3:dim(tree_sum)[2]]){
  x = x + 1
  model <- aov(morph ~ as.factor(rootstock), data = tree_sum)
  summary_rootstock[['morph']][[colnames(tree_sum)[x]]] <- summary(model)
  test  <- duncan.test(model, "as.factor(rootstock)")
  duncan_rootstock[['morph']][[colnames(tree_sum)[x]]]  <- test$groups
}

x = 0
for (int in sma_res) {
  x = x + 1
  model <- aov(int[[1]][-roots_exc] ~ as.factor(rootstock), data = tree_sum)
  summary_rootstock[['int']][[relationships[x]]] <- summary(model)
  test  <- duncan.test(model, "as.factor(rootstock)")
  duncan_rootstock[['int']][[relationships[x]]]  <- test$groups
}

x = 0
for (exp in sma_res) {
  x = x + 1
  model <- aov(exp[[4]][-roots_exc] ~ as.factor(rootstock), data = tree_sum)
  summary_rootstock[['exp']][[relationships[x]]] <- summary(model)
  test  <- duncan.test(model, "as.factor(rootstock)")
  duncan_rootstock[['exp']][[relationships[x]]]  <- test$groups
}

x = 0
for (intr in sma_res) {
  x = x + 1
  ranges <- intr[[3]][-roots_exc] - intr[[2]][-roots_exc]
  model <- aov(ranges ~ as.factor(rootstock), data = tree_sum)
  summary_rootstock[['intr']][[relationships[x]]] <- summary(model)
  test  <- duncan.test(model, "as.factor(rootstock)")
  duncan_rootstock[['intr']][[relationships[x]]]  <- test$groups
}

x = 0
for (expr in sma_res) {
  x = x + 1
  ranges <- expr[[6]][-roots_exc] - expr[[5]][-roots_exc]
  model <- aov(ranges ~ as.factor(rootstock), data = tree_sum)
  summary_rootstock[['expr']][[relationships[x]]] <- summary(model)
  test  <- duncan.test(model, "as.factor(rootstock)")
  duncan_rootstock[['expr']][[relationships[x]]]  <- test$groups
}



# ANOVA Correlation ----
roots_morph <- arrange(summarize(group_by(tree_sum, rootstock),
                         avg_trunk_diam    = round(mean(trunk_diam), 3),
                         avg_height        = round(mean(height), 3),
                         avg_max_path      = round(mean(max_path), 3),
                         avg_stem_length   = round(mean(tot_length), 3),
                         avg_stem_area     = round(mean(tot_area), 3),
                         avg_stem_volume   = round(mean(tot_volume), 3),
                         avg_Mf            = round(mean(Mf), 3),
                         avg_canopy_spread = round(mean(canopy_spread),3),
                         avg_canopy_volume = round(mean(canopy_volume), 3),
                         avg_stem_m        = round(mean(tot_stem_m), 3),
                         avg_twig_m        = round(mean(tot_twig_m), 3),
                         avg_no_branches   = round(mean(tot_no_branch), 3),
                         avg_no_twigs      = round(mean(tot_no_twigs), 3),
                         avg_no_spurs      = round(mean(tot_no_spurs), 3),
                         avg_no_scars      = round(mean(tot_no_scars), 3)),
                       avg_trunk_diam)

sig_data <- mutate(inner_join(roots_yield, roots_morph),
                   expr_L_D_seg   = sma_res[[1]][[6]][roots_loc] - 
                                    sma_res[[1]][[5]][roots_loc],
                   expr_D_V_sub   = sma_res[[9]][[6]][roots_loc] - 
                                    sma_res[[9]][[5]][roots_loc],
                   expr_M_D_seg   = sma_res[[22]][[6]][roots_loc] - 
                                    sma_res[[22]][[5]][roots_loc],
                   expr_M_V_path  = sma_res[[25]][[6]][roots_loc] - 
                                    sma_res[[25]][[5]][roots_loc],
                   intr_L_D_seg   = sma_res[[1]][[3]][roots_loc] - 
                                    sma_res[[1]][[2]][roots_loc],
                   intr_M_D_seg   = sma_res[[22]][[3]][roots_loc] - 
                                    sma_res[[22]][[2]][roots_loc])
# write.csv(sig_data, "yield-morph.csv")

morph_yield <- c()
for (morph in sig_data[, 5:dim(sig_data)[2]]){
  test <- lm(sig_data$avg_cum_yield ~ morph)
  morph_yield <- append(morph_yield, round(summary.lm(test)$r.squared, 3))
} 

morph_yield_eff <- c()
for (morph in sig_data[, 5:dim(sig_data)[2]]){
  test <- lm(sig_data$avg_cum_yield / sig_data$avg_trunk_diam ~ morph)
  morph_yield_eff <- append(morph_yield_eff, round(summary.lm(test)$r.squared, 3))
}

morph_wgt <- c()
for (morph in sig_data[, 5:dim(sig_data)[2]]){
  test <- lm(sig_data$avg_fruit_wgt ~ morph)
  morph_wgt <- append(morph_wgt, round(summary.lm(test)$r.squared, 3))
} 

morph_fruit <- c()
for (morph in sig_data[, 5:dim(sig_data)[2]]){
  test <- lm(sig_data$avg_no_fruit ~ morph)
  morph_fruit <- append(morph_fruit, round(summary.lm(test)$r.squared, 3))
} 

morph_R2 <- data.frame(morph         = colnames(sig_data)[5:dim(sig_data)[2]],
                       cum_yield     = morph_yield,
                       yield_eff     = morph_yield_eff,
                       avg_fruit_wgt = morph_wgt,
                       avg_no_fruit  = morph_fruit)
#write.csv(morph_R2, 'morph-R2.csv')
                                                  
morph_R2_digest <- apply(morph_R2[, 2:5], c(1,2), mark_significance)
morph_R2_digest <- cbind(as.character(morph_R2[["morph"]]), 
                          morph_R2_digest)
#write.csv(morph_R2_digest, 'morph-R2-digest.csv')

multiple_summary <- list(
  summary(lm(avg_cum_yield ~ avg_max_path + expr_M_D_seg, data = sig_data)),
  summary(lm(avg_cum_yield ~ avg_max_path + intr_M_D_seg, data = sig_data)),
  summary(lm(avg_cum_yield ~ avg_max_path + expr_M_V_path, data = sig_data)),
  summary(lm(avg_cum_yield ~ avg_max_path + expr_M_V_path + 
             avg_max_path * expr_M_V_path, data = sig_data)),
  summary(lm(avg_cum_yield ~ avg_stem_m + expr_L_D_seg, data = sig_data)),
  summary(lm(avg_cum_yield ~ avg_stem_m + intr_L_D_seg, data = sig_data)),
  summary(lm(avg_cum_yield ~ avg_Mf + expr_L_D_seg, data = sig_data))
) 

multiple_R2 <- c()
for (summary in multiple_summary){
  multiple_R2 <- c(multiple_R2, round(summary$adj.r.squared, 3))
}

# Blast Correlation ----
exp_yield <- c()
for (i in 1:27){
  test <- lm(yield$cum_yield[tree_order]~sma_res[[i]][[4]][-roots_exc])
  exp_yield <- append(exp_yield, round(summary.lm(test)$r.squared, 3))
}  # Highest R2 == 0.224


root_exp_yield <- c()
for (i in 1:27){
  test <- lm(roots_yield$avg_cum_yield~sma_res[[i]][[4]][roots_loc])
  root_exp_yield <- append(root_exp_yield, round(summary.lm(test)$r.squared, 3))
} 

root_exp_wgt <- c()
for (i in 1:27){
  test <- lm(roots_yield$avg_fruit_wgt~sma_res[[i]][[4]][roots_loc])
  root_exp_wgt <- append(root_exp_wgt, round(summary.lm(test)$r.squared, 3))
}  

root_exp_fruit <- c()
for (i in 1:27){
  test <- lm(roots_yield$avg_no_fruit~sma_res[[i]][[4]][roots_loc])
  root_exp_fruit <- append(root_exp_fruit, round(summary.lm(test)$r.squared, 3))
}  

root_int_yield <- c()
for (i in 1:27){
  test <- lm(roots_yield$avg_cum_yield~sma_res[[i]][[1]][roots_loc])
  root_int_yield <- append(root_int_yield, round(summary.lm(test)$r.squared, 3))
} 

root_int_wgt <- c()
for (i in 1:27){
  test <- lm(roots_yield$avg_fruit_wgt~sma_res[[i]][[1]][roots_loc])
  root_int_wgt <- append(root_int_wgt, round(summary.lm(test)$r.squared, 3))
}  

root_int_fruit <- c()
for (i in 1:27){
  test <- lm(roots_yield$avg_no_fruit~sma_res[[i]][[1]][roots_loc])
  root_int_fruit <- append(root_int_fruit, round(summary.lm(test)$r.squared, 3))
}  

root_expr_yield <- c()
for (i in 1:27){
  range <- abs(sma_res[[i]][[6]][roots_loc] - sma_res[[i]][[5]][roots_loc])
  test <- lm(roots_yield$avg_cum_yield~range)
  root_expr_yield <- append(root_expr_yield, round(summary.lm(test)$r.squared, 7))
} 

root_expr_wgt <- c()
for (i in 1:27){
  range <- abs(sma_res[[i]][[6]][roots_loc] - sma_res[[i]][[5]][roots_loc])
  test <- lm(roots_yield$avg_fruit_wgt~range)
  root_expr_wgt <- append(root_expr_wgt, round(summary.lm(test)$r.squared, 3))
}  

root_expr_fruit <- c()
for (i in 1:27){
  range <- abs(sma_res[[i]][[6]][roots_loc] - sma_res[[i]][[5]][roots_loc])
  test <- lm(roots_yield$avg_no_fruit~range)
  root_expr_fruit <- append(root_expr_fruit, round(summary.lm(test)$r.squared, 3))
}  

root_intr_yield <- c()
for (i in 1:27){
  range <- abs(sma_res[[i]][[3]][roots_loc] - sma_res[[i]][[2]][roots_loc])
  test <- lm(roots_yield$avg_cum_yield~range)
  root_intr_yield <- append(root_intr_yield, round(summary.lm(test)$r.squared, 3))
} 

root_intr_wgt <- c()
for (i in 1:27){
  range <- abs(sma_res[[i]][[3]][roots_loc] - sma_res[[i]][[2]][roots_loc])
  test <- lm(roots_yield$avg_fruit_wgt~range)
  root_intr_wgt <- append(root_intr_wgt, round(summary.lm(test)$r.squared, 3))
}  

root_intr_fruit <- c()
for (i in 1:27){
  range <- abs(sma_res[[i]][[3]][roots_loc] - sma_res[[i]][[2]][roots_loc])
  test <- lm(roots_yield$avg_no_fruit~range)
  root_intr_fruit <- append(root_intr_fruit, round(summary.lm(test)$r.squared, 3))
} 

roots_exp <- data.frame(
               relationship = relationships,
               exp_yield    = root_exp_yield,
               int_yield    = root_int_yield,
               expr_yield   = root_expr_yield,
               intr_yield   = root_intr_yield,
               exp_wgt      = root_exp_wgt,
               int_wgt      = root_int_wgt,
               expr_wgt     = root_expr_wgt,
               intr_wgt     = root_intr_wgt,
               exp_fruit    = root_exp_fruit,
               int_fruit    = root_int_fruit,     
               expr_fruit   = root_expr_fruit,
               intr_fruit   = root_intr_fruit)

#write.csv(roots_exp, 'exp-R2.csv')

roots_exp_digest <- apply(roots_exp[, 2:13], c(1,2), mark_significance)
roots_exp_digest <- cbind(as.character(roots_exp[["relationship"]]), 
                          roots_exp_digest)

#write.csv(roots_exp_digest, 'exp-R2-digest.csv')