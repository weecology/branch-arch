# This script analysis and generates figures for the Yield ~ m_diff relationship

# Functions ---

get_data <- function(){
  # Compiles data for computation from the visually oriented SMAResults file
  
  sma <- read.csv('SMAResults.csv', sep=',', head=T) 
  
  input_data <- list()
  for (i in 1:27){                              # Scaling relationships
    input_data[[i]] <- list()
    for (j in 1:4){                             # Results output
      #   1.exponent, 2.CI-, 3.CI+, 4.R2 
      input_data[[i]][[j]] <- vector(length = 32)
      for (k in 1:32) {                         # Groups and individuals
        input_data[[i]][[j]][k] = as.numeric(strsplit(
          as.character(sma[(k+1),(i+2)]), " ")[[1]][(2*j-1)])
      }
    }
  }
  return(input_data)
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
  
  tree_order   <<- c(2,7,12,3,
                     5,11,6,8,
                     10,1,4,9,
                     13,
                     16,15,17,  # tree_ids - 1 after 16
                     19,18,14)
  
  roots_loc    <<- c(3, 9, 15, 23, 28)
  
  roots_exc    <<- c(1, 2, 3, 8, 9, 11, 15, 19, 22, 23, 26, 28, 32)
  
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

# Execute ----
sma_res <- get_data()

yield <- read.csv("AppleYield.csv", sep =',', head=T)

roots_yield <- read.csv("RootstockYield.csv", sep =',', head=T)

init()

# Analysis ----
exp_yield <- c()
for (i in 1:27){
  test <- lm(yield$cum_yield[tree_order]~sma_res[[i]][[1]][-roots_exc])
  exp_yield <- append(exp_yield, round(summary.lm(test)$r.squared, 3))
}  # Highest R2 == 0.205

root_exp_yield <- c()
for (i in 1:27){
  test <- lm(roots_yield$avg_cum_yield~sma_res[[i]][[1]][roots_loc])
  root_exp_yield <- append(root_exp_yield, round(summary.lm(test)$r.squared, 3))
}  # Highest R2 = .470; R2 > 0.4 in c(3*, 6, 15, 26)

root_exp_wgt <- c()
for (i in 1:27){
  test <- lm(roots_yield$avg_fruit_wgt~sma_res[[i]][[1]][roots_loc])
  root_exp_wgt <- append(root_exp_wgt, round(summary.lm(test)$r.squared, 3))
}  # Highest R2 = .733; R2 > 0.6 in c(2, 3, 7, 8, 9*, 13, 14, 15, 23)  

root_exp_fruit <- c()
for (i in 1:27){
  test <- lm(log10(roots_yield$avg_no_fruit)~sma_res[[i]][[1]][roots_loc])
  root_exp_fruit <- append(root_exp_fruit, round(summary.lm(test)$r.squared, 3))
}  # Highest R2 = .87: R2 > 0.6 in c(18, 22*, 23, 27)

# Output ----
roots_exp <- data.frame(
               relationship = relationships,
               exp_yield = root_exp_yield,
               exp_wgt = root_exp_wgt,
               exp_fruit = root_exp_fruit)
