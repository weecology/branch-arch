# This script calculates slope differences from the SMAResults.csv.

# Functions ----

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
  
  ylabels  <- c("L~D (Segment)", "L~D (Path)", "L~D (Subtree)", 
                "SA~V (Segment)", "SA~V (Path)", "SA~V (Subtree)", 
                "D~V (Segment)", "D~V (Path)", "D~V (Subtree)", 
                "L~V (Segment)", "L~V (Path)", "L~V (Subtree)", 
                "D~SA (Segment)", "D~SA (Path)", "D~SA (Subtree)", 
                "L~SA (Segment)", "L~SA (Path)", "L~SA (Subtree)", 
                "L~M (Segment)", "L~M (Path)", "L~M (Subtree)", 
                "M~D (Segment)", "M~D (Subtree)", 
                "M~V (Segment)", "M~V (Path)", "M~V (Subtree)", 
                "D/P Ratio ~ P Diam")
  
  flow    <<- c(2, 2, 2, 
                .75, .75, .75, 
                .25, .25, .25, 
                .5, .5, .5, 
                .33, .33, .33, 
                .67, .67, .67)
  
  elastic <<- c(.67, .67, .67, 
                .625, .625, .625, 
                .375, .375, .375, 
                .25, .25, .25, 
                .6, .6, .6, 
                .4, .4, .4, 
                .25, .25, .25, 
                2.67, 2.67)
  
  return(input_data)
}

calc_m_diffs <- function(relationship, focal_group, i){
  m_diff_flow = sma_res[[relationship]][[1]][i] - flow[relationship]
  m_diff_elastic = sma_res[[relationship]][[1]][i] - elastic[relationship]
  m_diff_relationship = sma_res[[relationship]][[1]][i] - 
                 sma_res[[relationship]][[1]][focal_group]
  return(c(m_diff_flow, m_diff_elastic, m_diff_relationship))
}

calc_m_diffs_std <- function(relationship, focal_group, i){
  m_diff_flow = (sma_res[[relationship]][[1]][i] - flow[relationship]) / 
    (sma_res[[relationship]][[3]][i] - sma_res[[relationship]][[2]][i])
  m_diff_elastic = (sma_res[[relationship]][[1]][i] - elastic[relationship]) / 
    (sma_res[[relationship]][[3]][i] - sma_res[[relationship]][[2]][i])
  m_diff_relationship = (sma_res[[relationship]][[1]][i] - 
    sma_res[[relationship]][[1]][focal_group]) / 
    (sma_res[[relationship]][[3]][i] - sma_res[[relationship]][[2]][i])
  return(c(m_diff_flow, m_diff_elastic, m_diff_relationship))
}

get_m_diffs <- function(relationship, focal_group = 2, standardized = F){
  if (standardized == F){
    for (i in 1:length(sma_res[[relationship]][[1]])){
      if(exists('output')){
        output = rbind(output, calc_m_diffs(relationship, focal_group, i))
      } else {
        output = calc_m_diffs(relationship, focal_group, i)
      }
    }
    return(output)
  } else {
    for (i in 1:length(sma_res[[relationship]][[1]])){
      if(exists('output')){
        output = rbind(output, calc_m_diffs_std(relationship, focal_group, i))
      } else {
        output = calc_m_diffs_std(relationship, focal_group, i)
      }
    }
    return(output)
  }
}




# Execute ----

sma_res <- get_data()

m_diffs <- list()
for (i in 1:length(sma_res)){
  m_diffs[[i]] = get_m_diffs(i)
}

m_diffs_std <- list()
for (i in 1:length(sma_res)){
  m_diffs_std[[i]] = get_m_diffs(i, standardized = T)
}


