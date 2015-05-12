# This script calculates slope differences from the SMAResults.csv.

###To Do
# Make figure with just rootstock level

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
  return(input_data)
}

calc_m_diffs <- function(relationship, focal_group, i){
  # calculates differences in scaling parameters from focal_group
  m_diff_all = -1 * (sma_res[[relationship]][[1]][i] -
               sma_res[[relationship]][[1]][focal_group])
  
  m_diff_CId = -1 * (sma_res[[relationship]][[2]][i] - 
               sma_res[[relationship]][[1]][focal_group])

  m_diff_CIu = -1 * (sma_res[[relationship]][[3]][i] - 
               sma_res[[relationship]][[1]][focal_group])

  return(c(m_diff_all, m_diff_CId, m_diff_CIu,
           sma_res[[relationship]][[4]][i]))
}

calc_m_diffs_std <- function(relationship, focal_group, i){
  # calculates differences in scaling parameters from focal group 
  # standardized by focal group CI
  all_branch_CI = sma_res[[relationship]][[3]][focal_group] - 
                  sma_res[[relationship]][[2]][focal_group]
  
  m_diff_all    = -1 * (sma_res[[relationship]][[1]][i] -
                  sma_res[[relationship]][[1]][focal_group]) / 
                  all_branch_CI
  
  m_diff_CId    = -1 * (sma_res[[relationship]][[2]][i] - 
                  sma_res[[relationship]][[1]][focal_group]) / 
                  all_branch_CI
  
  m_diff_CIu    = -1 * (sma_res[[relationship]][[3]][i] - 
                  sma_res[[relationship]][[1]][focal_group]) / 
                  all_branch_CI
  
  return(c(m_diff_all, m_diff_CId, m_diff_CIu,
         sma_res[[relationship]][[4]][i]))  # R2
}

get_m_diffs <- function(relationship, focal_group = 2, standardized = F){
  # compiles scaling parameter diffs into output file
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

poly_set <- function(results, x_set, n){
  # sets the grey shading regions for the plot function
  polygon(x_set, 
          c(min(results[[n]], na.rm=T)-1, max(results[[n]], na.rm=T)+1, 
            max(results[[n]], na.rm=T)+1, min(results[[n]], na.rm=T)-1), 
          col = rgb(.80,.80,.80,0.5), border = NA)
}

gen_plot <- function(results, n){
  # general plot for scaling results by rootstock and individual
  plot(range(2,32), range(min(results[[n]][, 1:3], na.rm=T), max(results[[n]][, 1:3], na.rm=T)), 
       ylab = relationships[n], xlab="", xaxt = 'n', type = 'n',
       ylim = c(min(results[[n]][, 1:3], na.rm=T), max(results[[n]][, 1:3], na.rm=T)))
  poly_set(results, c(2.5, 2.5, 8.5, 8.5), n)
  poly_set(results, c(14.5, 14.5, 20.5, 20.5), n) 
  poly_set(results, c(22.5, 22.5, 27.5, 27.5), n) 
  points(results[[n]][, 1], pch = 19, cex = 2, bg = "black")
  #points(results[[n]][, 2], pch = 25, cex = 2, bg = "black")
  #points(results[[n]][, 3], pch = 16, cex = 4)
  for(s in 1:32){
    arrows(y0=results[[n]][, 2][s], y1=results[[n]][, 3][s], x0=s, x1=s, code=3, angle=90, lwd=1.7, length=.08)  
  }
  abline(h = 0, lwd = 2, lty = 1)
  #abline(h = elastic[n], lwd = 2, lty = 2)
}

multi_plot <- function(results, col_list, location = "topright"){
  # builds panel of general plots for branch-levels by scaling relationship
  par(mfrow = c(length(col_list),1), oma = c(9,0,0,0), mar = c(1,6,1,2), cex.lab = 2, bty = 'o')
  gen_plot(results, col_list[1])
  #par(xpd=T)
  #legend(location, bty = 'n', horiz = T, cex = 2, pt.cex = 2, pt.bg = 'black',
  #       legend = c("Flow",  "Elastic", "All-apples"),  
  #       pch    = c(24, 25, 16))  
  #par(xpd=F)
  for (i in col_list[-1]){ gen_plot(results, i) }
  axis(1, 1:32, group_names, las = 2, cex.axis = 1.5)
}

gen_plot_roots <- function(results, n, relationship, rootstocks = roots_list, 
                           axis = FALSE){
  # general plot for scaling results by rootstock and individual
  plot(range(0,9), range(min(results[[relationship]][, 1:3], na.rm=T), 
                          max(results[[relationship]][, 1:3], na.rm=T)), 
       ylab = relationships_abv[n], xlab="", xaxt = 'n', type = 'n',
       ylim = c(min(results[[relationship]][, 1:3], na.rm=T), 
                max(results[[relationship]][, 1:3], na.rm=T))
       )
  for(s in 1:8){
    points(s, results[[relationship]][, 1][as.numeric(rootstocks[[s]][1])], 
           pch = 19, cex = 2, bg = "black")
    arrows(y0=results[[relationship]][, 2][as.numeric(rootstocks[[s]][1])], 
           y1=results[[relationship]][, 3][as.numeric(rootstocks[[s]][1])], 
           x0=s, x1=s, code=3, angle=90, lwd=1.7, length=.08)  
  }
  if (axis == TRUE){
    axis(1, 1:8, names_abv, las = 2, cex.axis = 1.5)
  } else {
    axis(1, 1:8, labels = F, las = 2, cex.axis = 1.5)
  }
  
  abline(h = 0, lwd = 2, lty = 1)
}

multi_plot_roots <- function(results){
  # generates an panel of exponent vs rootstock
  par(mfrow= c(3,3), mar = c(4,5,1,1),  oma = c(9,0,0,0), 
      cex.lab = 2.7, cex.axis = 2)
  gen_plot_roots(results, 1, 3) 
  gen_plot_roots(results, 2, 6)
  gen_plot_roots(results, 3, 9)
  gen_plot_roots(results, 4, 12)
  gen_plot_roots(results, 5, 15)
  gen_plot_roots(results, 6, 18)
  gen_plot_roots(results, 7, 21, axis = T)
  gen_plot_roots(results, 8, 23, axis = T)
  gen_plot_roots(results, 9, 26, axis = T)
}

exponent_R2 <- function(results, n, relationship, rootstocks = roots_list, xlabel = ''){
  # generates an exponent by R2 plot at rootstock level
  plot(range(0.4,1), range(min(results[[relationship]][, 1], na.rm=T), 
                           max(results[[relationship]][, 1], na.rm=T)), 
       ylab = relationships_abv[n], xlab=xlabel, type = 'n',
       ylim = c(min(results[[relationship]][, 1], na.rm=T), 
                max(results[[relationship]][, 1], na.rm=T)))
  for(root in rootstocks){
    if (is.character(root[1])){
      points(results[[relationship]][, 4][as.numeric(root[1])], 
             results[[relationship]][, 1][as.numeric(root[1])], 
             cex = 5, pch = 23, bg = root[2],  lwd = 4)
    } else {
      points(results[[relationship]][, 4][root[1]], 
             results[[relationship]][, 1][root[1]], 
             cex = root[2], pch = 21, bg = 'grey',  lwd = 4)
    }
  }
}

multi_exp_R2 <- function(results){
  # generates an panel of exponent vs R2 plots
  par(mfrow= c(3,3), mar = c(4,5,1,1), cex.lab = 2.7, cex.axis = 2)
  exponent_R2(results, 1, 3) 
  exponent_R2(results, 2, 6)
  exponent_R2(results, 3, 9)
  exponent_R2(results, 4, 12)
  exponent_R2(results, 5, 15)
  exponent_R2(results, 6, 18)
  exponent_R2(results, 7, 21)
  par(xpd=T)
  legend('bottomleft', bty = 'n', cex = 2.2, pt.lwd = 4,
         legend = names_abv, 
         pch = c(23, 23, 21, 21, 21, 21, 21, 21), 
         pt.bg = c('black', 'grey', 'grey', 'grey', 'grey', 'grey', 'grey', 'grey'),
         pt.cex = c(4, 4, 2, 2.5, 3, 3.5, 4, 4.5)
  )
  par(xpd=F)
  exponent_R2(results, 8, 23, xlabel = 'R2')
  exponent_R2(results, 9, 26)
}

init <- function(){
  # initiates variable lists
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
  
  relationships_abv <<- c("L~D", "SA~V", "D~V", 
                          "L~V", "D~SA", "L~SA", 
                          "L~M", "M~D", "M~V")
  
  group_names <<- c("all-tree", "all-branch",
                    "Bud.9", "Bud.9-1", "Bud.9-2", "Bud.9-3", "Bud.9-4", "Bud.9-4+",
                    "CG.3041", "CG.3041-1", "CG.3041-1+", "CG.3041-2", "CG.3041-3", "CG.3041-4", 
                    "CG.6210", "CG.6210-1", "CG.6210-2", "CG.6210-3", "CG.6210-3+", "CG.6210-4", 
                    "M.26", "M.26+",
                    "JM.8", "JM.8-1", "JM.8-2", "JM.8-2+", "JM.8-3",
                    "PiAu.5683", "PiAu.5683-1", "PiAu.5683-2", "PiAu.5683-3", "PiAu.5683-3+")
  
  names_abv <<- c("All-tree", "All-branch", 
                  "Bud.9", "CG.3041","CG.6210","M.26", "JM.8", "PiAu.5683")
  
  roots_list <<- list(c(1, 'black'),
                      c(2, 'grey'),
                      c(28, 4.5),
                      c(23, 4),
                      c(21, 3.5),
                      c(15, 3),
                      c(9, 2.5),
                      c(3, 2))
  
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
}

# Execute ----

sma_res <- get_data()

init()

m_diffs <- list()
for (i in 1:length(sma_res)){
  m_diffs[[i]] = get_m_diffs(i)
}

m_diffs_std <- list()
for (i in 1:length(sma_res)){
  m_diffs_std[[i]] = get_m_diffs(i, standardized = T)
}

# Visualize ----

pdf(file="m_diffFigures_all.pdf", width= 10, height=10,family="Helvetica", pointsize=12)
multi_plot(m_diffs, c(1:3))
multi_plot(m_diffs, c(4:6))
multi_plot(m_diffs, c(7:9))
multi_plot(m_diffs, c(10:12))
multi_plot(m_diffs, c(13:15))
multi_plot(m_diffs, c(16:18))
multi_plot(m_diffs, c(19:21))
multi_plot(m_diffs, c(22:23))
multi_plot(m_diffs, c(24:26))
multi_plot(m_diffs, c(27))
dev.off()


pdf(file="m_diff_stdFigures_all.pdf", width= 10, height=10,family="Helvetica", pointsize=12)
multi_plot(m_diffs_std, c(1:3))
multi_plot(m_diffs_std, c(4:6))
multi_plot(m_diffs_std, c(7:9))
multi_plot(m_diffs_std, c(10:12))
multi_plot(m_diffs_std, c(13:15))
multi_plot(m_diffs_std, c(16:18))
multi_plot(m_diffs_std, c(19:21))
multi_plot(m_diffs_std, c(22:23), 'bottom')
multi_plot(m_diffs_std, c(24:26))
multi_plot(m_diffs_std, c(27))
dev.off()

pdf(file="m_diffFigures.pdf", width= 16, height=12,family="Helvetica", pointsize=14)
multi_plot_roots(m_diffs) 
dev.off()

pdf(file="m_diffstdFigures.pdf", width= 16, height=12,family="Helvetica", pointsize=14)
multi_plot_roots(m_diffs_std) 
dev.off()

pdf(file="m_diffsExpR2.pdf", width= 16, height=12,family="Helvetica", pointsize=14)
multi_exp_R2(m_diffs)
dev.off()

pdf(file="m_diffs_stdExpR2.pdf", width= 16, height=12,family="Helvetica", pointsize=14)
multi_exp_R2(m_diffs_std)
dev.off()