###This script builds the figures used in the Chapter 1 publication.###

###initiate exponent results
sma <- read.csv('SMAResults.csv', sep=',', head=T)

levels <- c(1,2,3,4,5,12)
results <- list()
for (i in 1:23){ #relationship columns
  results[[i]] <- list()
  for (j in 1:4){
    results[[i]][[j]] <- vector(length = 6)
    # 1: exponent, 2: CI-, 3: CI+, 4: R2 
    for (k in 1:6) { # group/ind rows
      results[[i]][[j]][k] = as.numeric(strsplit(as.character(sma[(levels[k]+1),(i+2)]), " ")[[1]][(2*j-1)])
    }
  }
}

###Subtree improves R2

exponent_R2 <- function(col_list, n, xlabel){
  if (length(col_list == 2)){
    min_range <- c(results[[col_list[1]]][[2]], results[[col_list[2]]][[2]])
    max_range <- c(results[[col_list[1]]][[3]], results[[col_list[2]]][[3]])}
  else{
    min_range <- c(results[[col_list[1]]][[2]], results[[col_list[2]]][[2]], results[[col_list[3]]][[2]])
    max_range <- c(results[[col_list[1]]][[3]], results[[col_list[2]]][[3]], results[[col_list[3]]][[3]])}
  plot(range(0,1), range(min(min_range, na.rm=T), max(max_range, na.rm=T)), 
       ylab = ylabels[n], xlab=xlabel, type = 'n',
       ylim = c(min(min_range, na.rm=T), max(max_range, na.rm=T)))
  for (i in 1:length(col_list)){
    for(j in 1:6){
      points(results[[col_list[i]]][[4]][j], results[[col_list[i]]][[1]][j], pch = as.numeric(points[[i]][j]), 
             bg = 'grey', cex = 2, lwd = 2)
      arrows(x0=results[[col_list[i]]][[4]][j], y0=results[[col_list[i]]][[2]][j], y1=results[[col_list[i]]][[3]][j], 
             code=3, angle=90, lwd=1.7, length=.08)
    }
  }
  abline(h = flow[n], lwd = 2, lty = 6)
  abline(h = elastic[n], lwd = 2, lty = 2)
}

ylabels <- c("L~D", "SA~V", "D~V", "L~V", "D~SA", "L~SA", "L~M", "M~D", "M~V")
flow <- c(2, .75, .25, .5, .33, .67, 10, 15, 10)
elastic <- c(.67, .625, .375, .25, .6, .4, .25, 2.67, 10)
points <- list(list(19, 21, 1, 17, 24, 2),
               list(9, 7, 12, 18, 23, 5),
               list(14, 25, 6, 15, 22, 0))

pdf(file="ExponentFigures.pdf", width= 16, height=12,family="Helvetica", pointsize=14)
par(mfrow= c(3,4), mar = c(4,5,1,1), cex.lab=1.5)
exponent_R2(c(1:3), 1, '')
exponent_R2(c(4:5), 2, '')
exponent_R2(c(6:7), 3, '')
plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
exponent_R2(c(8:10), 4, '')
exponent_R2(c(11:12), 5, '')
exponent_R2(c(13:15), 6, '')
plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
par(xpd=T)
legend('left', legend=c("", "", "", "", "", "", "", "", "", "", "", "",
                          "Tree-", "Cherry", "Apple", 
                          "Branch-", "Cherry", "Apple"), 
       pch= c(19, 21, 1, 17, 24, 2, 9, 7, 12, 18, 23, 5, 14, 25, 6, 15, 22, 0), pt.bg = 'grey', cex = 1.2, 
       bty = 'n', title = "Segment        Path        Subtree", title.adj = -.35, ncol = 3)
par(xpd=F)
exponent_R2(c(16:18), 7, '')
exponent_R2(c(19:20), 8, 'R2')
exponent_R2(c(21:22), 9, '')
dev.off()

