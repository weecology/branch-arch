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

exponent_R2 <- function(col_list, n, k, m, xlabel){
  if (length(col_list) == 2){
    min_range <- c(results[[col_list[1]]][[2]][k:m], results[[col_list[2]]][[2]][k:m], flow[n], elastic[n])
    max_range <- c(results[[col_list[1]]][[3]][k:m], results[[col_list[2]]][[3]][k:m], flow[n], elastic[n])}
  else{
    min_range <- c(results[[col_list[1]]][[2]][k:m], results[[col_list[2]]][[2]][k:m], 
                   results[[col_list[3]]][[2]][k:m], flow[n], elastic[n])
    max_range <- c(results[[col_list[1]]][[3]][k:m], results[[col_list[2]]][[3]][k:m], 
                   results[[col_list[3]]][[3]][k:m], flow[n], elastic[n])}
  plot(range(0,1), range(min(min_range, na.rm=T), max(max_range, na.rm=T)), 
       ylab = ylabels[n], xlab=xlabel, type = 'n',
       ylim = c(min(min_range, na.rm=T), max(max_range, na.rm=T)))
  for (i in 1:length(col_list)){
    for(j in k:m){
      points(results[[col_list[i]]][[4]][j], results[[col_list[i]]][[1]][j], pch = as.numeric(points[[i]][j]), 
             bg = 'grey', cex = 2, lwd = 2.5)
      arrows(x0=results[[col_list[i]]][[4]][j], y0=results[[col_list[i]]][[2]][j], y1=results[[col_list[i]]][[3]][j], 
             code=3, angle=90, lwd=1.7, length=.08)
    }
  }
  abline(h = flow[n], lwd = 2, lty = 6)
  abline(h = elastic[n], lwd = 2, lty = 2)
}

ylabels <- c("L~D", "SA~V", "D~V", "L~V", "D~SA", "L~SA", "L~M", "M~D", "M~V")
flow <- c(2, .75, .25, .5, .33, .67, NA, NA, NA)
elastic <- c(.67, .625, .375, .25, .6, .4, .25, 2.67, NA)
points <- list(list(17, 24, 2, 17, 24, 2),
               list(18, 23, 5, 18, 23, 5),
               list(15, 22, 0, 15, 22, 0))

pdf(file="ExponentR2.pdf", width= 16, height=12,family="Helvetica", pointsize=14)

par(mfrow= c(3,4), mar = c(4,5,1,1), cex.lab=1.5)
exponent_R2(c(1:3), 1, 1, 3, '')
exponent_R2(c(4:5), 2, 1, 3, '')
exponent_R2(c(6:7), 3, 1, 3, '')
plot(range(0,1), range(0,1), main = 'Tree Level', bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
exponent_R2(c(8:10), 4, 1, 3, '')
exponent_R2(c(11:12), 5, 1, 3, '')
exponent_R2(c(13:15), 6, 1, 3, '')
plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
par(xpd=T)
legend('left', legend=c("", "", "", "", "", "", "All", "Cherry", "Apple"), 
       pch= c(17, 24, 2, 18, 23, 5, 15, 22, 0), pt.bg = 'grey', cex = 1.2, 
       bty = 'n', title = "Segment      Path      Subtree", title.adj = -.35, ncol = 3)
par(xpd=F)
exponent_R2(c(16:18), 7, 1, 3, '')
exponent_R2(c(19:20), 8, 1, 3, 'R2')
exponent_R2(c(21:22), 9, 1, 3, '')

par(mfrow= c(3,4), mar = c(4,5,1,1), cex.lab=1.5)
exponent_R2(c(1:3), 1, 4, 6, '')
exponent_R2(c(4:5), 2, 4, 6, '')
exponent_R2(c(6:7), 3, 4, 6, '')
plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
exponent_R2(c(8:10), 4, 4, 6, '')
exponent_R2(c(11:12), 5, 4, 6, '')
exponent_R2(c(13:15), 6, 4, 6, '')
plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
par(xpd=T)
legend('left', legend=c("", "", "", "", "", "", "All", "Cherry", "Apple"), 
       pch= c(17, 24, 2, 18, 23, 5, 15, 22, 0), pt.bg = 'grey', cex = 1.2, 
       bty = 'n', title = "Segment      Path      Subtree", title.adj = -.35, ncol = 3)
par(xpd=F)
exponent_R2(c(16:18), 7, 4, 6, '')
exponent_R2(c(19:20), 8, 4, 6, 'R2')
exponent_R2(c(21:22), 9, 4, 6, '')

dev.off()

