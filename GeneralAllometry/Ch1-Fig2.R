### Modifies Chapter1Figs.R to remove path scale and "both" group.

sma <- read.csv("SMAResults.csv", sep=",", head=T)

levels <- c(1,2,3,4,5,12)
results <- list()
for (i in 1:26){ #relationship columns
  results[[i]] <- list()
  for (j in 1:4){
    results[[i]][[j]] <- vector(length = 6)
    # 1: exponent, 2: CI-, 3: CI+, 4: R2 
    for (k in 1:6) { # group/ind rows
      results[[i]][[j]][k] = as.numeric(strsplit(as.character(sma[(levels[k]+1),(i+2)]), " ")[[1]][(2*j-1)])
    }
  }
}

exponent_R2 <- function(col_list, n, k=5, m=6, xlabel="R2"){
  
  if (length(col_list) == 2){
    min_range <- c(results[[col_list[1]]][[2]][k:m], results[[col_list[2]]][[2]][k:m], flow[n], elastic[n])
    max_range <- c(results[[col_list[1]]][[3]][k:m], results[[col_list[2]]][[3]][k:m], flow[n], elastic[n])}
  else{
    min_range <- c(results[[col_list[1]]][[2]][k:m], results[[col_list[2]]][[2]][k:m], 
                   results[[col_list[3]]][[2]][k:m], flow[n], elastic[n])
    max_range <- c(results[[col_list[1]]][[3]][k:m], results[[col_list[2]]][[3]][k:m], 
                   results[[col_list[3]]][[3]][k:m], flow[n], elastic[n])}
  
  plot(range(0,1), range(min(min_range, na.rm=T), max(max_range, na.rm=T)), 
       ylab = ylabels[n], xlab=xlabel, type = "n",
       ylim = c(min(min_range, na.rm=T), max(max_range, na.rm=T)))
  legend("topleft", LETTERS[n], bty="n", cex=2, x.intersp=0)
   
  for (i in 1:length(col_list)){
    for(j in k:m){
      points(results[[col_list[i]]][[4]][j], results[[col_list[i]]][[1]][j], pch = as.numeric(di_points[[i]][j]), 
             bg = "grey", cex=3.5, lwd=2.5)
      arrows(x0=results[[col_list[i]]][[4]][j], y0=results[[col_list[i]]][[2]][j], y1=results[[col_list[i]]][[3]][j], 
             code=3, angle=90, lwd=1.7, length=.08)
    }
  } 
  abline(h = flow[n], lwd = 2, lty = 6)
  abline(h = elastic[n], lwd = 2, lty = 2)
}

ylabels <- c("Length ~ Diameter", "Area ~ Volume", "Diamter ~ Volume", 
             "Length ~ Volume", "Diameter ~ Area", "Length ~ Area", 
             "Length ~ Mass", "Mass ~ Diameter", "Mass ~ Volume")
flow <- c(2, .75, .25, .5, .33, .67, NA, 2.67, NA)
elastic <- c(.67, .625, .375, .25, .6, .4, .25, 2.53, NA)
di_points <- list(list(17, 24, 2, 17, 24, 2),
                  list(15, 22, 0, 15, 22, 0))


pdf(file="Fig2.pdf", width= 16, height=12,family="Helvetica", pointsize=18)

par(mfrow= c(3,4), mar = c(4,5,1,1), cex.lab=1.75, cex.axis=1.35)
exponent_R2(c(1,3), 1)
exponent_R2(c(4,6), 2)
exponent_R2(c(7,9), 3)
plot(range(0,1), range(0,1), bty="n", main ="", xaxt="n", yaxt="n", xlab="", ylab="", type ="n")
exponent_R2(c(10,12), 4)
exponent_R2(c(13,15), 5)
exponent_R2(c(16,18), 6)
plot(range(0,1), range(0,1), bty="n", xaxt="n", yaxt="n", xlab="", ylab="", type ="n")
par(xpd=T)
legend("left", legend=c("", "", "Cherry", "Apple"), 
       pch=c(24, 2, 22, 0), pt.bg="grey", cex=1.4, pt.cex=2, 
       bty="n", title="Segment  Subtree", title.adj=-.35, ncol=2)
legend("bottomleft", legend=c("Elastic Similarity","Flow Similarity"), lty=c(2,6), lwd = 1.7, 
       bty = "n", cex=1.4)
par(xpd=F)
exponent_R2(c(19,21), 7)
exponent_R2(c(22,23), 8)
exponent_R2(c(24,26), 9)

dev.off()