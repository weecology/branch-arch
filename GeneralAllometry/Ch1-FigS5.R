###This script builds the exponent covariation figures###

library('smatr')

sma <- read.csv('SMAResults.csv', sep=',', head=T)

inds <- c(7:9, 11, 12, 15:18, 21, 23:25, 27:29, 31, 32, 35, 36, 38, 40:42)  # row count from SMAResults.csv
results <- list()
for (i in 1:26){ #relationship columns
  results[[i]] <- list()
  for (j in 1:4){
    results[[i]][[j]] <- vector(length = 6)
    # 1: exponent, 2: CI-, 3: CI+, 4: R2 
    for (k in 1:length(inds)) { # group/ind rows
      results[[i]][[j]][k] = as.numeric(strsplit(as.character(sma[(inds[k]),(i+2)]), " ")[[1]][(2*j-1)])
    }
  }
}

exp_exp <- function(x, y, k, m, point, letter=NULL){
  
  test <- lm(results[[y]][[1]]~results[[x]][[1]] + I(results[[x]][[1]]^2))
  
  plot(range(min(results[[x]][[1]], na.rm=T)-0.1, 
             max(results[[x]][[1]], na.rm=T))+0.1,
       range(min(results[[y]][[1]], na.rm=T)-0.1, 
             max(results[[y]][[1]], na.rm=T))+0.1, 
       xlab=relationships[k], ylab=relationships[m], type = 'n',
       cex.lab=1.7, cex.axis=1.5)
  
  points(results[[x]][[1]][1:5], results[[y]][[1]][1:5], pch = point[1], 
         bg = 'grey', cex = 2.5, lwd = 2.5)
  points(results[[x]][[1]][6:24], results[[y]][[1]][6:24], pch = point[2],
         bg = 'grey', cex = 2.5, lwd = 2.5)
  
  if (summary(test)$r.squared > 0.33){
    curve(test$coef[3]*x^2 + test$coef[2]*x + test$coef[1],
          min(results[[x]][[1]], na.rm=T)-0.1, max(results[[x]][[1]], na.rm=T)+0.1,
          lwd=4, lty=2, add=T)
    r2_exp <- paste("R2 = ", round(summary(test)$r.squared,3),"     ", sep="")
    legend("topright", legend=as.expression(r2_exp), cex=1.7, bty="n")
  } else {
    legend("topright", legend="NS     ", cex=1.7, bty="n")
  }
  
  if(length(letter)>0){
    mtext(letter, adj=0, line=1, cex=1.3)
  }
}

relationships <- c("Length ~ Diameter", "Area ~ Volume", "Diamter ~ Volume", 
                   "Length ~ Volume", "Diameter ~ Area", "Length ~ Area",
                   "Length ~ Mass", "Mass ~ Diameter", "Mass ~ Volume")

relationship_ref <- c("L~D (Segment)", "(Path)", "(Subtree)", 
                      "SA~V (Segment)", "(Path)", "(Subtree)", 
                      "D~V(Segment)", "(Path)", "(Subtree)", 
                      "L~V (Segment)", "(Path)", "(Subtree)", 
                      "D~SA (Segment)", "(Path)", "(Subtree)", 
                      "L~SA (Segment)", "(Path)", "(Subtree)", 
                      "L~M (Segment)", "(Path)", "(Subtree)", 
                      "M~D (Segment)", "(Subtree)",
                      "M~V(Segment)", "(Path)", "(Subtree)", 
                      "D/P Ratio ~ P Diam")

points <- list(c(24, 2), c(22, 0)) # cherry, apple


pdf(file="FigS5.pdf", width=12, height=14,family="Helvetica", pointsize=18)

par(mfrow= c(3,2), mar = c(5,5,3,1))
exp_exp(1, 4, 1, 2, points[[1]], letter="  A")
title(main="Segment", cex.main=1.5)
exp_exp(3, 6, 1, 2, points[[2]])
title(main="Subtree", cex.main=1.5)
exp_exp(22, 10, 8, 4, points[[1]], letter="  B")
exp_exp(23, 12, 8, 4, points[[2]])
plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
plot(range(0,1), range(0,1), bty='n', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')

exp_exp(1, 22, 1, 8, points[[1]], letter="  C")
title(main="Segment", cex.main=1.5)
exp_exp(3, 23, 1, 8, points[[2]])
title(main="Subtree", cex.main=1.5)
exp_exp(1, 19, 1, 7, points[[1]], letter="  D")
exp_exp(3, 21, 1, 7, points[[2]])
exp_exp(19, 22, 7, 8, points[[1]], letter="  E")
exp_exp(21, 23, 7, 8, points[[2]])

dev.off()