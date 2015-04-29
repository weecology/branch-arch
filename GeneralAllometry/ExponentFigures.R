# This script generates figures from the SMAResults.csv.

sma <- read.csv('SMAResults.csv', sep=',', head=T)

poly_set <- function(x_set, n){
  polygon(x_set, 
          c(min(results[[n]][[2]], na.rm=T)-1, max(results[[n]][[3]], na.rm=T)+1, max(results[[n]][[3]], na.rm=T)+1, min(results[[n]][[2]], na.rm=T)-1), 
          col = rgb(.80,.80,.80,0.5), border = NA)
}

gen_plot <- function(n){
  plot(range(2,41), range(min(results[[n]][[2]][4:41], na.rm=T), max(results[[n]][[3]][4:41], na.rm=T)), 
       ylab = ylabels[n], xlab="", xaxt = 'n', type = 'n',
       ylim = c(min(results[[n]][[2]][4:41], na.rm=T), max(results[[n]][[3]][4:41], na.rm=T)))
  poly_set(c(4.5, 4.5, 11.5, 11.5), n)
  poly_set(c(18.5, 18.5, 24.5, 24.5), n) 
  poly_set(c(30.5, 30.5, 32.5, 32.5), n) 
  poly_set(c(37.5, 37.5, 43, 43), n) 
  points(results[[n]][[1]], pch = 19, cex = 3*results[[n]][[4]])
  for(s in 1:42){
    arrows(y0=results[[n]][[2]][s], y1=results[[n]][[3]][s], x0=s, x1=s, code=3, angle=90, lwd=1.7, length=.08)  
  }
  abline(h = flow[n], lwd = 2, lty = 6)
  abline(h = elastic[n], lwd = 2, lty = 2)
}

multi_plot <- function(col_list, location){
  par(mfrow = c(length(col_list),1), oma = c(7,0,0,0), mar = c(1,5,1,2), cex.lab = 1.5, bty = 'o')
  gen_plot(col_list[1])
  par(xpd=T)
  legend(location, legend=c("R2 = 0.99", "R2 = 0.80", "R2 = 0.67", "R2 = 0.33", "Elastic Exp", "Flow Exp"), 
         lty = c(0,0,0,0,2,6), lwd = c(0,0,0,0,1.5,1.5), pch=19, bty = 'n', pt.cex = c(3*.99, 3*.80, 3*.67, 3*.33, 0, 0))
  par(xpd=F)
  for (i in col_list[-1]){ gen_plot(i) }
  axis(1, 1:42, sma[2:43,2], las = 2)
}
  

#Compiles data for computation from the visually oriented SMAResults file
results <- list()
for (i in 1:27){
  results[[i]] <- list()
  for (j in 1:4){
    results[[i]][[j]] <- vector(length = 42)
    # 1: exponent, 2: CI-, 3: CI+, 4: R2 
    for (k in 1:42) {
      results[[i]][[j]][k] = as.numeric(strsplit(as.character(sma[(k+1),(i+2)]), " ")[[1]][(2*j-1)])
    }
  }
}

ylabels <- c("L~D (Segment)", "L~D (Path)", "L~D (Subtree)", "SA~V (Segment)", "SA~V (Path)", "SA~V (Subtree)", 
             "D~V (Segment)", "D~V (Path)", "D~V (Subtree)", "L~V (Segment)", "L~V (Path)", "L~V (Subtree)", 
             "D~SA (Segment)", "D~SA (Path)", "D~SA (Subtree)", "L~SA (Segment)", "L~SA (Path)", "L~SA (Subtree)", 
             "L~M (Segment)", "L~M (Path)", "L~M (Subtree)", "M~D (Segment)", "M~D (Subtree)", 
             "M~V (Segment)", "M~V (Path)", "M~V (Subtree)", "D/P Ratio ~ P Diam")

flow <- c(2, 2, 2, .75, .75, .75, .25, .25, .25, .5, .5, .5, .33, .33, .33, .67, .67, .67, 10, 10, 10, 10, 10, 10, 10, 10, 10)

elastic <- c(.67, .67, .67, .625, .625, .625, .375, .375, .375, .25, .25, .25, .6, .6, .6, .4, .4, .4, .25, .25, .25, 2.67, 2.67, 10, 10, 10, 10)

pdf(file="ExponentFigures.pdf", width= 10, height=10,family="Helvetica", pointsize=12)

multi_plot(c(1:3), 'bottomright')
multi_plot(c(4:6), 'topright')
multi_plot(c(7:9), 'topright')
multi_plot(c(10:12), 'bottomright')
multi_plot(c(13:15), 'bottomright')
multi_plot(c(16:18), 'topright')
multi_plot(c(19:21), 'topright')
multi_plot(c(22:23), 'topright')
multi_plot(c(24:26), 'bottomright')
multi_plot(c(27), 'topright')

dev.off()





