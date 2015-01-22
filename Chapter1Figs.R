###This script builds the figures used in the Chapter 1 publication.###

###initiate exponent results
sma <- read.csv('SMAResults.csv', sep=',', head=T)

results <- list()
for (i in 1:23){ #relationship columns
  results[[i]] <- list()
  for (j in 1:4){
    results[[i]][[j]] <- vector(length = 42)
    # 1: exponent, 2: CI-, 3: CI+, 4: R2 
    for (k in 1:42) { # group/ind rows
      results[[i]][[j]][k] = as.numeric(strsplit(as.character(sma[(k+1),(i+2)]), " ")[[1]][(2*j-1)])
    }
  }
}

###Subtree improves R2

range_values(col_list{
  mins <- vector(length = length(col_list))
  maxs <- vector(length = length(col_list))
  for (i in col_list){
    mins[i] = results[[i]][[2]][4:41]
}

gen_exponent <- function(col_list, n){
  plot(range(0,1), range(min(results[[col_list[1]:col_list[length(col_list)]]][[2]][4:41], na.rm=T), 
                         max(results[[col_list[1]:col_list[length(col_list)]]][[3]][4:41], na.rm=T)), 
       ylab = ylabels[n], xlab="R2", xaxt = 'n', type = 'n',
       ylim = c(min(results[[n]][[2]][4:41], na.rm=T), max(results[[n]][[3]][4:41], na.rm=T)))
  for (j in group_list){
    points(results[[col_list[i]]][[4]][4], results[[col_list[i]]][[1]][4], pch = 19, cex = 2)
    arrows(x0=results[[col_list[i]]][[4]][4], y0=results[[col_list[i]]][[2]][4], y1=results[[col_list[1]]][[3]][4], 
           code=3, angle=90, lwd=1.7, length=.08)
  }
  abline(h = flow[n], lwd = 2, lty = 6)
  abline(h = elastic[n], lwd = 2, lty = 2)
}

multi_plot <- function(col_list, location){
  par(mfrow = c(length(col_list),1), oma = c(7,0,0,0), mar = c(1,5,1,2), cex.lab = 1.5, bty = 'o')
  gen_exponent(col_list[1])
  par(xpd=T)
  legend(location, legend=c("R2 = 0.99", "R2 = 0.80", "R2 = 0.67", "R2 = 0.33", "Elastic Exp", "Flow Exp"), 
         lty = c(0,0,0,0,2,6), lwd = c(0,0,0,0,1.5,1.5), pch=19, bty = 'n', pt.cex = c(3*.99, 3*.80, 3*.67, 3*.33, 0, 0))
  par(xpd=F)
  for (i in col_list[-1]){ gen_exponent(i) }
  axis(1, 1:42, sma[2:43,2], las = 2)
}

ylabels <- c("L~D", "SA~V", "D~V", "L~V", "D~SA", "L~SA", "L~M", "M~D", "M~V")
flow <- c(2, .75, .25, .5, .33, .67, 10, 10, 10)
elastic <- c(.67, .625, .375, .25, .6, .4, .25, 2.67, 10)


multi_plot(c(1:3), 'bottomright')
multi_plot(c(4:5), 'topright')
multi_plot(c(6:7), 'topright')
multi_plot(c(8:10), 'bottomright')
multi_plot(c(11:12), 'bottomright')
multi_plot(c(13:15), 'topright')
multi_plot(c(16:18), 'topright')
multi_plot(c(19:20), 'topright')
multi_plot(c(21:22), 'bottomright')
multi_plot(c(23), 'topright')
