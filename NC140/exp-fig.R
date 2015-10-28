# This script generates figures from the SMAResults.csv.

library(stringr)

restructure_data <- function(data){
  # Compiles data for computation from the visually oriented SMAResults file
  new_data <- list()
  for (i in 1:27){                              # Scaling relationships
    new_data[[i]] <- list()
    for (j in 1:7){                             # Results output
      #  1. Intercept, 2. Int CI-, 3. Int CI+, 
      #  4. Exponent,  5. Exp CI-, 6. Exp CI+, 7.R2 
      new_data[[i]][[j]] <- vector(length = 32)
      for (k in 1:32) {                         # Groups and individuals
        new_data[[i]][[j]][k] = as.numeric(str_split(
          data[(k+1),(i+2)], ";")[[1]][j])
      }
    }
  }
  return(new_data)
}

poly_set <- function(x_set, n){
  polygon(x_set, 
          c(min(results[[n]][[5]], na.rm=T)-1, max(results[[n]][[6]], na.rm=T)+1, 
            max(results[[n]][[6]], na.rm=T)+1, min(results[[n]][[5]], na.rm=T)-1), 
          col = rgb(.80,.80,.80,0.5), border = NA)
}

gen_plot <- function(n, exclude = exclude_missing){
  plot(range(1,32), range(min(results[[n]][[5]][-exclude], na.rm=T), max(results[[n]][[6]][-exclude], na.rm=T)), 
       ylab = ylabels[n], xlab="", xaxt = 'n', type = 'n',
       ylim = c(min(results[[n]][[5]][-exclude], na.rm=T), max(results[[n]][[6]][-exclude], na.rm=T)))
  poly_set(c(2.5, 2.5, 8.5, 8.5), n)
  poly_set(c(14.5, 14.5, 20.5, 20.5), n) 
  poly_set(c(22.5, 22.5, 27.5, 27.5), n) 
  poly_set(c(37.5, 37.5, 43, 43), n) 
  points(results[[n]][[4]], pch = 19, cex = 3*results[[n]][[7]])
  for(s in 1:32){
    arrows(y0=results[[n]][[5]][s], y1=results[[n]][[6]][s], x0=s, x1=s, code=3, angle=90, lwd=1.7, length=.08)  
  }
  #abline(h = flow[n], lwd = 2, lty = 6)
  #abline(h = elastic[n], lwd = 2, lty = 2)
}

multi_plot <- function(col_list, location){
  par(mfrow = c(length(col_list),1), oma = c(9,0,0,0), mar = c(1,5,1,2), 
      cex.lab = 1.7, cex.axis=1.7, bty = 'o')
  gen_plot(col_list[1])
  par(xpd=T)
  legend(location, legend=c("R2 = 0.99", "R2 = 0.80", "R2 = 0.67", "R2 = 0.33"), 
         pch=19, bty = 'n', 
         pt.cex = c(3*.99, 3*.80, 3*.67, 3*.33, 0, 0), cex = 1.25)
  par(xpd=F)
  for (i in col_list[-1]){ gen_plot(i) }
  axis(1, 1:32, sma[2:33,2], las = 2)
}

gen_plot_roots <- function(results, n, relationship, rootstocks = roots_list, 
                           axis = FALSE){
  # general plot for scaling results by rootstock and individual
  plot(range(0,9), range(min(results[[relationship]][[5]][roots_no]), 
                         max(results[[relationship]][[6]][roots_no])), 
       ylab = relationships_abv[n], xlab="", xaxt = 'n', type = 'n',
       ylim = c(min(results[[relationship]][[5]][roots_no]), 
                max(results[[relationship]][[6]][roots_no]))
  )
  
  for(s in 1:8){
    points(s, results[[relationship]][[4]][as.numeric(rootstocks[[s]][1])], 
           pch = 19, cex = 5, bg = "black")
    arrows(y0=results[[relationship]][[5]][as.numeric(rootstocks[[s]][1])], 
           y1=results[[relationship]][[6]][as.numeric(rootstocks[[s]][1])], 
           x0=s, x1=s, code=3, angle=90, lwd=2.5, length=.08)  
  }
  
  legend("topright", LETTERS[n], cex=2.5, bty="n", x.intersp=0)
  
  if (axis == TRUE){
    axis(1, 1:8, names_abv, las = 2, cex.axis = 2.7)
  } else {
    axis(1, 1:8, labels = F, las = 2, cex.axis = 2.7)
  }
  
  abline(h = 0, lwd = 2.5, lty = 1)
}

multi_plot_roots <- function(results){
  # generates an panel of exponent vs rootstock
  par(mfrow= c(3,3), mar = c(4,5,1,1),  oma = c(9,0,0,0), 
      cex.lab = 2.7, cex.axis = 2.7)
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

sma <- read.csv("SMAResults.csv", sep=",", head=T)   
results <- restructure_data(sma)

ylabels <- c("L~D (Segment)", "L~D (Path)", "L~D (Subtree)", "SA~V (Segment)", "SA~V (Path)", "SA~V (Subtree)", 
             "D~V (Segment)", "D~V (Path)", "D~V (Subtree)", "L~V (Segment)", "L~V (Path)", "L~V (Subtree)", 
             "D~SA (Segment)", "D~SA (Path)", "D~SA (Subtree)", "L~SA (Segment)", "L~SA (Path)", "L~SA (Subtree)", 
             "L~M (Segment)", "L~M (Path)", "L~M (Subtree)", "M~D (Segment)", "M~D (Subtree)", 
             "M~V (Segment)", "M~V (Path)", "M~V (Subtree)", "D/P Ratio ~ P Diam")

relationships_abv <- c("Length ~ Diameter", "Area ~ Volume", "Diameter ~ Volume", 
                       "Length ~ Volume", "Diameter ~ Area", "Length ~ Area", 
                       "Length ~ Mass", "Mass ~ Diameter", "Mass ~ Volume")

names_abv <- c("All-tree", "All-branch", 
               "Bud.9", "G.41","G.210","M.26", "JM.8", "PiAu 56-83")

roots_no   <- c(1, 2, 3, 9, 15, 21, 23, 28)
exclude_missing <- c(1, 8, 9, 11, 19, 22, 28, 32)
roots_list <- list(c(1, 'black'),
                   c(2, 'grey'),
                   c(28, 4.5),
                   c(23, 4),
                   c(21, 3.5),
                   c(15, 3),
                   c(9, 2.5),
                   c(3, 2))

pdf(file="exp-fig.pdf", width= 10, height=10,family="Helvetica", pointsize=14)

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

pdf(file="exp-fig-rootstocks.pdf", width= 16, height=12,family="Helvetica", pointsize=14)
multi_plot_roots(results) 
dev.off()




