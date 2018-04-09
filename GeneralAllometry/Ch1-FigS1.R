### This script builds a figure to visualize the linear vs poly fits against r2 
### and sample size. The figure appears as Fig S1 in the manuscript.

fits <- read.csv('Fits_for_fig.csv', sep=',', head=T)

labels <- c("Length ~ Diameter (Segment)", "L~D (Path)", "Length ~ Diameter (Subtree)", 
            "Area ~ Volume (Segment)", "Area ~ Volume (Subtree)", 
            "Diameter ~ Volume (Segment)", "Diameter ~ Volume (Subtree)", 
            "Length ~ Volume (Segment)", "L~V (Path)", "Length ~ Volume (Subtree)", 
            "Diameter ~ Area (Segment)", "Diameter ~ Area (Subtree)", 
            "Length ~ Area (Segment)", "L~SA (Path)", "Length ~ Area (Subtree)", 
            "Length ~ Mass (Segment)", "L~M (Path)", "Length ~ Mass (Subtree)", 
            "Mass ~ Diameter (Segment)", "Mass ~ Diameter (Subtree)", 
            "Mass ~ Volume (Segment)", "Mass ~ Volume (Subtree)")

fit_vals <- matrix(ncol=22, nrow=32)
r_vals <- matrix(ncol=22, nrow=32)
n_vals <- matrix(ncol=22, nrow=32)

for (i in 1:22){
  for (j in 1:32){
    fit_vals[j,i] = as.numeric(strsplit(as.character(fits[i,(j+1)]), ",")[[1]][1])
    r_vals[j,i] = as.numeric(strsplit(as.character(fits[i,(j+1)]), ",")[[1]][2])
    n_vals[j,i] = as.numeric(strsplit(as.character(fits[i,(j+1)]), ",")[[1]][3])
  }
}

multi_plot_r <- function(col_list){
  for (i in col_list){
    plot(r_vals[,i], fit_vals[,i], main = labels[i], pch=21, lwd=2.5, cex=1.75, 
         ylim=c(0,3), xlim=c(0,1), ylab = 'Fit Value', xlab='r2',  yaxt='n')
    axis(2, at = c(0,1,3), labels = c("Poly", "NS", "Linear"))
  }
}

multi_plot_n <- function(col_list){
  for (i in col_list){
    plot(n_vals[,i], fit_vals[,i], main = labels[i], pch=21, lwd=2.5, cex=1.75, 
         ylim=c(0,3), xlim=c(0,820), ylab = 'Fit Value', xlab = 'n', yaxt='n')
    axis(2, at = c(0,1,3), labels = c("Poly", "NS", "Linear"))
  }
}

pdf(file="FigS1.pdf", width=12, height=12, family="Helvetica", pointsize=14)

###Fit vs R2
par(mfrow= c(5,4), mar=c(5,5,2,1), cex.lab = 1.5, cex.axis=1.2)
multi_plot_r(c(1,3))
multi_plot_r(c(4,5))
multi_plot_r(c(6,7))
multi_plot_r(c(8,10))
multi_plot_r(c(11,12))
multi_plot_r(c(13,15))
multi_plot_r(c(16,18))
multi_plot_r(c(19,20))
multi_plot_r(c(21,22))

###Fit vs n
par(mfrow= c(5,4))
multi_plot_n(c(1,3))
multi_plot_n(c(4,5))
multi_plot_n(c(6,7))
multi_plot_n(c(8,10))
multi_plot_n(c(11,12))
multi_plot_n(c(13,15))
multi_plot_n(c(16,18))
multi_plot_n(c(19,20))
multi_plot_n(c(21,22))

dev.off()


