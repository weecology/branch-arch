# This script generates figures for yield ~ morphologies from yield-morph.csv.

gen_fig <- function(x, x_lab, R2, position, 
                    y = sig_data$avg_cum_yield, y_lab = ''){
  # generates a yield by morphology plot at rootstock level
  plot(range(min(x) - .15 * min(x), max(x) + .15 * max(x)), 
       range(min(y) - .15 * min(y), max(y) + .15 * max(y)), 
       ylab = y_lab, xlab = x_lab, type = 'n')
  for(rootstock in 1:length(x)){
      points(x[rootstock], y[rootstock],
             cex = (1.5 + .5 * rootstock), pch = 21,  lwd = 4,
             bg = if (rootstock %% 2 == 1) 'white' else 'grey')
  } 
  abline(get_ab(x,y), lwd = 3, lty = 'dashed')
  legend(position, legend=R2, bty='n', cex=2.5) 
}

insert_legend <- function(){
  plot(range(0,1), range(0,1), bty='n', main = '', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
  par(xpd=T)
  legend('center', bty = 'n', cex = 2, pt.lwd = 4,
         legend = sig_data$rootstock, 
         pch = 21, pt.bg = c('white', 'grey', 'white', 'grey', 'white', 'grey'),
         pt.cex = c(2, 2.5, 3, 3.5, 4, 4.5)
  )
  par(xpd=F)
}

get_ab <- function(x, y){
  test <- lm(y~x)
  return(test$coef)
}

sig_data <- read.csv('yield-morph.csv')
 

pdf(file="morph-yield.pdf", width= 10, height=10, family="Helvetica", 
    pointsize=14)
par(mfrow= c(3,3), mar = c(5,5,1,1),  oma = c(0,0,0,0), 
    cex.lab = 2, cex.axis = 1.8)
gen_fig((sig_data$avg_trunk_diam * pi), 'TCSA [cm2]', 
        expression(R^2 == 0.592), 'bottomright', 
        y_lab = 'Cumulative Yield [Kg]')
gen_fig(sig_data$avg_max_path, 'Longest Branch [cm]',
        expression(R^2 == 0.868), 'bottomright')
insert_legend()
gen_fig(sig_data$avg_no_branches, 'No. Branches',
        expression(R^2 == 0.786), 'bottomright',
        y_lab = 'Cumulative Yield [Kg]')
gen_fig(sig_data$avg_no_twigs, 'No. Twigs',
        expression(R^2 == 0.842), 'bottomright')
gen_fig(sig_data$avg_no_scars, 'No. Scars', 
        expression(R^2 == 0.862), 'bottomright')
gen_fig(sig_data$expr_L_D_seg, 'Segment L~D Variance', 
        expression(R^2 == 0.777), 'topright', 
        y_lab = 'Cumulative Yield [Kg]')
gen_fig(sig_data$expr_M_D_seg, 'Segment M~D Variance',
        expression(R^2 == 0.782), 'topright')
gen_fig(sig_data$expr_M_V_path, 'Path M~V Variance',
        expression(R^2 == 0.782), 'topright')
dev.off()
