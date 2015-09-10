# This script generates figures for yield ~ morphologies from yield-morph.csv.

gen_fig <- function(x, x_lab, R2, position, letter,
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
  legend("topleft", letter, cex=2.5, bty="n", x.intersp=0, inset=-0.075) 
    # set inset = -0.125 for morph-yield-figs, = -0.075 for std-yield-cov) 
}

insert_blank <- function(){
  plot(range(0,1), range(0,1), bty='n', main = '', xaxt='n', yaxt='n', xlab='', ylab='', type ='n')
}

insert_legend <- function(){
  insert_blank()
  par(xpd=T)
  legend('center', bty = 'n', cex = 2, pt.lwd = 4,
         legend = c('Bud.9', 'G.41', 'G.210', 'M.26', 'JM.8', 'PiAu 56-83'), 
         pch = 21, pt.bg = c('white', 'grey', 'white', 'grey', 'white', 'grey'),
         pt.cex = c(2, 2.5, 3, 3.5, 4, 4.5)
  )
  par(xpd=F)
}

get_ab <- function(x, y){
  test <- lm(y~x)
  print(summary(test)$r.squared)
  return(test$coef)
}

sig_data <- read.csv('yield-morph.csv')

TCSA <- pi*(sig_data$avg_trunk_diam/20)^2
 
### FIG 2
pdf(file="morph-yield.pdf", width= 12, height=9, family="Helvetica", 
    pointsize=14)
par(mfrow= c(3,4), mar = c(6,5,1.2,1),  oma = c(0,0,0,0), 
    cex.lab = 1.8, cex.axis = 1.8)
gen_fig(TCSA, 'TCSA [cm2]', 
        expression(R^2 == 0.473), 'bottomright', 'A',
        y_lab = 'Cumulative Yield [Kg]')
gen_fig(sig_data$avg_canopy_spread, 'Canopy Spread [cm]', 
        expression(R^2 == 0.763), 'bottomright', 'B')
gen_fig(sig_data$avg_max_path, 'Longest Branch [cm]',
        expression(R^2 == 0.868), 'bottomright', 'C')
insert_blank()
gen_fig(sig_data$avg_no_branches, 'No. Branches',
        expression(R^2 == 0.786), 'bottomright', 'D',
        y_lab = 'Cumulative Yield [Kg]')
gen_fig(sig_data$avg_no_twigs, 'No. Twigs',
        expression(R^2 == 0.842), 'bottomright', 'E')
gen_fig(sig_data$avg_no_scars, 'No. Scars',
        expression(R^2 == 0.862), 'bottomright', 'F')
insert_legend()
gen_fig(sig_data$expr_L_D_seg, 'Variance in Segment',
        expression(R^2 == 0.777), 'bottom', 'G',
        y_lab = 'Cumulative Yield [Kg]')
title(sub="Length ~ Diameter ", cex.sub=2, line=4.5)
gen_fig(sig_data$expr_M_D_seg, 'Variance in Segment', 
        expression(R^2 == 0.782), 'bottom', 'H')
title(sub="Mass ~ Diameter", cex.sub=2, line=4.5)
gen_fig(sig_data$expr_M_V_path, 'Variance in Path',
        expression(R^2 == 0.782), 'bottom', 'I')
title(sub="Mass ~ Volume", cex.sub=2, line=4.5) 
dev.off()

png(file="morph-yield.png", width= 12, height=9, units = "in", res = 600,
    pointsize=14)
par(mfrow= c(3,4), mar = c(5,5,1,1),  oma = c(0,0,0,0), 
    cex.lab = 2, cex.axis = 1.8)
gen_fig((sig_data$avg_trunk_diam * pi), 'TCSA [cm2]', 
        expression(R^2 == 0.592), 'bottomright', 
        y_lab = 'Cumulative Yield [Kg]')
gen_fig(sig_data$avg_canopy_spread, 'Canopy Spread [cm]', 
        expression(R^2 == 0.763), 'bottomright')
gen_fig(sig_data$avg_max_path, 'Longest Branch [cm]',
        expression(R^2 == 0.868), 'bottomright')
insert_blank()
gen_fig(sig_data$avg_no_branches, 'No. Branches',
        expression(R^2 == 0.786), 'bottomright',
        y_lab = 'Cumulative Yield [Kg]')
gen_fig(sig_data$avg_no_twigs, 'No. Twigs',
        expression(R^2 == 0.842), 'bottomright')
gen_fig(sig_data$avg_no_scars, 'No. Scars', 
        expression(R^2 == 0.862), 'bottomright')
insert_legend()
gen_fig(sig_data$expr_L_D_seg, 'Segment L~D Var.', 
        expression(R^2 == 0.777), 'bottom', 
        y_lab = 'Cumulative Yield [Kg]')
gen_fig(sig_data$expr_M_D_seg, 'Segment M~D Var.',
        expression(R^2 == 0.782), 'bottom')
gen_fig(sig_data$expr_M_V_path, 'Path M~V Variance',
        expression(R^2 == 0.782), 'bottom')
dev.off()


### COVARIATION

pdf(file="std-yield-cov.pdf", width=4, height=9, family="Helvetica", 
    pointsize=14)
par(mfrow= c(3,1), mar = c(6,5,1.2,1), 
    cex.lab = 1.8, cex.axis = 1.8)
gen_fig((sig_data$avg_cum_yield/sig_data$avg_canopy_spread), 'Yield per Area', 
        expression(R^2 == 0.010), 'bottomright', 'A',
        y = sig_data$avg_cum_yield/TCSA, y_lab = 'Yield Efficiency')
gen_fig((sig_data$avg_cum_yield/sig_data$avg_max_path), 'Yield per Max Length', 
        expression(R^2 == 0.239), 'right', 'B',
        y = TCSA, y_lab = 'Yield Efficiency')
gen_fig((sig_data$avg_cum_yield/sig_data$avg_max_path), 'Yield per Max Length', 
        expression(R^2 == 0.839), 'bottomright', 'C',
        y = (sig_data$avg_cum_yield/sig_data$avg_canopy_spread), 
        y_lab = 'Yield per Area')
dev.off()