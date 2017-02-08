### plotting functions

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

lm_eqn <- function(formula, df = avg_vol, params=FALSE, round = 3){
  m <- lm(formula, data = df)
  if (params==FALSE) {
    eq <- substitute(~~italic(r)^2~"="~r2,
                     list(r2 = format(summary(m)$r.squared, digits = round)))
  } else { 
    eq <- substitute(~~italic(r)^2~"="~r2 ~~italic(a)~"="~a1 ~~italic(b)~"="~b1,
                     list(r2 = format(summary(m)$r.squared, digits = 3),
                          a1 = format(summary(m)$coef[2,1], digits = 3),
                          b1 = format(summary(m)$coef[1,1], digits = 3)))
  }
  return(as.character(as.expression(eq)))
}

lm_intercept <- function(formula, df1 = old, df2 = young){
  m1 <- lm(formula, data = df1)
  a1 <- summary(m1)$coef[2,1]
  b1 <- summary(m1)$coef[1,1]
  
  m2 <- lm(formula, data = df2)
  a2 <- summary(m2)$coef[2,1]
  b2 <- summary(m2)$coef[1,1]
  
  x_intercept <- (b2 - b1) / (a1 - a2)
  return(round(x_intercept, 2))
}