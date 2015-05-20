### This script generates Root Biomass Distribution plots for within and between
### row transects

source('~/Desktop/branch-arch/NC140/root-yield.R', echo = FALSE)

vplayout <- function(x,y){
  viewport(layout.pos.row = x, layout.pos.col = y)
}

within_plots <- c()
for (i in c(1:5)){
  rootstock_within <- filter(roots_depth, 
                             rootstock == unique(rootstocks)[i], 
                             direction == "within")
  
  plot <- ggplot(rootstock_within, 
                 aes(x = distance, y = (-1*max_depth), z = total_roots)) 
  within_plots[[i]] <- plot + 
    stat_contour(geom="polygon", aes(fill=..level..), bins = 30) + 
    labs(title = unique(rootstocks)[i])
}

pdf('Within-Row-Total-Mass.pdf', width= 14, height=7, family="Helvetica", pointsize=12)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,3)))

print(within_plots[[1]], vp = vplayout(1,1))
print(within_plots[[2]], vp = vplayout(1,2))
print(within_plots[[3]], vp = vplayout(1,3))
print(within_plots[[4]], vp = vplayout(2,1))
print(within_plots[[5]], vp = vplayout(2,2))

dev.off()

between_plots <- c()
for (i in c(1:5)){
  rootstock_between <- filter(roots_depth, 
                              rootstock == unique(rootstocks)[i], 
                              direction == "between")
  
  plot <- ggplot(rootstock_between, 
                 aes(x = distance, y = (-1*max_depth), z = total_roots)) 
  
  between_plots[[i]] <- plot + 
    stat_contour(geom="polygon", aes(fill=..level..), bins = 30) + 
    labs(title = unique(rootstocks)[i])
}

pdf('Between-Row-Total-Mass.pdf', width= 14, height=7, family="Helvetica", pointsize=12)

grid.newpage()
pushViewport(viewport(layout = grid.layout(2,3)))

print(between_plots[[1]], vp = vplayout(1,1))
print(between_plots[[2]], vp = vplayout(1,2))
print(between_plots[[3]], vp = vplayout(1,3))
print(between_plots[[4]], vp = vplayout(2,1))
print(between_plots[[5]], vp = vplayout(2,2))

dev.off()