### This script generates the allometries reported in Table 1.

library(smatr)
library(ggplot2)

tree_sum <- read.csv("TreeSummary.csv")
yield <- read.csv("AppleYield.csv", sep =',', head=T)
canopy_volumes <- read.csv("VolumeEstimates.csv") %>%
  dplyr::filter(species == "apple") %>%
  dplyr::select(tree, triangles)
tree_sum <- inner_join(tree_sum, canopy_volumes)
tree_yield <- inner_join(tree_sum, yield)
tree_yield <- dplyr::mutate(tree_yield, 
                            TCSA = pi*(trunk_diam_cm/2)^2,
                            tot_length_m = tot_length/100,
                            tot_area_m2 = tot_area/10000,
                            tot_volume_m3 = tot_volume/1000000,
                            canopy_area = pi*(canopy_spread/2)^2,
                            tot_mass_kg = (tot_stem_m + tot_twig_m)/1000)
sma <- read.csv("SMAResults.csv")

### Individual Level

test <- sma(log10((tot_stem_m + tot_twig_m)) ~ log10(TCSA), data = tree_yield)
test <- sma(log10(height) ~ log10(TCSA), data = tree_yield)
test <- sma(log10((tot_stem_m + tot_twig_m)) ~ log10(height), data = tree_yield)
test <- sma(log10((tot_stem_m + tot_twig_m)) ~ log10(tot_volume), data = tree_yield)
test <- sma(log10((tot_volume)) ~ log10(TCSA), data = tree_yield)
test <- sma(log10(cum_yield) ~ log10(TCSA), data = tree_yield)
test <- sma(log10(tot_stem_m + tot_twig_m) ~ log10(pi*(canopy_spread/2)^2), data = tree_yield)
test <- sma(log10(cum_yield) ~ log10(pi*(canopy_spread/2)^2), data = tree_yield)
summary(test)

mass_allometry <- c()
cum_yield_allometry <- c()
for (a in tree_sum[c(-1,-2)]) {
  mass_allometry <- c(mass_allometry, 
                      ifelse(length(a[a<=0]) > 0, 'No test',
                        sma(log10((tree_yield$tot_stem_m + tree_yield$tot_twig_m)) ~ 
                        log10(a))$r2[[1]]))
  cum_yield_allometry <- c(cum_yield_allometry, 
                      ifelse(length(a[a<=0]) > 0, 'No test',
                             sma(log10(tree_yield$cum_yield) ~ 
                                   log10(a))$r2[[1]]))
}
allometry <- cbind(names(tree_sum[c(-1,-2)]), 
                   mass_allometry, cum_yield_allometry)


### Rootstock Level

tree_yield_roots <- dplyr::arrange(dplyr::summarize(
                              dplyr::group_by(tree_yield, rootstock),
                              avg_TCSA = round(mean(TCSA), 3),
                              avg_height = round(mean(height), 3),
                              avg_max_path = round(mean(max_path), 3),
                              avg_stem_length = round(mean(tot_length), 3),
                              avg_stem_area = round(mean(tot_area), 3),
                              avg_stem_volume = round(mean(tot_volume), 3),
                              avg_stem_mass = round(mean(tot_stem_m + tot_twig_m), 3),
                              avg_canopy_spread = round(mean(canopy_spread),3),
                              avg_canopy_area = round(mean(pi*(canopy_spread/2)^2), 3),
                              avg_canopy_volume = round(mean(triangles), 3),
                              avg_cum_yield = round(mean(cum_yield), 3)),
                            avg_TCSA)

mass_allom_roots <- c()
cum_yield_allom_roots <- c()
for (a in tree_yield_roots[-1]) {
  mass_allom_roots <- c(mass_allom_roots, 
                        ifelse(length(a[a<=0]) > 0, 'No test',
                               sma(log10(tree_yield_roots$avg_stem_mass) ~ 
                                   log10(a))$r2[[1]]))
  cum_yield_allom_roots <- c(cum_yield_allom_roots, 
                             ifelse(length(a[a<=0]) > 0, 'No test',
                                    sma(log10(tree_yield_roots$avg_cum_yield) ~ 
                                        log10(a))$r2[[1]]))
}
allom_roots <- cbind(names(tree_yield_roots[-1]), 
                   mass_allom_roots, cum_yield_allom_roots)


### Root Data

tree_root <- left_join(tree_sum, read.csv('StumpMass.csv'))
test <- sma(log10((tot_stem_m + tot_twig_m + stump_wgt_kg)) ~ log10(TCSA), data = tree_root)
test <- sma(log10((tot_stem_m + tot_twig_m + stump_wgt_kg)) ~ log10(cum_yield), data = tree_root)

### Visualize

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


png("allometries.png", width = 1500, height = 450)
a1 <- ggplot(tree_yield, aes(x=log10(TCSA), y=log10(tot_mass_kg))) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=rootstock), size=8) +
  labs(x="Log( TCSA )", y="Log( Stem Biomass )", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

a2 <- ggplot(tree_yield, aes(x = log10(tot_mass_kg), y = log10(cum_yield))) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape = rootstock), size = 8) +
  labs(x = "Log( Stem Biomass )", y = "Log( Cumulative Yield )",
       title="B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

a3 <- ggplot(tree_yield, aes(x = log10(TCSA), y = log10(cum_yield))) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape = rootstock), size = 8) +
  labs(x = "Log( TCSA )", y = "Log( Cumulative Yield )",
       title="C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), 
        legend.justification=c(1,0), legend.position=c(1, 0))
multiplot(a1, a2, a3, cols=3)
dev.off()

ggplot() +
  geom_point(aes(x=log10(TCSA), y=log10(tree_yield$height), 
                 col=tree_yield$rootstock))

ggplot() +
  geom_point(aes(x=log10(tree_yield$tot_volume), 
                 y=log10((tree_yield$tot_stem_m + tree_yield$tot_twig_m)), 
                 col=tree_yield$rootstock))