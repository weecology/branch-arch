### This script generates the allometries reported in Table 1.

library(smatr)
library(ggplot2)

tree_sum <- read.csv("TreeSummary.csv")
yield <- read.csv("AppleYield.csv", sep =',', head=T)
tree_yield <- inner_join(tree_sum, yield)
tree_yield <- dplyr::mutate(tree_yield, TCSA = pi*(trunk_diam_cm/2)^2)
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
                              avg_canopy_volume = round(mean(canopy_volume), 3),
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

ggplot() +
  geom_point(aes(x=log10(TCSA), 
                 y=log10((tree_yield$tot_stem_m + tree_yield$tot_twig_m)), 
                 col=tree_yield$rootstock))
ggplot() +
  geom_point(aes(x=log10(TCSA), y=log10(tree_yield$height), 
                 col=tree_yield$rootstock))

ggplot() +
  geom_point(aes(x=log10(tree_yield$tot_volume), 
                 y=log10((tree_yield$tot_stem_m + tree_yield$tot_twig_m)), 
                 col=tree_yield$rootstock))