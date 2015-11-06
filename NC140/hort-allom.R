### This script generates the allometries reported in Table 1.

library(smatr)
library(ggplot2)

tree_sum <- read.csv("TreeSummary.csv")
yield <- read.csv("AppleYield.csv", sep =',', head=T)
tree_yield <- inner_join(tree_sum, yield)
TCSA <- pi*(tree_yield$trunk_diam_cm/2)^2
sma <- read.csv("SMAResults.csv")

test <- sma(log10((tree_yield$tot_stem_m + tree_yield$tot_twig_m)) ~ log10(TCSA))
test <- sma(log10((tree_yield$height)) ~ log10(TCSA))
test <- sma(log10((tree_yield$tot_stem_m + tree_yield$tot_twig_m)) ~ log10(tree_yield$height))
test <- sma(log10((tree_yield$tot_stem_m + tree_yield$tot_twig_m)) ~ log10(tree_yield$tot_volume))
test <- sma(log10((tree_yield$tot_volume)) ~ log10(TCSA))
summary(test)

for (a in tree_sum[c(-1,-2)]) {
  ifelse(length(a[a<=0]) > 0, 
         print('No test'),
         print(sma(log10((tree_yield$tot_stem_m + tree_yield$tot_twig_m)) ~ 
                     log10(a))$r2[[1]]))
}

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