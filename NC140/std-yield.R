### This code generates and analyses standard yield metric in hort research.

library(dplyr)
library(agricolae)
library(ggplot2)

tree_sum <- read.csv("TreeSummary.csv")
yield <- read.csv("AppleYield.csv", sep =',', head=T)
tree_yield <- inner_join(tree_sum, yield)
TCSA <- pi*(tree_yield$trunk_diam_cm/2)^2

model <- aov(TCSA ~ as.factor(tree_yield$rootstock))
summary_TCSA <- summary(model)
test  <- duncan.test(model, "as.factor(tree_yield$rootstock)")
duncan_TCSA  <- test$groups

model <- aov((tree_yield$cum_yield/TCSA) ~ as.factor(tree_yield$rootstock))
summary_YE <- summary(model)
test  <- duncan.test(model, "as.factor(tree_yield$rootstock)")
duncan_YE  <- test$groups

model <- aov(cum_yield / (tot_stem_m + tot_twig_m)  ~ as.factor(rootstock),
             data = tree_yield)
summary_HI <- summary(model)
test  <- duncan.test(model, "as.factor(rootstock)")
duncan_HI  <- test$groups

ggplot(tree_yield) +
  geom_point(aes(x = (tot_stem_m + tot_twig_m), y = cum_yield, 
                 color = factor(rootstock), size = 5))

##NEXT simulate mass using D~M equation
