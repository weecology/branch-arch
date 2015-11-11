### This code generates and analyses standard yield metric in hort research.

library(agricolae)
library(ggplot2)
library(stringr)
library(smatr)
library(dplyr)

## Data

tree_sum <- read.csv("TreeSummary.csv")
yield <- read.csv("AppleYield.csv", sep =',', head=T)
tree_yield <- inner_join(tree_sum, yield)
tree_yield <- dplyr::mutate(tree_yield, TCSA = pi*(trunk_diam_cm/2)^2)
sma <- read.csv("SMAResults.csv")

## ANOVA and Duncan's test

model <- aov(TCSA ~ as.factor(tree_yield$rootstock))
summary_TCSA <- summary(model)
test  <- duncan.test(model, "as.factor(tree_yield$rootstock)")
duncan_TCSA  <- test$groups

model <- aov(yield ~ as.factor(rootstock),
             data = tree_yield)
summary_yield <- summary(model)
test  <- duncan.test(model, "as.factor(rootstock)")
duncan_yield  <- test$groups

model <- aov(cum_yield ~ as.factor(rootstock),
             data = tree_yield)
summary_cum_yield <- summary(model)
test  <- duncan.test(model, "as.factor(rootstock)")
duncan_cum_yield  <- test$groups

model <- aov((tree_yield$yield/tree_yield$TCSA) ~ as.factor(tree_yield$rootstock))
summary_ye <- summary(model)
test  <- duncan.test(model, "as.factor(tree_yield$rootstock)")
duncan_ye  <- test$groups

model <- aov((tree_yield$cum_yield/TCSA) ~ as.factor(tree_yield$rootstock))
summary_YE <- summary(model)
test  <- duncan.test(model, "as.factor(tree_yield$rootstock)")
duncan_YE  <- test$groups

model <- aov((cum_yield/canopy_spread) ~ rootstock, tree_yield)
summary_YS <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_YS  <- test$groups

model <- aov((pi*(canopy_spread/2)^2) ~ rootstock, tree_yield)
summary_canopy_area <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_canopy_area  <- test$groups

model <- aov((cum_yield/(pi*(canopy_spread/2)^2)) ~ rootstock, tree_yield)
summary_YA <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_YA  <- test$groups

model <- aov(1000*yield / (tot_stem_m + tot_twig_m + 1000*yield)  ~ 
               as.factor(rootstock), data = tree_yield)
summary_YI <- summary(model)
test  <- duncan.test(model, "as.factor(rootstock)")
duncan_YI  <- test$groups

model <- aov(1000*cum_yield / (tot_stem_m + tot_twig_m + 1000*cum_yield)  ~ 
               as.factor(rootstock), data = tree_yield)
summary_HI <- summary(model)
test  <- duncan.test(model, "as.factor(rootstock)")
duncan_HI  <- test$groups

## Allometry

Y0 <- -0.86  # str_split(sma$X.Subtree..7[3], ";")[[1]][1]
a <- 2.67  # str_split(sma$X.Subtree..7[3], ";")[[1]][4]
mass <- 10^(as.numeric(Y0) +  as.numeric(a)*log10(tree_yield$trunk_diam))

allom_mass <- lm(mass ~ (tree_yield$tot_stem_m + tree_yield$tot_twig_m))  #0.972

model <- aov(1000*tree_yield$yield / (1000*tree_yield$yield + mass)  ~ 
               as.factor(tree_yield$rootstock))
summary_YI_a <- summary(model)
test  <- duncan.test(model, "as.factor(tree_yield$rootstock)")
duncan_YI_a  <- test$groups

model <- aov(1000*tree_yield$cum_yield / (1000*tree_yield$cum_yield + mass)  ~ 
               as.factor(tree_yield$rootstock))
summary_HI_a <- summary(model)
test  <- duncan.test(model, "as.factor(tree_yield$rootstock)")
duncan_HI_a  <- test$groups

## Test SMA for TCSA

test <- sma(log10((tree_yield$tot_stem_m + tree_yield$tot_twig_m)) ~ 
              log10(tree_yield$trunk_diam))

test <- sma((tree_yield$tot_stem_m + tree_yield$tot_twig_m) ~ TCSA)


### YE correlation

morph_yield_eff <- c()
for (morph in tree_yield[c(-1,-2)]) {
  test <- lm(tree_yield$cum_yield / tree_yield$TCSA ~ morph)
  morph_yield_eff <- append(morph_yield_eff, round(summary.lm(test)$r.squared, 3))
}
morph_yield_eff <- cbind(names(tree_yield[c(-1,-2)]), morph_yield_eff)

morph_harvest_index <- c()
for (morph in tree_yield[c(-1,-2)]) {
  test <- lm(tree_yield$cum_yield / (tree_yield$tot_stem_m + tree_yield$tot_twig_m) ~ morph)
  morph_harvest_index <- append(morph_harvest_index, round(summary.lm(test)$r.squared, 3))
}
morph_harvest_index <- cbind(names(tree_yield[c(-1,-2)]), morph_harvest_index)

## Visualize

ggplot(tree_yield) +
  geom_point(aes(x = (tot_stem_m + tot_twig_m)/1000, y = yield, 
                 color = factor(rootstock)), size = 5) +
  labs(x = "Total Above-ground Biomass", y = "Yield", 
       color="Rootstocks") +
  geom_abline(slope = 6) + geom_abline(slope = 2) + 
  geom_abline(slope = 0.5) + geom_abline(slope = 0.1) +
  geom_text(data=NULL, x=5, y=55, label="HI = 6") +
  geom_text(data=NULL, x=25, y=55, label="2") +
  geom_text(data=NULL, x=75, y=40, label="0.5") +
  geom_text(data=NULL, x=80, y=10, label="0.1") +
  theme_classic(base_size = 18, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20))

ggplot(tree_yield) +
  geom_point(aes(x = (tot_stem_m + tot_twig_m)/1000, y = cum_yield, 
                 color = factor(rootstock)), size = 5) +
  labs(x = "Total Above-ground Biomass", y = "Cumulative Yield", 
       color="Rootstocks") +
  geom_abline(slope = 30) + geom_abline(slope = 15) + 
  geom_abline(slope = 8) + geom_abline(slope = 4) +
  geom_text(data=NULL, x=6, y=350, label="HI = 30") +
  geom_text(data=NULL, x=21, y=375, label="15") +
  geom_text(data=NULL, x=35, y=325, label="8") +
  geom_text(data=NULL, x=45, y=200, label="4") +
  theme_classic(base_size = 18, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20))
