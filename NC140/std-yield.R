### This code generates and analyses standard yield metric in hort research.

library(agricolae)
library(ggplot2)
library(stringr)
library(smatr)
library(dplyr)
library(grid)

## Data

tree_sum <- read.csv("TreeSummary.csv")
canopy_volumes <- read.csv("VolumeEstimates.csv") %>%
  dplyr::filter(species == "apple") %>%
  dplyr::select(tree, triangles)
tree_sum <- inner_join(tree_sum, canopy_volumes)

yield <- read.csv("AppleYield.csv", sep =',', head=T)
tree_yield <- inner_join(tree_sum, yield)
tree_yield <- dplyr::mutate(tree_yield, 
                            TCSA = pi*(trunk_diam_cm/2)^2,
                            tot_length_m = tot_length/100,
                            tot_area_m2 = tot_area/10000,
                            tot_volume_m3 = tot_volume/1000000,
                            canopy_area = pi*(canopy_spread/2)^2,
                            tot_mass_kg = (tot_stem_m + tot_twig_m)/1000)
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

model <- aov((cum_yield/height) ~ rootstock, tree_yield)
summary_YH <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_YH  <- test$groups

model <- aov((100*cum_yield/tot_length) ~ rootstock, tree_yield)
summary_YL <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_YL  <- test$groups

model <- aov((10000*cum_yield/tot_area) ~ rootstock, tree_yield)
summary_YSA <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_YSA  <- test$groups

model <- aov((1000000*cum_yield/tot_volume) ~ rootstock, tree_yield)
summary_YSV <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_YSV  <- test$groups

model <- aov((cum_yield/canopy_spread) ~ rootstock, tree_yield)
summary_YS <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_YS  <- test$groups

model <- aov((pi*(canopy_spread/2)^2) ~ rootstock, tree_yield)
summary_canopy_area <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_canopy_area  <- test$groups

model <- aov((cum_yield/(pi*(canopy_spread/2)^2)) ~ rootstock, tree_yield)
summary_YCA <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_YCA  <- test$groups

model <- aov((cum_yield/triangles) ~ rootstock, tree_yield)
summary_YCV <- summary(model)
test  <- duncan.test(model, "rootstock")
duncan_YCV  <- test$groups

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

Y0 <- 1.83  # str_split(sma$X.Subtree..7[3], ";")[[1]][1]
a <- 1.25  # str_split(sma$X.Subtree..7[3], ";")[[1]][4]
mass <- 10^(as.numeric(Y0) +  as.numeric(a)*log10(tree_yield$TCSA))

allom_mass <- lm(mass ~ (tree_yield$tot_stem_m + tree_yield$tot_twig_m))  #0.981

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

yield_index <- dplyr::transmute(tree_yield, tree, rootstock,
                                harvest_index = cum_yield / 
                                  (tot_mass_kg + cum_yield),
                                yield_eff = cum_yield / TCSA,
                                modeled_HI = cum_yield / (mass + cum_yield),
                                yield_height = cum_yield / height,
                                yield_length = cum_yield / tot_length_m,
                                yield_stem_area = cum_yield / tot_area_m2,
                                yield_stem_volume = cum_yield / tot_volume_m3,
                                yield_spread = cum_yield / canopy_spread,
                                yield_canopy_area = cum_yield / canopy_area,
                                yield_canopy_volume = cum_yield / triangles)

yield_index_root <- dplyr::summarize(dplyr::group_by(yield_index, rootstock),
                                     avg_HI = mean(harvest_index),
                                     avg_YE = mean(yield_eff),
                                     avg_HI_a = mean(modeled_HI),
                                     avg_YH = mean(yield_height),
                                     avg_YL = mean(yield_length),
                                     avg_YSA = mean(yield_stem_area),
                                     avg_YSV = mean(yield_stem_volume),
                                     avg_YS = mean(yield_spread),
                                     avg_YCA = mean(yield_canopy_area),
                                     avg_YCV = mean(yield_canopy_volume))

test <- lm(harvest_index ~ yield_eff, data = yield_index)
test <- lm(harvest_index ~ modeled_HI, data = yield_index)
test <- lm(harvest_index ~ yield_height, data = yield_index)
test <- lm(harvest_index ~ yield_canopy_area, data = yield_index)
summary(test)

index_labels <- c("A", "B", "C", 
                  "Stem Length", "Stem Area", "Stem Volume",
                  "Canopy Spread", "D", "Canopy Volume")

index_long <- yield_index %>% 
  select(-harvest_index) %>%
  gather(index, value, -rootstock, -tree)

harvest_index <- dplyr::select(yield_index, tree, rootstock, harvest_index)
index_data <- distinct(select(index_long, index))
index_join <- cbind(index_data, index_labels) 

index_graph <- left_join(index_long, harvest_index)
index_graph <- left_join(index_graph, index_join)
index_graph$index_labels <- factor(index_graph$index_labels, levels = index_labels)
index_graph <- dplyr::filter(index_graph, 
                             index_labels == "A" | 
                               index_labels == "B" |
                               index_labels ==  "C" | 
                               index_labels == "D")

png("index_comp.png", width = 1500, height = 450)
ggplot(index_graph) +
  geom_point(aes(y=harvest_index, x=value, shape = rootstock), size = 10) +
  facet_grid(. ~ index_labels, scales="free_x") +
  labs(x = "    Yield efficiency               Modeled HI                Yield : Height        Yield : Canopy Area", 
       y = "Harvest Index") +
  theme_bw(base_size = 28, base_family = "Helvetica") +
  theme(axis.title=element_text(size=32), 
        strip.background = element_rect(color='white', fill='white')) +
  theme(panel.margin = unit(1.5, "lines"), legend.key = element_blank(),
        axis.title.x = element_text(hjust=0))
dev.off()
