### This code generates and analyses standard yield metric in hort research.

library(agricolae)
library(ggplot2)
library(stringr)
library(smatr)
library(dplyr)
library(tidyr)
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

model <- aov(tree_yield$TCSA ~ as.factor(tree_yield$rootstock))
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

model <- aov((tree_yield$cum_yield/tree_yield$TCSA) ~ as.factor(tree_yield$rootstock))
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
mass <- 10^(as.numeric(Y0) +  as.numeric(a)*log10(tree_yield$TCSA))  # est. in g

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

test <- sma((tree_yield$tot_stem_m + tree_yield$tot_twig_m) ~ tree_yield$TCSA)


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
                                modeled_HI = cum_yield / (mass/1000 + cum_yield),
                                yield_height = cum_yield / height,
                                yield_length = cum_yield / tot_length_m,
                                yield_stem_area = cum_yield / tot_area_m2,
                                yield_stem_volume = cum_yield / tot_volume_m3,
                                yield_spread = cum_yield / canopy_spread,
                                yield_canopy_area = cum_yield / canopy_area,
                                yield_canopy_volume = cum_yield / triangles)

rootstock_names <- data.frame(old_root_names = c("Bud.9", "CG.3041", "CG.6210",
                                                 "M.26", "JM.8", "PiAu.5683"),
                              new_root_names = c("B.9", "G.41", "G.210",
                                                 "M.26", "JM.8", "Pi-AU 56-83"))
tree_yield$new_root_names <- factor(tree_yield$new_root_names, 
                                     levels = c("B.9", "G.41", "G.210",
                                                "M.26", "JM.8", "Pi-AU 56-83"))

tree_yield <- left_join(tree_yield, rootstock_names, 
                        by = c("rootstock" = "old_root_names"))
yield_index <- left_join(yield_index, rootstock_names, 
                        by = c("rootstock" = "old_root_names"))
yield_index$new_root_names <- factor(yield_index$new_root_names, 
                                     levels = c("B.9", "G.41", "G.210",
                                                "M.26", "JM.8", "Pi-AU 56-83"))

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

test <- lm(yield_index$harvest_index ~ tree_yield$TCSA)
test <- lm(harvest_index ~ yield_eff, data = yield_index)
test <- lm(harvest_index ~ poly(yield_eff, 2, raw=T), data = yield_index)
test <- lm(harvest_index ~ modeled_HI, data = yield_index)
test <- lm(harvest_index ~ yield_height, data = yield_index)
test <- lm(harvest_index ~ yield_canopy_area, data = yield_index)
summary(test)

index_labels <- c("A", "B", "C", 
                  "Stem Length", "Stem Area", "Stem Volume",
                  "Canopy Spread", "D", "Canopy Volume")

index_long <- yield_index %>% 
  select(-harvest_index) %>%
  gather(index, value, -rootstock, -tree, -new_root_names)

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
  geom_point(aes(y=harvest_index, x=value, shape = new_root_names), 
             size = 10, bg="black") +
  scale_shape_manual(values=c(20:25)) +
  facet_grid(. ~ index_labels, scales="free_x") +
  labs(x = "      Yield efficiency                   Modeled HI                     Yield : Height              Yield : Canopy Area", 
       y = "Observed HI", shape = "Rootstock") +
  #geom_smooth(aes(y=harvest_index, x=value), method="lm") +
  theme_bw(base_size = 28, base_family = "Helvetica") +
  theme(axis.title=element_text(size=32), 
        strip.background = element_rect(color='white', fill='white')) +
  theme(panel.margin = unit(1.5, "lines"), legend.key = element_blank(),
        axis.title.x = element_text(hjust=0), legend.position=c(0.93, 0.31))
dev.off()

png("harvest_index.png", width = 700, height = 600)
ggplot(tree_yield) +
  geom_point(aes(y=cum_yield, x=cum_yield+tot_mass_kg, shape = new_root_names), 
             size = 10, bg="black") +
  scale_shape_manual(values=c(20:25)) +
  geom_abline(slope=1) + geom_abline(slope=0.9) + geom_abline(slope=0.8) +
  labs(x = "Total Mass [Kg]", y = "Fruit Mass [Kg]", shape = "Rootstock") +
  guides(shape=guide_legend(nrow=2)) +
  theme_classic(base_size = 28, base_family = "Helvetica") +
  theme(axis.title=element_text(size=32), 
        strip.background = element_rect(color='white', fill='white')) +
  theme(panel.margin = unit(1.5, "lines"), legend.key = element_blank(),
        legend.position=c(0.7, 0.1))
dev.off()

png("yield_efficiency.png", width = 700, height = 600)
ggplot(tree_yield) +
  geom_point(aes(y=cum_yield, x=TCSA, shape = new_root_names), 
             size = 10, bg="black") +
  scale_shape_manual(values=c(20:25)) +
  geom_abline(slope=4) + geom_abline(slope=3) + geom_abline(slope=1) +
  labs(x = "TCSA [cm2]", y = "Fruit Mass [Kg]", shape = "Rootstock") +
  guides(shape=guide_legend(nrow=2)) +
  theme_classic(base_size = 28, base_family = "Helvetica") +
  theme(axis.title=element_text(size=32), 
        strip.background = element_rect(color='white', fill='white')) +
  theme(panel.margin = unit(1.5, "lines"), legend.key = element_blank(),
        legend.position=c(0.7, 0.1))
dev.off()

png("harvest_index_color.png", width = 700, height = 600)
ggplot(tree_yield) +
  geom_point(aes(y=cum_yield, x=cum_yield+tot_mass_kg, color=new_root_names), 
             size = 12) +
  scale_color_brewer(palette="BrBG") +
  geom_abline(slope=1) + geom_abline(slope=0.9) + geom_abline(slope=0.8) +
  labs(x = "Total Mass [Kg]", y = "Fruit Mass [Kg]", color = "Rootstock") +
  guides(color=guide_legend(nrow=2)) +
  theme_classic(base_size = 28, base_family = "Helvetica") +
  theme(axis.title=element_text(size=32), 
        strip.background = element_rect(color='white', fill='white')) +
  theme(panel.margin = unit(1.5, "lines"), legend.key = element_blank(),
        legend.position=c(0.7, 0.1))
dev.off()

png("yield_efficiency_color.png", width = 700, height = 600)
ggplot(tree_yield) +
  geom_point(aes(y=cum_yield, x=TCSA, color = new_root_names), 
             size = 12, bg="black") +
  scale_color_brewer(palette="BrBG") +
  geom_abline(slope=4) + geom_abline(slope=3) + geom_abline(slope=1) +
  labs(x = "TCSA [cm2]", y = "Fruit Mass [Kg]", color="Rootstock") +
  guides(color=guide_legend(nrow=2)) +
  theme_classic(base_size = 28, base_family = "Helvetica") +
  theme(axis.title=element_text(size=32), 
        strip.background = element_rect(color='white', fill='white')) +
  theme(panel.margin = unit(1.5, "lines"), legend.key = element_blank(),
        legend.position=c(0.7, 0.1))
dev.off()

png("index_comp_color.png", width = 1500, height = 450)
ggplot(index_graph) +
  geom_point(aes(y=harvest_index, x=value, color = new_root_names), 
             size = 12) +
  scale_color_brewer(palette="BrBG") +
  facet_grid(. ~ index_labels, scales="free_x") +
  labs(x = "      Yield efficiency                   Modeled HI                     Yield : Height              Yield : Canopy Area", 
       y = "Harvest Index", color = "Rootstock") +
  #geom_smooth(aes(y=harvest_index, x=value), method="lm") +
  theme_bw(base_size = 28, base_family = "Helvetica") +
  theme(axis.title=element_text(size=32), 
        strip.background = element_rect(color='white', fill='white')) +
  theme(panel.margin = unit(1.5, "lines"), legend.key = element_blank(),
        axis.title.x = element_text(hjust=0), legend.position=c(0.93, 0.31))
dev.off()