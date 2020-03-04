### This script generates the allometries reported in Table 1.

library(dplyr)
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
rootstock_names <- data.frame(old_root_names = c("Bud.9", "CG.3041", "CG.6210",
                                                 "M.26", "JM.8", "PiAu.5683"),
                              new_root_names = c("B.9", "G.41", "G.210",
                                                 "M.26", "JM.8", "Pi-AU 56-83"))
tree_yield <- left_join(tree_yield, rootstock_names, 
                        by = c("rootstock" = "old_root_names"))
tree_yield$new_root_names <- factor(tree_yield$new_root_names, 
                                     levels = c("B.9", "G.41", "G.210",
                                                "M.26", "JM.8", "Pi-AU 56-83"))
sma <- read.csv("SMAResults.csv")

source("../UtahTarts/multiplot.R")

### Individual Level

test <- lm((tot_stem_m + tot_twig_m) ~ poly(TCSA, 2), data = tree_yield)
test <- sma((tot_stem_m + tot_twig_m) ~ TCSA, data = tree_yield)
test <- sma(log10((tot_stem_m + tot_twig_m)) ~ log10(TCSA), data = tree_yield)
test <- sma(log10(height) ~ log10(TCSA), data = tree_yield)
test <- sma(log10((tot_stem_m + tot_twig_m)) ~ log10(height), data = tree_yield)
test <- sma(log10((tot_volume)) ~ log10(TCSA), data = tree_yield)
test <- sma(log10((tot_stem_m + tot_twig_m)) ~ log10(tot_volume), data = tree_yield)
test <- sma(log10(cum_yield) ~ log10(TCSA), data = tree_yield)


test <- sma(log10(tot_stem_m + tot_twig_m) ~ log10(pi*(canopy_spread/2)^2), data = tree_yield)
test <- sma(log10(tot_stem_m + tot_twig_m) ~ log10(canopy_volume), data = tree_yield)
test <- sma(log10(TCSA) ~ log10(pi*(canopy_spread/2)^2), data = tree_yield)
test <- sma(log10(TCSA) ~ log10(canopy_volume), data = tree_yield)
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

#### New Fig 1

tcsa_lab <- expression("TCSA [cm"^2*"]")
shape_list <- c(0,1,5,12,10,9)

png("allometry-compare.png", width = 1000, height = 450)  # FIG 1
a1 <- ggplot(tree_yield, aes(x=TCSA, y=tot_mass_kg)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=new_root_names), size=10, stroke = 2) +
  scale_shape_manual(values=shape_list) +
  labs(x=tcsa_lab, y="Stem Biomass [kg]", 
       shape = "Rootstock", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

a2 <- ggplot(tree_yield, aes(x = log10(TCSA), y = log10(tot_mass_kg))) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=new_root_names), size=10, stroke = 2) +
  scale_shape_manual(values=shape_list) +
  labs(x = "Log( TCSA )", y = "Log( Stem Biomass )",
       shape = "Rootstock", title="B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  guides(shape=guide_legend(ncol=2)) +
  theme(axis.title=element_text(size=36), 
        legend.justification=c(1,0), legend.position=c(1, 0))

multiplot(a1, a2, cols=2)
dev.off()

#### Old Fig 1
png("allometries.png", width = 1500, height = 450)  # FIG 1
a1 <- ggplot(tree_yield, aes(x=log10(TCSA), y=log10(tot_mass_kg))) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=new_root_names), size=10, bg="black") +
  scale_shape_manual(values=c(20:25)) +
  labs(x="Log( TCSA )", y="Log( Stem Biomass )", 
       shape = "Rootstock", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

a2 <- ggplot(tree_yield, aes(x = log10(tot_mass_kg), y = log10(cum_yield))) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=new_root_names), size=10, bg="black") +
  scale_shape_manual(values=c(20:25)) +
  labs(x = "Log( Stem Biomass )", y = "Log( Cumulative Yield )",
       shape = "Rootstock", title="B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

YE <- function(x) {log10(3) + x}  # simplified from log10(4 * 10^x)
a3 <- ggplot(tree_yield, aes(x = log10(TCSA), y = log10(cum_yield))) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=new_root_names), size=10, bg="black") +
  scale_shape_manual(values=c(20:25)) +
  labs(x = "Log( TCSA )", y = "Log( Cumulative Yield )",
       shape = "Rootstock", title="C") +
  stat_function(size=2, linetype=2, fun=YE) +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  guides(shape=guide_legend(ncol=2)) +
  theme(axis.title=element_text(size=36), 
        legend.justification=c(1,0), legend.position=c(1, 0))
multiplot(a1, a2, a3, cols=3)
dev.off()

a4 <- ggplot(tree_yield, aes(x = (tot_mass_kg + yield), 
                             y = yield)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape = rootstock), size = 8) +
  labs(x = "Total Stem Biomass & Yield", y = "Yield",
       title="A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

a5 <- ggplot(tree_yield, aes(x = (tot_mass_kg + cum_yield), 
                             y = cum_yield)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape = rootstock), size = 8) +
  labs(x = "Total Stem Biomass & Cumulative Yield", y = "Cumulative Yield",
       title="B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

a6 <- ggplot(tree_yield, aes(x = cum_yield / (tot_mass_kg + cum_yield), 
                             y = TCSA)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape = rootstock), size = 8) +
  labs(x = "Harvest Index", y = "TCSA",
       title="C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

a7 <- ggplot(tree_yield, aes(y = cum_yield/canopy_spread, 
                             x = canopy_spread)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape = rootstock), size = 8) +
  labs(x = "Canopy Spread", y = "Yield:Canopy Spread",
       title="C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot() +
  geom_point(aes(x=log10(TCSA), y=log10(tree_yield$height), 
                 col=tree_yield$rootstock))

ggplot() +
  geom_point(aes(x=log10(tree_yield$tot_volume), 
                 y=log10((tree_yield$tot_stem_m + tree_yield$tot_twig_m)), 
                 col=tree_yield$rootstock))