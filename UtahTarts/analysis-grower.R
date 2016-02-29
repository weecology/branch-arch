### This code generates the analysis for Utah Co tarts at the GROWER level
library(dplyr)
library(ggplot2)
source("multiplot.R")

tree_averages <- read.csv('tree-averages-all.csv')
tree_averages_light <- read.csv('tree-averages-light.csv')
tree_volumes <- read.csv('canopy-size.csv')
block_info <- read.csv('block.csv')

scaffold <- read.csv('scaffold.csv')
block_id <- distinct(select(scaffold, grower, block, id, light_id))
block_code <- mutate(block_id, block_code = paste(grower, 0, block, sep=''))
block_code[37, ]$block_code <- 'SS10'
block_code[38, ]$block_code <- 'SS12'

avg_block_code <- inner_join(tree_averages, block_code, by = c('block' = 'id'))
avg_vol_tree <- inner_join(avg_block_code, tree_volumes, 
                           by = c('block_code' = 'block', 'tree'))
avg_vol <- summarize(group_by(avg_vol_tree, block),
                     block_code = unique(block_code),
                     grower = unique(grower),
                     TCSA = mean(TCSA_cm2),
                     cum_BCSA = mean(cum_BCSA),
                     no_scaffolds = mean(no_scaffolds),
                     scaffold_l = mean(avg_scaffold_l),
                     scaffold_l_sd = mean(sd_scaffold_l),
                     scaffold_d = mean(avg_scaffold_d),
                     scaffold_d_sd = mean(sd_scaffold_d),
                     angles = mean(avg_angle),
                     angles_sd = mean(sd_angle),
                     height = mean(height),
                     spread = mean(spread),
                     volume = mean(frustum),
                     top_size = mean(top_frust),
                     top_cone = mean(top_cone))

avg_vol <- inner_join(avg_vol, block_info, by = 'block_code')

tree_light_sugar <- read.csv('light-sugar.csv')
avg_l_block_code <- inner_join(tree_averages_light, block_code, by = c('block' = 'light_id'))
avg_vol_light_tree <- inner_join(avg_l_block_code, tree_volumes, 
                                 by = c('block_code' = 'block', 'tree'))

avg_vol_light <- summarize(group_by(avg_vol_light_tree, block),
                           block_code = unique(block_code),
                           grower = unique(grower),
                           TCSA = mean(TCSA_cm2),
                           no_scaffolds = mean(no_scaffolds),
                           scaffold_l = mean(avg_scaffold_l),
                           scaffold_l_sd = mean(sd_scaffold_l),
                           scaffold_d = mean(avg_scaffold_d),
                           scaffold_d_sd = mean(sd_scaffold_d),
                           angles = mean(avg_angle),
                           angles_sd = mean(sd_angle),
                           height = mean(height),
                           spread = mean(spread),
                           volume = mean(frustum),
                           top_size = mean(top_frust),
                           top_cone = mean(top_cone),
                           sugar = mean(avg_sugar),
                           sugar_out = mean(avg_sugar_out),
                           sugar_diff = mean(sugar_diff),
                           absorbed = mean(avg_absorbed),
                           extinction = mean(avg_extinction))

avg_vol_light_tree_alt <- left_join(avg_vol_light_tree, tree_light_sugar,
                                    by = c('grower', 'block.y' = 'block', 'tree'))
avg_vol_light_alt <- summarize(group_by(avg_vol_light_tree_alt, block),
                               sugar_alt = mean(avg_sugar.y),
                               light_alt = mean(avg_light))

avg_vol_light <- inner_join(avg_vol_light, avg_vol_light_alt)
avg_vol_light <- inner_join(avg_vol_light, block_info, by = 'block_code')

smalls <- filter(avg_vol, TCSA < 200)
bigs <- filter(avg_vol, TCSA > 200)
scaffolds <- filter(scaffold, scaffold!=0)

#Visualize
png("grower-architecture.png", width = 600, height = 1200) 
a1 <- ggplot(avg_vol, aes(x=TCSA, y=cum_BCSA)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="TCSA [cm2]", y="BCSA [cm2]", 
       color = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a2 <- ggplot(avg_vol, aes(x=TCSA, y=scaffold_l)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="TCSA [cm2]", y="Scaffold Length [cm]", 
       color = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a3 <- ggplot(avg_vol, aes(x=scaffold_d, y=scaffold_l)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="Scaffold Diameter [cm]", y="Scaffold Length [cm]", 
       color = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(a1, a2, a3, cols=1)
dev.off()

png("grower-canopy.png", width = 600, height = 1200) 
a4 <- ggplot(avg_vol, aes(x=TCSA, y=height)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="TCSA [cm2]", y="Height [cm]", 
       color = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a5 <- ggplot(avg_vol, aes(x=TCSA, y=spread)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="TCSA [cm2]", y="Canopy Spread [cm]", 
       color = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a6 <- ggplot(avg_vol, aes(x=TCSA, y=volume)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="TCSA [cm2]", y="Canopy Volume [m3]", 
       color = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(a4, a5, a6, cols=1)
dev.off()

png("grower-sugar.png", width = 1200, height = 900) 
a7 <- ggplot(avg_vol_light, aes(x=TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="TCSA [cm2]", y="Sugar Content [Brix]", 
       color = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a8 <- ggplot(avg_vol_light, aes(x=height, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="Height [cm]", y="Sugar Content [Brix]", 
       color = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.9),
        legend.direction = "horizontal")
a9 <- ggplot(avg_vol_light, aes(x=spread, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="Canopy Spread [cm]", y="Sugar Content [Brix]", 
       color = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a10 <- ggplot(avg_vol_light, aes(x=volume, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="Canopy Volume [m3]", y="Sugar Content [Brix]", 
       color = "", title = "D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(a7, a9, a8, a10, cols=2)
dev.off()

#Visualize
png("grower-sugar-byTCSA.png", width = 600, height = 900) 
a11 <- ggplot(avg_vol_light, aes(x=height/TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="Height : TCSA", y="Sugar Content [Brix]", 
       color = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a12 <- ggplot(avg_vol_light, aes(x=spread/TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(color=grower.x), size=10) +
  labs(x="Canopy Spread : TCSA", y="Sugar Content [Brix]", 
       color = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(a11, a12, cols=1)
dev.off()
