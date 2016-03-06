### cursory analysis of implications of selecting only low light values 
### (full_sun < 1000) for estimation of light environment

library(dplyr)
library(ggplot2)
source("multiplot.R")

avg_light <- read.csv("tree-averages-light.csv")
tree_volumes <- read.csv('canopy-size.csv')

scaffold <- read.csv('scaffold.csv')
block_id <- distinct(select(scaffold, grower, block, id, light_id))
block_code <- mutate(block_id, block_code = paste(grower, 0, block, sep=''))
block_code[37, ]$block_code <- 'SS10'
block_code[38, ]$block_code <- 'SS12'
avg_l_block_code <- inner_join(avg_light, block_code, by = c('block' = 'light_id'))
avg_vol_light_tree <- inner_join(avg_l_block_code, tree_volumes, 
                                 by = c('block_code' = 'block', 'tree'))

grower_conv <- data.frame(grower=levels(avg_vol_light_tree$grower), 
                          grower.x=as.factor(seq_along(levels(avg_vol_light_tree$grower))))
avg_vol_light_tree <- left_join(avg_vol_light_tree, grower_conv)

png("light.png", width = 1200, height = 900)
A1 <- ggplot(avg_vol_light_tree, aes(x=(1-low_light), y=avg_sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=0.24, y=8.2, size=18, 
           label=lm_eqn(avg_sugar_out~low_light, df=avg_vol_light_tree), parse = TRUE) +
  labs(x="Light [% full sun]", y="Sugar Content [Brix]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A2 <- ggplot(avg_vol_light_tree, aes(x=height, y=(1-low_light))) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  scale_x_continuous(limits=c(300,600)) +
  annotate("text", x=400, y=0.14, size=18, 
           label=lm_eqn(low_light~height, df=avg_vol_light_tree), parse = TRUE) +
  labs(x="Height [cm]", y="Light [% full sun]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A3 <- ggplot(avg_vol_light_tree, aes(x=spread, y=(1-low_light))) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  scale_x_continuous(limits=c(200,600)) +
  annotate("text", x=310, y=0.14, size=18, 
           label=lm_eqn(low_light~spread, df=avg_vol_light_tree), parse = TRUE) +
  labs(x="Canopy Spread [cm]", y="Light [% full sun]", 
       shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A4 <- ggplot(avg_vol_light_tree, aes(x=frustum, y=(1-low_light))) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  scale_x_continuous(limits=c(0,80)) +
  annotate("text", x=60, y=0.14, size=18, 
           label=lm_eqn(low_light~frustum, df=avg_vol_light_tree), parse = TRUE) +
  labs(x="Volume [m3]", y="Light [% full sun]", 
       shape = "", title = "D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.9),
        legend.direction = "horizontal")
multiplot(A1, A3, A2, A4, cols=2)
dev.off()
