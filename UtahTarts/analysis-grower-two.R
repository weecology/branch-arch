### This code generates the analysis for Utah Co tarts between two growers
### Grower 5 - low fertility; Grower 3 - high fertility

library(dplyr)
library(ggplot2)
source("multiplot.R")
source("block-summaries.R")

two_growers_a <- filter(avg_vol, grower==2 | grower==5)
two_a_young <- filter(two_growers_a, age_class=="young")
two_growers_b <- filter(avg_vol, grower==3 | grower==5)
two_b_young <- filter(two_growers_b, age_class=="young")

png("two-growers-age.png", width = 650, height = 400)
A1 <- ggplot(two_growers_b, aes(x=age, y=TCSA)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(fill=grower.x), shape=21, size=12, stroke=3) +
  scale_fill_manual(values = c("#810F7C","#EDF8FB")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  annotate("text", x=17, y=50, size=18, 
           label=lm_eqn(TCSA~poly(age, 2, raw=T), df=two_growers_b), parse = TRUE) +
  labs(x="Age", y="TCSA [cm2]", fill = "Grower") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="right")
A1
dev.off()

png("two-growers-canopy-yield.png", width = 1200, height = 900)
A1 <- ggplot(two_growers_b, aes(x=TCSA, y=spread, group=grower)) +
  geom_smooth(method="lm", formula = y ~ poly(x, 2),
              fill='grey', color='black', size=2) +
  geom_point(aes(fill=grower), shape=21, size=12, stroke=3) +
  scale_fill_manual(values = c("#810F7C","#EDF8FB")) +
# annotate("text", x=350, y=225, size=18, 
#          label = lm_eqn(spread~poly(TCSA, 2, raw=T)), 
#          df=two_growers_b, parse = TRUE) +
  labs(x="TCSA [cm2]", y="Canopy Spread [cm]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A2 <- ggplot(two_growers_b, aes(x=tree_hect, y=spread, group=grower)) +
  geom_smooth(method="lm", formula = y ~ poly(x, 2),
              fill='grey', color='black', size=2) +
  geom_point(aes(fill=grower), shape=21, size=12, stroke=3) +
  scale_fill_manual(values = c("#810F7C","#EDF8FB")) +
# annotate("text", x=350, y=225, size=18, 
#          label = lm_eqn(spread~poly(TCSA, 2, raw=T)), 
#          df=two_growers_b, parse = TRUE) +
  labs(x="Trees / Hectare", y="Canopy Spread [cm]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A3 <- ggplot(two_growers_b, aes(x=TCSA, y=tree_yield_2014*0.454, group=grower)) +
  geom_smooth(method="lm", formula = y ~ poly(x, 2),
              fill='grey', color='black', size=2) +
  geom_point(aes(fill=grower), shape=21, size=12, stroke=3) +
  scale_fill_manual(values = c("#810F7C","#EDF8FB")) +
# annotate("text", x=350, y=225, size=18, 
#          label = lm_eqn(tree_yield_2014~poly(TCSA, 2, raw=T)), 
#          df=two_growers_b, parse = TRUE) +
  labs(x="TCSA [cm2]", y="Yield / Tree [kg]", 
       shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A4 <- ggplot(two_growers_b, aes(x=TCSA, y=tree_yield_2014*tree_hect*0.454, group=grower)) +
  geom_smooth(method="lm", formula = y ~ poly(x, 2),
              fill='grey', color='black', size=2) +
  geom_point(aes(fill=grower), shape=21, size=12, stroke=3) +
  scale_fill_manual(values = c("#810F7C","#EDF8FB")) +
# annotate("text", x=350, y=225, size=18, 
#          label = lm_eqn(tree_yield_2014*tree_hect~poly(TCSA, 2, raw=T)), 
#          df=two_growers_b, parse = TRUE) +
  labs(x="TCSA [cm2]", y="Yield / Hectare [kg]", 
       shape = "", title = "D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(A1, A3, A2, A4, cols=2)
dev.off()