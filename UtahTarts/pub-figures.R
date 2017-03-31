### This code generates the analysis for Utah Co tarts for publication

library(dplyr)
library(ggplot2)
source("multiplot.R")
source("block-summaries.R")

young <- filter(avg_vol, age_class=="young")
old <- filter(avg_vol, age_class=="old")
age_zero <- filter(avg_vol, !is.na(age))

# Fig 1
png("grower-age-class-pub.png", width = 1200, height = 900)
Z1 <- ggplot(age_zero, aes(x=age, y=TCSA, group=age_class)) +
  geom_smooth(method="lm", alpha=0, color='black', size=2, fullrange=T) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  ylim(NA, max(age_zero$TCSA)) +
  geom_segment(aes(x=16, y=175, xend=15.5, yend=240), size=1.25,
               arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=16, y=150, size=10, label=lm_intercept(TCSA~age)) +
  labs(x="Age", y="TCSA [cm2]", shape = "", title = "A") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
Z2 <- ggplot(age_zero, aes(x=age, y=spread, group=age_class)) +
  geom_smooth(method="lm", alpha=0, color='black', size=2, fullrange=T) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  ylim(NA, max(age_zero$spread)) +
  geom_segment(aes(x=16.25, y=425, xend=15.25, yend=475), size=1.25,
               arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=16.25, y=400, size=10, label=lm_intercept(spread~age)) +
  labs(x="Age", y="Canopy Spread [cm]", shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
Z3 <- ggplot(age_zero, aes(x=age, y=height/100, group=age_class)) +
  geom_smooth(method = "lm", alpha=0, color='black', size = 2, fullrange=T) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  ylim(NA, max(age_zero$height/100)) +
  geom_segment(aes(x=14.5, y=4.25, xend=13, yend=4.75), size=1.25,
               arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=15, y=4, size=10, label=lm_intercept(height~age)) +
  labs(x="Age", y="Height [cm]", shape="", title="B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
Z4 <- ggplot(age_zero, aes(x=age, y=volume, group=age_class)) +
  geom_smooth(method="lm", alpha=0, color='black', size=2, fullrange=T) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  ylim(NA, max(age_zero$volume)) +
  geom_segment(aes(x=16, y=30, xend=15, yend=40), size=1.25,
               arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=16.5, y=25, size=10, label=lm_intercept(volume~age)) +
  labs(x="Age", y="Canopy Volume [m3]", shape="", title="D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.075),
        legend.direction = "horizontal")
multiplot(Z1, Z2, Z3, Z4, cols=2)
dev.off()

png("grower-sugar-yield-pub.png", width = 1200, height = 500)
C1 <- ggplot(avg_vol, aes(x=TCSA, y=tree_yield_2014*0.454)) +
  geom_smooth(method="lm", fill='white', color='black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=250, y=15, size=18, 
           label = lm_eqn(tree_yield_2014~TCSA, round=1), parse = TRUE) +
  geom_abline(slope=0.5) +
  geom_abline(slope=0.25) +
  geom_abline(slope=0.125) +
  labs(x="TCSA [cm2]", y="Yield / tree [kg]", 
       shape="", title="A") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
C2 <- ggplot(avg_vol_light, aes(x=TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Sugar Content [Brix]", 
       shape="", title="B") +
  annotate("text", x=310, y=14, size=18, 
           label = lm_eqn(sugar_out~poly(TCSA, 2, raw=T), df=avg_vol_light), 
           parse = TRUE) +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(C1, C2, cols=2)
dev.off()

png("grower-sugar-byTCSA-pub.png", width = 1200, height = 900)
D1 <- ggplot(avg_vol_light, aes(y=sugar_out, x=tree_yield_2014*0.454/TCSA)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=0.55, y=9.5, size=18, 
           label = lm_eqn(sugar_out~tree_yield_2014/TCSA, df=avg_vol_light), 
           parse = TRUE) +
  labs(y="Sugar Content [Brix]", x="Crop Load [kg / cm2]", 
       shape="", title="A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
D2 <- ggplot(avg_vol_light, aes(x=height/TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=4.5, y=9.75, size=18, 
           label = lm_eqn(sugar_out~height/TCSA, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Height : TCSA", y="Sugar Content [Brix]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
D3 <- ggplot(avg_vol_light, aes(x=spread/TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=3.2, y=9.5, size=18, 
           label = lm_eqn(sugar_out~spread/TCSA, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Canopy Spread : TCSA", y="Sugar Content [Brix]", 
       shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
D4 <- ggplot(avg_vol_light, aes(x=volume/TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume : TCSA", y="Sugar Content [Brix]", 
       shape = "", title = "D") +
  annotate("text", x=0.150, y=12.5, size=18, 
           label = lm_eqn(sugar_out~volume/TCSA, df=avg_vol_light), 
           parse = TRUE) +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.3,0.1),
        legend.direction = "horizontal")
multiplot(D1, D3, D2, D4, cols=2)
dev.off()