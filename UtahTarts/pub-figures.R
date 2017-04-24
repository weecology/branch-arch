### This code generates the analysis for Utah Co tarts for publication

library(dplyr)
library(ggplot2)
source("multiplot.R")
source("block-summaries.R")

young <- filter(avg_vol, age_class=="young")
old <- filter(avg_vol, age_class=="old")
age_zero <- filter(avg_vol, !is.na(age))

# Fig 1
pdf("Fig1.pdf", width=6, height=5)
ggplot(age_zero, aes(x=age, y=TCSA, group=age_class)) +
  geom_smooth(method="lm", fill=NA, color='black', size=1.5, fullrange=T) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  ylim(NA, max(age_zero$TCSA)) +
  geom_segment(aes(x=16, y=175, xend=15.5, yend=240), size=1,
               arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=16, y=150, size=8, label=lm_intercept(TCSA~age)) +
  labs(x="Age", y="TCSA [cm2]", shape = "") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
dev.off()

# Fig 2
pdf("Fig2.pdf", width=10, height=5)
A1 <- ggplot(age_zero, aes(x=age, y=height/100, group=age_class)) +
  geom_smooth(method = "lm", fill=NA, color='black', size=1.5, fullrange=T) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  ylim(3, 6.5) +
  scale_shape_manual(values=c(21:25)) +
  geom_segment(aes(x=14.5, y=4.25, xend=13, yend=4.75), size=1,
               arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=15, y=4, size=8, 
           label=round(lm_intercept(height~age), 1)) +
  annotate("text", x=13, y=3.25, size=8, 
           label=lm_eqn(height/100~age, df=young), parse = T) +
  annotate("text", x=25, y=3.5, size=8, 
           label=lm_eqn(height~age, df=old), parse = T) +
  labs(x="Age", y="Height [m]", shape = "", title = "A") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B1 <- ggplot(avg_vol, aes(x=TCSA, y=height/100)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Height [m]", shape = "", title = "B") +
  annotate("text", x=350, y=3.25, size=8, 
           label = lm_eqn(height~poly(TCSA, 2, raw=T)), parse = TRUE) +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
multiplot(A1, B1, cols=2)
dev.off()

# Fig 3
pdf("Fig3.pdf", width=6, height=5)
ggplot(avg_vol, aes(x=age, y=spread/(30.48*spacing_x))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=25, y=0.6, size=8, 
           label=lm_eqn(spread/(30.48*spacing_x)~poly(age, 2, raw=T), 
                        df=age_zero), parse = TRUE) +
  labs(x="Age", y="In-row Space Filling", shape = "") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
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