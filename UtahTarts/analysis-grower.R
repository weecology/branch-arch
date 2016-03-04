### This code generates the analysis for Utah Co tarts at the GROWER level

library(dplyr)
library(ggplot2)
source("multiplot.R")
source("block-summaries.R")

age_zero <- filter(avg_vol, !is.na(age))

png("grower-age.png", width = 1200, height = 900)
A1 <- ggplot(avg_vol, aes(x=age, y=TCSA)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=25, y=100, size=18, 
           label=lm_eqn(TCSA~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="TCSA [cm2]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A2 <- ggplot(avg_vol, aes(x=age, y=spread)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=25, y=270, size=18, 
           label=lm_eqn(spread~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Canopy Spread [cm]", 
       shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A3 <- ggplot(avg_vol, aes(x=age, y=height)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=25, y=355, size=18, 
           label=lm_eqn(height~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Height [cm]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A4 <- ggplot(avg_vol, aes(x=age, y=volume)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=25, y=18, size=18, 
           label=lm_eqn(volume~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Canopy Volume [m3]", 
       shape = "", title = "D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(A1, A2, A3, A4, cols=2)
dev.off()

png("grower-canopy.png", width = 600, height = 1200)
B1 <- ggplot(avg_vol, aes(x=TCSA, y=height)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Height [cm]", 
       shape = "", title = "A") +
  annotate("text", x=350, y=315, size=18, 
           label = lm_eqn(height~poly(TCSA, 2, raw=T)), parse = TRUE) +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
B2 <- ggplot(avg_vol, aes(x=TCSA, y=spread)) +
  geom_smooth(method="lm", formula = y ~ poly(x, 2),
              fill='white', color='black', size=2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=350, y=225, size=18, 
           label = lm_eqn(spread~poly(TCSA, 2, raw=T)), parse = TRUE) +
  labs(x="TCSA [cm2]", y="Canopy Spread [cm]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
B3 <- ggplot(avg_vol, aes(x=TCSA, y=volume)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Canopy Volume [m3]", 
       shape = "", title = "C") +
  annotate("text", x=350, y=10, size=18, 
           label = lm_eqn(volume~poly(TCSA, 2, raw=T)), parse = TRUE) +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(B1, B2, B3, cols=1)
dev.off()

png("grower-sugar-yield.png", width = 1200, height = 900)
C1 <- ggplot(avg_vol, aes(x=TCSA, y=tree_yield_2014)) +
  geom_smooth(method="lm", fill='white', color='black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=250, y=20, size=18, 
           label = lm_eqn(tree_yield_2014~TCSA), parse = TRUE) +
  geom_abline(slope=1) +
  geom_abline(slope=0.5) +
  geom_abline(slope=0.25) +
  labs(x="TCSA [cm2]", y="Yield / tree [lbs]", 
       shape="", title="A") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
C2 <- ggplot(avg_vol_light, aes(x=TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Sugar Content [Brix]", 
       shape="", title="B") +
  annotate("text", x=310, y=14, size=18, 
           label = lm_eqn(sugar_out~TCSA, df=avg_vol_light), parse = TRUE) +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
C3 <- ggplot(avg_vol, aes(x=TCSA/age, y=tree_yield_2014)) +
  geom_smooth(method = "lm", fill='white', color='black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=17, y=25, size=18, 
           label = lm_eqn(tree_yield_2014~TCSA/age), parse = TRUE) +
  labs(x="TCSA / Age", y="Yield / tree [lbs]", 
       shape="", title="C") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
C4 <- ggplot(avg_vol_light, aes(x=TCSA/age, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=20, y=14.5, size=18, 
           label = lm_eqn(sugar_out~TCSA/age, df=avg_vol_light), parse = TRUE) +
  labs(x="TCSA / Age", y="Sugar Content [Brix]", 
       shape="", title="D") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(C1, C3, C2, C4, cols=2)
dev.off()

png("grower-sugar-byTCSA.png", width = 1200, height = 900)
D1 <- ggplot(avg_vol_light, aes(y=sugar_out, x=tree_yield_2014/TCSA)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=1.3, y=9.5, size=18, 
           label = lm_eqn(sugar_out~tree_yield_2014/TCSA, df=avg_vol_light), 
           parse = TRUE) +
  labs(y="Sugar Content [Brix]", x="Yield Efficiency [lbs / cm2]", 
       shape="", title="A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
D2 <- ggplot(avg_vol_light, aes(x=height/TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
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
  geom_point(aes(shape=grower.x), size=10, bg="black") +
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
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume : TCSA", y="Sugar Content [Brix]", 
       shape = "", title = "C") +
  annotate("text", x=0.150, y=12.5, size=18, 
           label = lm_eqn(sugar_out~volume/TCSA, df=avg_vol_light), 
           parse = TRUE) +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.3,0.1),
        legend.direction = "horizontal")
multiplot(D1, D3, D2, D4, cols=2)
dev.off()



### Appendix
png("grower-architecture.png", width = 600, height = 900) 
a1 <- ggplot(avg_vol, aes(x=TCSA, y=cum_BCSA)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=350, y=100, size=18, 
           label = lm_eqn(cum_BCSA~TCSA), parse = TRUE) +
  labs(x="TCSA [cm2]", y="BCSA [cm2]", 
       shape="", title="A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a2 <- ggplot(avg_vol, aes(y=height, x=scaffold_l*sin(angles*pi/180))) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=400, y=400, size=18, 
           label = lm_eqn(height~scaffold_l*sin(angles*pi/180)), parse = TRUE) +
  labs(y="Height [cm]", x="Scaffold Length * Angle", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(a1, a2, cols=1)
dev.off()

png("grower-yield.png", width = 1200, height = 900) 
b1 <- ggplot(avg_vol, aes(x=TCSA, y=tree_yield_2014, shape=grower.x)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  geom_abline(slope=1) +
  geom_abline(slope=0.5) +
  geom_abline(slope=0.25) +
  labs(x="TCSA [cm2]", y="Yield / tree [lbs]", 
       shape="", title="A") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
b2 <- ggplot(avg_vol, aes(x=volume, y=tree_yield_2014, shape=grower.x)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume [m3]", y="Yield / tree [lbs]", 
       shape="", title="B") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
b3 <- ggplot(avg_vol, aes(x=tree_acre, y=tree_yield_2014, shape=grower.x)) +
  geom_smooth(method = "lm", fill='grey', color = 'black', size = 2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  scale_x_continuous(limits = c(125, 210)) +
  labs(x="Trees / Acre", y="Yield / Tree [lbs]", 
       shape="", title="C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
b4 <- ggplot(avg_vol, aes(x=volume/TCSA, y=tree_yield_2014, shape=grower.x)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume / TCSA", y="Yield / tree [lbs]", 
       shape="", title="D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(b1, b3, b2, b4, cols=2)
dev.off()

png("grower-sugar.png", width = 1200, height = 900) 
c1 <- ggplot(avg_vol_light, aes(x=TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=125, y=9, size=18, 
           label = lm_eqn(sugar_out~TCSA, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="TCSA [cm2]", y="Sugar Content [Brix]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
c2 <- ggplot(avg_vol_light, aes(x=height, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=375, y=9.75, size=18, 
           label = lm_eqn(sugar_out~height, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Height [cm]", y="Sugar Content [Brix]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.9),
        legend.direction = "horizontal")
c3 <- ggplot(avg_vol_light, aes(x=spread, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=300, y=9.5, size=18, 
           label = lm_eqn(sugar_out~spread, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Canopy Spread [cm]", y="Sugar Content [Brix]", 
       color = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
c4 <- ggplot(avg_vol_light, aes(x=volume, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=15, y=9.5, size=18, 
           label = lm_eqn(sugar_out~volume, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Canopy Volume [m3]", y="Sugar Content [Brix]", 
       shape = "", title = "D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(c1, c3, c2, c4, cols=2)
dev.off()



### Extra

ggplot(avg_vol, aes(x=scaffold_d, y=scaffold_l)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="scaffold_d", y="Scaffold Length [cm]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol_light, aes(x=volume*tree_acre, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume / Acre", y="Sugar Content [Brix]", 
       shape = "", title = "D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol, aes(x=volume/TCSA, y=tree_yield_2014, shape=grower.x)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume / TCSA", y="Yield / tree [lbs]", 
       shape="", title="B") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol, aes(x=mass, y=tree_yield_2014, shape=grower.x)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="mass", y="Yield / tree [lbs]", 
       shape="", title="B") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol, aes(x=mass+tree_yield_2014, y=tree_yield_2014, shape=grower.x)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Total Mass", y="Fruit Mass", 
       shape="", title="B") +
  geom_abline(slope=1) +
  geom_abline(slope=0.5) +
  geom_abline(slope=0.25) +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol, aes(x=TCSA/age, y=tree_yield_2014, shape=grower.x)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA / Age", y="Yield / tree [lbs]", 
       shape="", title="A") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol_light, aes(x=sugar_out, y=tree_yield_2014/volume)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Sugar Content [Brix]", y="Yield / tree / Canopy Volume", 
       shape="", title="D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.4,0.1),
        legend.direction = "horizontal")

ggplot(avg_vol_light, aes(x=absorbed_low, y=sugar_out)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=0.7, y=8.5, size=18, 
           label = lm_eqn(sugar_out~absorbed_low, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Light", y="Sugar Content [Brix]", 
       shape="", title="D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.4,0.1),
        legend.direction = "horizontal")

ggplot(avg_vol_light, aes(x=volume, y=absorbed_low)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=20, y=0.85, size=18, 
           label = lm_eqn(absorbed_low~volume/TCSA, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Volume", y="Light", shape="", title="D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.4,0.1),
        legend.direction = "horizontal")