### This code generates the analysis for Utah Co tarts at the GROWER level

library(dplyr)
library(ggplot2)
source("multiplot.R")
source("block-summaries.R")

age_zero <- filter(avg_vol, !is.na(age))

pdf("age-tcsa.pdf", width = 6, height = 5)
ggplot(avg_vol, aes(x=age, y=TCSA)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color='black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=23, y=100, size=10, 
           label=lm_eqn(TCSA~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="TCSA [cm2]", 
       shape = "", title = "A") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
dev.off()

pdf("size-age-tcsa.pdf", width = 10, height = 12)
A1 <- ggplot(avg_vol, aes(x=age, y=height)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=23, y=355, size=10, 
           label=lm_eqn(height~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Height [cm]", 
       shape = "", title = "A") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
A2 <- ggplot(avg_vol, aes(x=age, y=spread)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=23, y=270, size=10, 
           label=lm_eqn(spread~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Canopy Spread [cm/tree]", 
       shape = "", title = "C") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
A3 <- ggplot(avg_vol, aes(x=age, y=volume)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=23, y=5, size=10, 
           label=lm_eqn(volume~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Canopy Volume [m3/tree]", 
       shape = "", title = "E") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B1 <- ggplot(avg_vol, aes(x=TCSA, y=height)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Height [cm]", 
       shape = "", title = "B") +
  annotate("text", x=350, y=315, size=10, 
           label = lm_eqn(height~poly(TCSA, 2, raw=T)), parse = TRUE) +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B2 <- ggplot(avg_vol, aes(x=TCSA, y=spread)) +
  geom_smooth(method="lm", formula = y ~ poly(x, 2),
              fill='white', color='black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=350, y=225, size=10, 
           label = lm_eqn(spread~poly(TCSA, 2, raw=T)), parse = TRUE) +
  labs(x="TCSA [cm2]", y="Canopy Spread [cm/tree]", 
       shape = "", title = "D") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B3 <- ggplot(avg_vol, aes(x=TCSA, y=volume)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Canopy Volume [m3/tree]", 
       shape = "", title = "F") +
  annotate("text", x=350, y=5, size=10, 
           label = lm_eqn(volume~poly(TCSA, 2, raw=T)), parse = TRUE) +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
multiplot(A1, A2, A3, B1, B2, B3, cols=2)
dev.off()

pdf("area-age-tcsa.pdf", width = 10, height = 8)
A2 <- ggplot(avg_vol, aes(x=age, y=pi*(spread/200)^2*tree_hect)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=23, y=2500, size=10, 
           label=lm_eqn(tree_hect*spread^2~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Canopy Area [m2/ha]", 
       shape = "", title = "A") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
A3 <- ggplot(avg_vol, aes(x=age, y=volume*tree_hect)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=23, y=1000, size=10, 
           label=lm_eqn(tree_hect*volume~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Canopy Volume [m3/ha]", 
       shape = "", title = "C") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B2 <- ggplot(avg_vol, aes(x=TCSA, y=pi*(spread/200)^2*tree_hect)) +
  geom_smooth(method="lm", formula = y ~ poly(x, 2),
              fill='white', color='black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=350, y=2500, size=10, 
           label = lm_eqn(tree_hect*spread^2~poly(TCSA, 2, raw=T)), parse = TRUE) +
  labs(x="TCSA [cm2]", y="Canopy Area [m2/ha]", 
       shape = "", title = "B") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B3 <- ggplot(avg_vol, aes(x=TCSA, y=volume*tree_hect)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Canopy Volume [m3/ha]", 
       shape = "", title = "D") +
  annotate("text", x=350, y=1000, size=10, 
           label = lm_eqn(tree_hect*volume~poly(TCSA, 2, raw=T)), parse = TRUE) +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
multiplot(A2, A3, B2, B3, cols=2)
dev.off()

pdf("depth-age-tcsa.pdf", width = 10, height = 8)
A1 <- ggplot(avg_vol, aes(x=age, y=depth_trunk)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=23, y=200, size=10, 
           label=lm_eqn(depth_trunk~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Max Canopy Depth [cm]", 
       shape = "", title = "A") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
A2 <- ggplot(avg_vol, aes(x=age, y=depth_mid)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=23, y=200, size=10, 
           label=lm_eqn(depth_mid~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Mid Canopy Depth [cm]", 
       shape = "", title = "C") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B1 <- ggplot(avg_vol, aes(x=TCSA, y=depth_trunk)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Max Canopy Depth [cm]", 
       shape = "", title = "B") +
  annotate("text", x=350, y=200, size=10, 
           label = lm_eqn(depth_trunk~poly(TCSA, 2, raw=T)), parse = TRUE) +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B2 <- ggplot(avg_vol, aes(x=TCSA, y=depth_mid)) +
  geom_smooth(method="lm", formula = y ~ poly(x, 2),
              fill='white', color='black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=350, y=200, size=10, 
           label = lm_eqn(depth_mid~poly(TCSA, 2, raw=T)), parse = TRUE) +
  labs(x="TCSA [cm2]", y="Mid Canopy Depth [cm]", 
       shape = "", title = "D") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
multiplot(A1, A2, B1, B2, cols=2)
dev.off()

pdf("shape-age-tcsa.pdf", width = 10, height = 8)
A1 <- ggplot(avg_vol, aes(x=age, y=top_size)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=23, y=25, size=10, 
           label=lm_eqn(top_size~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="'Top' Canopy Portion [%]", 
       shape = "", title = "A") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
A2 <- ggplot(avg_vol, aes(x=age, y=depth_mid/depth_trunk*100)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=10, y=80, size=10, 
           label=lm_eqn(depth_mid/depth_trunk~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Canopy Depth Transition [%]", 
       shape = "", title = "C") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B1 <- ggplot(avg_vol, aes(x=TCSA, y=top_size)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="'Top' Canopy Portion [%]", 
       shape = "", title = "B") +
  annotate("text", x=350, y=25, size=10, 
           label = lm_eqn(top_size~poly(TCSA, 2, raw=T)), parse = TRUE) +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B2 <- ggplot(avg_vol, aes(x=TCSA, y=depth_mid/depth_trunk*100)) +
  geom_smooth(method="lm", formula = y ~ poly(x, 2),
              fill='white', color='black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=150, y=80, size=10, 
           label = lm_eqn(depth_mid/depth_trunk~poly(TCSA, 2, raw=T)), parse = TRUE) +
  labs(x="TCSA [cm2]", y="Canopy Depth Transition [%]", 
       shape = "", title = "D") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
multiplot(A1, A2, B1, B2, cols=2)
dev.off()

pdf("yield-sugar.pdf", width = 10, height = 12)
C1 <- ggplot(avg_vol, aes(x=age, y=tree_yield_2014*0.454)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color='black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=17, y=15, size=10, 
           label = lm_eqn(tree_yield_2014~age), 
           parse = TRUE) +
  labs(x="Age", y="Yield / tree [kg]", 
       shape="", title="A") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
C2 <- ggplot(avg_vol, aes(x=TCSA, y=tree_yield_2014*0.454)) +
  geom_smooth(method="lm", fill='white', color='black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=250, y=10, size=10, 
           label = lm_eqn(tree_yield_2014~TCSA), parse = TRUE) +
  geom_abline(slope=1) +
  geom_abline(slope=0.5) +
  geom_abline(slope=0.25) +
  labs(x="TCSA [cm2]", y="Yield / tree [kg]", 
       shape="", title="B") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
C3 <- ggplot(avg_vol, aes(x=age, y=tree_yield_2014*tree_hect*0.454)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color='black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=17, y=3000, size=10, 
           label = lm_eqn(tree_yield_2014*tree_hect~age), 
           parse = TRUE) +
  labs(x="Age", y="Yield / ha [kg]", 
       shape="", title="C") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
C4 <- ggplot(avg_vol, aes(x=TCSA, y=tree_yield_2014*tree_hect*0.454)) +
  geom_smooth(method="lm", fill='white', color='black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=250, y=3000, size=10, 
           label = lm_eqn(tree_yield_2014*tree_hect~TCSA), parse = TRUE) +
  labs(x="TCSA [cm2]", y="Yield / ha [kg]", 
       shape="", title="D") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
C5 <- ggplot(avg_vol_light, aes(x=TCSA, y=sugar_out)) +
  geom_smooth(method="lm", formula=y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Sugar Content [Brix]", 
       shape="", title="E") +
  annotate("text", x=310, y=14, size=10, 
           label = lm_eqn(sugar_out~poly(TCSA, 2, raw=T), df=avg_vol_light), 
           parse = TRUE) +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
C6 <- ggplot(avg_vol_light, aes(x=age, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=15, y=14, size=10, 
           label = lm_eqn(sugar_out~poly(age, 2, raw=T), df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Age", y="Sugar Content [Brix]", 
       shape="", title="F") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
multiplot(C1, C3, C5, C2, C4, C6, cols=2)
dev.off()

pdf("space-filling.pdf", width = 10, height = 8)
E1 <- ggplot(avg_vol, aes(x=age, y=100*spread/(30.48*spacing_x))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=25, y=60, size=10, 
           label=lm_eqn(spread/(30.48*spacing_x)~poly(age, 2, raw=T), 
                        df=age_zero), parse = TRUE) +
  labs(x="Age", y="In-row spread [%]", 
       shape = "", title = "A") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
E2 <- ggplot(avg_vol, aes(x=TCSA, y=100*spread/(30.48*spacing_x))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=300, y=60, size=10, 
           label=lm_eqn(spread/(30.48*spacing_x)~poly(TCSA, 2, raw=T), 
                        df=age_zero), parse = TRUE) +
  labs(x="TCSA", y="In-row spread [%]", 
       shape = "", title = "B") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
E3 <- ggplot(avg_vol, aes(x=age, y=(pi*(spread/200)^2*tree_hect)/100)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=25, y=25, size=10, 
           label=lm_eqn(((spread/200)^2*tree_hect)~poly(age, 2, raw=T), 
                        df=age_zero), parse = TRUE) +
  labs(x="Age", y="Canopy/Land Area [%]", 
       shape = "", title = "C") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
E4 <- ggplot(avg_vol, aes(x=TCSA, y=(pi*(spread/200)^2*tree_hect)/100)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=300, y=25, size=10, 
           label=lm_eqn(((spread/200)^2*tree_hect)~poly(TCSA, 2, raw=T), 
                        df=age_zero), parse = TRUE) +
  labs(x="TCSA", y="Canopy/Land Area [%]", 
       shape = "", title = "D") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
multiplot(E1, E3, E2, E4, cols=2)
dev.off()


### Appendix
  
  png("grower-sugar-byTCSA.png", width = 1200, height = 900)
D1 <- ggplot(avg_vol_light, aes(y=sugar_out, x=tree_yield_2014/TCSA)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=1.3, y=9.5, size=18, 
           label = lm_eqn(sugar_out~tree_yield_2014/TCSA, df=avg_vol_light), 
           parse = TRUE) +
  labs(y="Sugar Content [Brix]", x="Crop Load [lbs / cm2]", 
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
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(D1, D3, D2, D4, cols=2)
dev.off()

png("grower-architecture.png", width = 600, height = 900) 
a1 <- ggplot(avg_vol, aes(x=TCSA, y=cum_BCSA)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(size=no_scaffolds), bg="black") +
  scale_shape_manual(values=c(21:25)) +
  scale_size_continuous(range=c(8,12)) +
  annotate("text", x=350, y=100, size=18, 
           label = lm_eqn(cum_BCSA~TCSA), parse = TRUE) +
  labs(x="TCSA [cm2]", y="BCSA [cm2]", 
       size="Scaffolds", title="A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.2,0.7))
a2 <- ggplot(avg_vol, aes(y=height, x=scaffold_l*sin(angles*pi/180))) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(size=angles), bg="black") +
  scale_shape_manual(values=c(21:25)) +
  scale_size_continuous(range=c(8,12)) +
  annotate("text", x=400, y=400, size=18, 
           label = lm_eqn(height~scaffold_l*sin(angles*pi/180)), parse = TRUE) +
  labs(y="Height [cm]", x="Scaffold Length * Angle", 
       size="Branch Angle", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.2,0.7))
multiplot(a1, a2, cols=1)
dev.off()

png("grower-yield.png", width = 1200, height = 900) 
b1 <- ggplot(avg_vol, aes(x=TCSA, y=tree_yield_2014, shape=grower)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  geom_abline(slope=1) +
  geom_abline(slope=0.5) +
  geom_abline(slope=0.25) +
  labs(x="TCSA [cm2]", y="Yield / Tree [lbs]", 
       shape="", title="A") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
b2 <- ggplot(avg_vol, aes(x=volume, y=tree_yield_2014, shape=grower)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume [m3]", y="Yield / Tree [lbs]", 
       shape="", title="B") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
b3 <- ggplot(avg_vol, aes(x=tree_acre, y=tree_yield_2014*tree_acre, shape=grower)) +
  geom_smooth(method = "lm", fill='grey', color = 'black', size = 2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  scale_x_continuous(limits = c(125, 210)) +
  labs(x="Trees / Acre", y="Yield / Acre [lbs]", 
       shape="", title="C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
b4 <- ggplot(avg_vol, aes(x=volume/TCSA, y=tree_yield_2014, shape=grower)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume / TCSA", y="Yield / Tree [lbs]", 
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
  geom_point(aes(shape=grower), size=10, bg="black") +
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
  geom_point(aes(shape=grower), size=10, bg="black") +
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
  geom_point(aes(shape=grower), size=10, bg="black") +
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
  geom_point(aes(shape=grower), size=10, bg="black") +
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

png("yield-year.png", width = 600, height = 900)
d1 <- ggplot(avg_vol, aes(x=tree_yield_2013, y=tree_yield_2014, shape=grower)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  scale_x_continuous(limits = c(0,200)) +
  scale_y_continuous(limits = c(0,200)) +
  geom_abline(slope=1) +
  labs(x="Yield 2013", y="Yield 2014", 
       shape="", title="A") +
  annotate("text", x=75, y=20, size=18, 
           label = lm_eqn(tree_yield_2014~tree_yield_2013), 
           parse = TRUE) +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
d2 <- ggplot(avg_vol, aes(x=tree_yield_2014, y=tree_yield_2015, shape=grower)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  scale_x_continuous(limits = c(0,200)) +
  scale_y_continuous(limits = c(0,200)) +
  geom_abline(slope=1) +
  labs(x="Yield 2014", y="Yield 2015", 
       shape="", title="B") +
  annotate("text", x=75, y=20, size=18, 
           label = lm_eqn(tree_yield_2015~tree_yield_2014), 
           parse = TRUE) +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
d3 <- ggplot(avg_vol, aes(x=tree_yield_2014, y=tree_yield_2015, shape=grower)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  scale_x_continuous(limits = c(0,200)) +
  scale_y_continuous(limits = c(0,200)) +
  geom_abline(slope=1) +
  labs(x="Yield 2014", y="Yield 2015", 
       shape="", title="B") +
  annotate("text", x=75, y=20, size=18, 
           label = lm_eqn(tree_yield_2015~tree_yield_2014), 
           parse = TRUE) +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(d1, d2, cols = 1)
dev.off()

png("yield-acre.png", width = 1200, height = 900)
e1 <- ggplot(avg_vol, aes(x=age, y=tree_acre)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=22, y=225, size=18, 
           label = lm_eqn(tree_acre~age), parse = TRUE) +
  labs(x="Age", y="Tree / Acre", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
e2 <- ggplot(avg_vol, aes(x=TCSA, y=tree_acre)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=350, y=220, size=18, 
           label = lm_eqn(tree_yield_2014~TCSA), parse = TRUE) +
  labs(x="TCSA [cm2]", y="Tree / Acre", 
       shape="", title="B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
e3 <- ggplot(avg_vol, aes(x=age, y=tree_yield_2014*tree_acre)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=15, y=8500, size=18, 
           label = lm_eqn(tree_yield_2014*tree_acre~age), parse = TRUE) +
  labs(x="Age", y="Yield / Acre", 
       shape = "", title = "D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.4,0.1),
        legend.direction = "horizontal")
e4 <- ggplot(avg_vol, aes(x=TCSA, y=tree_yield_2014*tree_acre)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=150, y=5000, size=18, 
           label = lm_eqn(tree_yield_2014*tree_acre~TCSA), parse = TRUE) +
  labs(x="TCSA [cm2]", y="Yield / Acre", 
       shape="", title="C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(e1, e3, e2, e4, cols=2)
dev.off()

png("management.png", width = 600, height = 900)
f1 <- ggplot(avg_vol, aes(x=angles, y=spread)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=57, y=250, size=18, 
           label = lm_eqn(spread~angles), parse = TRUE) +
  labs(x="Branch Angle", y="Canopy Spread", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
f2 <- ggplot(avg_vol, aes(y=volume, x=tree_acre)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", y=8, x=215, size=18, 
           label = lm_eqn(tree_acre~volume), parse = TRUE) +
  labs(y="Canopy Volume", x="Tree / Acre", 
       shape="", title="B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(f1, f2, cols=1)
dev.off()

png("spread-acre.png", width = 1200, height = 450)
g1 <- ggplot(avg_vol, aes(x=spread*tree_acre, y=tree_yield_2014)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=65000, y=20, size=18, 
           label = lm_eqn(tree_yield_2014~spread*tree_acre, df=avg_vol), 
           parse = TRUE) +
  labs(x="Canopy Spread / Acre [cm]", y="Yield / Tree [lbs]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
g2 <- ggplot(avg_vol_light, aes(x=spread*tree_acre, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Spread / Acre [cm]", y="Sugar Content [Brix]", 
       shape = "", title = "B") +
  annotate("text", x=55000, y=9.5, size=18, 
           label = lm_eqn(sugar_out~spread*tree_acre, df=avg_vol_light), 
           parse = TRUE) +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.3,0.1),
        legend.direction = "horizontal")
multiplot(g1, g2, cols=2)
dev.off()


### Growth Stages
young <- filter(avg_vol, age_class=="young")
old <- filter(avg_vol, age_class=="old")
age_zero <- filter(avg_vol, !is.na(age))

png("grower-age-class.png", width = 1200, height = 900)
Z1 <- ggplot(age_zero, aes(x=age, y=TCSA, group=age_class)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=17, y=100, size=10, 
           label=lm_eqn(TCSA~age, df=young, params=T), parse = T) +
  annotate("text", x=23, y=200, size=10, 
           label=lm_eqn(TCSA~age, df=old, params=T), parse = T) +
  labs(x="Age", y="TCSA [cm2]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
Z2 <- ggplot(age_zero, aes(x=age, y=spread, group=age_class)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=15, y=275, size=10, 
           label=lm_eqn(spread~age, df=young, params=T), parse = T) +
  annotate("text", x=21, y=350, size=10, 
           label=lm_eqn(spread~age, df=old, params=T), parse = T) +
  labs(x="Age", y="Canopy Spread [cm]", 
       shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
Z3 <- ggplot(age_zero, aes(x=age, y=height, group=age_class)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=17, y=350, size=10, 
           label=lm_eqn(height~age, df=young, params=T), parse = T) +
  annotate("text", x=21, y=425, size=10, 
           label=lm_eqn(height~age, df=old, params=T), parse = T) +
  labs(x="Age", y="Height [cm]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
Z4 <- ggplot(age_zero, aes(x=age, y=volume, group=age_class)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=17, y=15, size=10, 
           label=lm_eqn(volume~age, df=young, params=T), parse = T) +
  annotate("text", x=21, y=20, size=10, 
           label=lm_eqn(volume~age, df=old, params=T), parse = T) +
  labs(x="Age", y="Canopy Volume [m3]", 
       shape = "", title = "D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.075),
        legend.direction = "horizontal")
multiplot(Z1, Z2, Z3, Z4, cols=2)
dev.off()



### Extra

ggplot(avg_vol, aes(x=scaffold_d, y=scaffold_l)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="scaffold_d", y="Scaffold Length [cm]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol_light, aes(x=volume*tree_acre, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume / Acre", y="Sugar Content [Brix]", 
       shape = "", title = "D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol, aes(x=volume/TCSA, y=tree_yield_2014, shape=grower)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume / TCSA", y="Yield / tree [lbs]", 
       shape="", title="B") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol, aes(x=age, y=tree_yield_2014, shape=grower)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Age", y="Yield / tree [lbs]", 
       shape="", title="B") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol, aes(x=age, y=tree_yield_2014, shape=grower)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="mass", y="Yield / tree [lbs]", 
       shape="", title="B") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol, aes(x=mass+tree_yield_2014, y=tree_yield_2014, shape=grower)) +
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

ggplot(avg_vol, aes(x=age, y=tree_yield_2014/(mass+tree_yield_2014), shape=grower)) +
  geom_smooth(method="lm", fill='grey', color='black', size=2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Age", y="Harvest Index", 
       shape="", title="B") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol, aes(x=TCSA/age, y=tree_yield_2014, shape=grower)) +
  geom_smooth(method = "lm", fill='grey', color='black', size = 2) +
  geom_point(size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA / Age", y="Yield / tree [lbs]", 
       shape="", title="A") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")

ggplot(avg_vol_light, aes(x=sugar_out, y=tree_yield_2014/volume)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Sugar Content [Brix]", y="Yield / tree / Canopy Volume", 
       shape="", title="D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.4,0.1),
        legend.direction = "horizontal")

ggplot(avg_vol_light, aes(x=absorbed_low, y=sugar_out)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
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
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=20, y=0.85, size=18, 
           label = lm_eqn(absorbed_low~volume, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Volume", y="Light", shape="", title="D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.4,0.1),
        legend.direction = "horizontal")

ggplot(avg_vol_light, aes(x=volume, y=absorbed_low)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=20, y=0.85, size=18, 
           label = lm_eqn(absorbed_low~volume/TCSA, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Volume", y="Light", shape="", title="D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.4,0.1),
        legend.direction = "horizontal")

ggplot(avg_vol_light, aes(x=volume, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=1.1, y=9.75, size=18, 
           label = lm_eqn(sugar_out~poly(volume, 2, raw=T), df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Volume", y="Sugar Content [Brix]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.9),
        legend.direction = "horizontal")

ggplot(avg_vol_light, aes(x=(height*spread), y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=1.1, y=9.75, size=18, 
           label = lm_eqn(sugar_out~height*spread, df=avg_vol_light), 
           parse = TRUE) +
  labs(x="Height  Spread", y="Sugar Content [Brix]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.9),
        legend.direction = "horizontal")

ggplot(avg_vol, aes(x=(height/100)*(spread/100), y=volume)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=25, y=12, size=18, 
           label = lm_eqn(volume~height*spread, df=avg_vol), 
           parse = TRUE) +
  labs(x="Height*Spread", y="Volume", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")