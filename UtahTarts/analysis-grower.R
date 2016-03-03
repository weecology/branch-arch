### This code generates the analysis for Utah Co tarts at the GROWER level

library(ggplot2)
source("multiplot.R")
source("block-summaries.R")

lm_eqn <- function(df, formula){
  m <- lm(formula, data = df)
  eq <- substitute(~~italic(r)^2~"="~r2,
          list(r2 = format(summary(m)$r.squared, digits = 3)))
  return(as.character(as.expression(eq)))
}

png("grower-age.png", width = 1200, height = 900)
A1 <- ggplot(avg_vol, aes(x=age, y=TCSA)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Age", y="TCSA [cm2]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A2 <- ggplot(avg_vol, aes(x=age, y=spread)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Age", y="Canopy Spread [cm]", 
       shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A3 <- ggplot(avg_vol, aes(x=age, y=height)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Age", y="Height [cm]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A4 <- ggplot(avg_vol, aes(x=age, y=volume)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
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
  annotate("text", x = 200, y = 300, size = 18, 
           label = lm_eqn(avg_vol, height~TCSA), parse = TRUE) +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
B2 <- ggplot(avg_vol, aes(x=TCSA, y=spread)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
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
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(B1, B2, B3, cols=1)
dev.off()

png("grower-sugar-yield.png", width = 1200, height = 900)
C1 <- ggplot(avg_vol, aes(x=TCSA, y=tree_yield_2014)) +
  geom_smooth(method = "lm", fill='white', color='black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
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
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
C3 <- ggplot(avg_vol, aes(x=TCSA/age, y=tree_yield_2014)) +
  geom_smooth(method = "lm", fill='white', color='black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA / Age", y="Yield / tree [lbs]", 
       shape="", title="C") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
C4 <- ggplot(avg_vol_light, aes(x=TCSA/age, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA / Age", y="Sugar Content [Brix]", 
       shape="", title="D") +
  theme_classic(base_size=24, base_family="Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(C1, C3, C2, C4, cols=2)
dev.off()

png("grower-sugar-byTCSA.png", width = 1200, height = 900)
D1 <- ggplot(avg_vol_light, aes(y=sugar_out, x=tree_yield_2014/TCSA)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(y="Sugar Content [Brix]", x="Yield Efficiency [lbs / cm2]", 
       shape="", title="A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
D2 <- ggplot(avg_vol_light, aes(x=height/TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Height : TCSA", y="Sugar Content [Brix]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
D3 <- ggplot(avg_vol_light, aes(x=spread/TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
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
  labs(x="TCSA [cm2]", y="BCSA [cm2]", 
       shape="", title="A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a2 <- ggplot(avg_vol, aes(y=height, x=scaffold_l*sin(angles*pi/180))) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(y="Height [cm]", x="Scaffold Length * Angle", 
       shape = "", title = "C") +
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
  labs(x="TCSA [cm2]", y="Sugar Content [Brix]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
c2 <- ggplot(avg_vol_light, aes(x=height, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
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
  labs(x="Canopy Spread [cm]", y="Sugar Content [Brix]", 
       color = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
c4 <- ggplot(avg_vol_light, aes(x=volume, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
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