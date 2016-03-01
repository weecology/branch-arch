### This code generates the analysis for Utah Co tarts at the GROWER level

library(ggplot2)
source("multiplot.R")
source("block-summaries.R")

png("grower-architecture.png", width = 600, height = 1200) 
a1 <- ggplot(avg_vol, aes(x=TCSA, y=cum_BCSA)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="BCSA [cm2]", 
       shape="", title="A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a2 <- ggplot(avg_vol, aes(x=TCSA, y=scaffold_l)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Scaffold Length [cm]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a3 <- ggplot(avg_vol, aes(x=scaffold_d, y=scaffold_l)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Scaffold Diameter [cm]", y="Scaffold Length [cm]", 
       shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(a1, a2, a3, cols=1)
dev.off()

png("grower-canopy.png", width = 600, height = 1200) 
a4 <- ggplot(avg_vol, aes(x=TCSA, y=height)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Height [cm]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a5 <- ggplot(avg_vol, aes(x=TCSA, y=spread)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Canopy Spread [cm]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a6 <- ggplot(avg_vol, aes(x=TCSA, y=volume)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Canopy Volume [m3]", 
       shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(a4, a5, a6, cols=1)
dev.off()

png("grower-sugar.png", width = 1200, height = 900) 
a7 <- ggplot(avg_vol_light, aes(x=TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="TCSA [cm2]", y="Sugar Content [Brix]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a8 <- ggplot(avg_vol_light, aes(x=height, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Height [cm]", y="Sugar Content [Brix]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.9),
        legend.direction = "horizontal")
a9 <- ggplot(avg_vol_light, aes(x=spread, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Spread [cm]", y="Sugar Content [Brix]", 
       color = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a10 <- ggplot(avg_vol_light, aes(x=volume, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Volume [m3]", y="Sugar Content [Brix]", 
       shape = "", title = "D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(a7, a9, a8, a10, cols=2)
dev.off()

#Visualize
png("grower-sugar-byTCSA.png", width = 600, height = 900) 
a11 <- ggplot(avg_vol_light, aes(x=height/TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Height : TCSA", y="Sugar Content [Brix]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a12 <- ggplot(avg_vol_light, aes(x=spread/TCSA, y=sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Canopy Spread : TCSA", y="Sugar Content [Brix]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(a11, a12, cols=1)
dev.off()

avg_vol_zero <- filter(avg_vol, planting_year > 0)
png("grower-age.png", width = 600, height = 1200)
a13 <- ggplot(avg_vol_zero, aes(x=(2014-planting_year), y=TCSA)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Age", y="TCSA [cm2]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a14 <- ggplot(avg_vol_zero, aes(x=(2014-planting_year), y=spread)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Age", y="Canopy Spread [cm]", 
       shape = "", title = "B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
a15 <- ggplot(avg_vol_zero, aes(x=(2014-planting_year), y=volume)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size = 2) +
  geom_point(aes(shape=grower.x), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x="Age", y="Canopy Volume [m3]", 
       shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.1),
        legend.direction = "horizontal")
multiplot(a13, a14, a15, cols=1)
dev.off()
