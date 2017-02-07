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