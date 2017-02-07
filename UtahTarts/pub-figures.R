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
  geom_segment(aes(x=16, y=175, xend=14.5, yend=240), size=1.25,
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
  geom_segment(aes(x=16.25, y=375, xend=15.25, yend=475), size=1.25,
               arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=16.25, y=350, size=10, label=lm_intercept(spread~age)) +
  labs(x="Age", y="Canopy Spread [cm]", shape = "", title = "C") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
Z3 <- ggplot(age_zero, aes(x=age, y=height, group=age_class)) +
  geom_smooth(method = "lm", alpha=0, color='black', size = 2, fullrange=T) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  ylim(NA, max(age_zero$height)) +
  geom_segment(aes(x=14.5, y=425, xend=13.5, yend=475), size=1.25,
               arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=15, y=400, size=10, label=lm_intercept(height~age)) +
  labs(x="Age", y="Height [cm]", shape="", title="B") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
Z4 <- ggplot(age_zero, aes(x=age, y=volume, group=age_class)) +
  geom_smooth(method="lm", alpha=0, color='black', size=2, fullrange=T) +
  geom_point(aes(shape=grower), size=10, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  ylim(NA, max(age_zero$volume)) +
  geom_segment(aes(x=16, y=25, xend=15, yend=40), size=1.25,
               arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=16.5, y=20, size=10, label=lm_intercept(volume~age)) +
  labs(x="Age", y="Canopy Volume [m3]", shape="", title="D") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.7,0.075),
        legend.direction = "horizontal")
multiplot(Z1, Z2, Z3, Z4, cols=2)
dev.off()