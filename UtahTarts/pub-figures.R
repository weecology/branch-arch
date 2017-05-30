### This code generates the publication figures and analysis for Utah Co tarts

## Initialize

library(dplyr)
library(segmented)
library(ggplot2)
source("multiplot.R")
source("block-summaries.R")

# Data Subsets
young <- filter(avg_vol, age_class=="young")
old <- filter(avg_vol, age_class=="old")
age_zero <- filter(avg_vol, !is.na(age))

# Label Expressions
tcsa_lab <- expression("TCSA [cm"^2*"]")
ha_ha <- expression("Canopy Area [ha"%.%"ha"^-1*"]")

## Figure Generation

# Fig 1
pdf("Fig1.pdf", width=6, height=5)
ggplot(age_zero, aes(x=age, y=TCSA, group=age_class)) +
  geom_smooth(method="lm", fill=NA, color='black', size=1.5, fullrange=T) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  ylim(NA, max(age_zero$TCSA)) +
  geom_segment(aes(x=16, y=175, xend=14, yend=240), size=1,
               arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=12, y=80, size=8, 
           label=lm_eqn(TCSA~age, df=young), parse = T) +
  annotate("text", x=25, y=200, size=8, 
           label=lm_eqn(TCSA~age, df=old), parse = T) +
  annotate("text", x=16, y=150, size=8, label=lm_intercept(TCSA~age)) +
  labs(x="Age", y=tcsa_lab, shape = "") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
dev.off()

report_lm(TCSA~age, df=young)
report_lm(TCSA~age, df=old)
confint(segmented(lm(TCSA~age, data=avg_vol), seg.Z=~age))


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
           label=lm_intercept(height~age)) +
  annotate("text", x=13, y=3.25, size=8, 
           label=lm_eqn(height/100~age, df=young), parse = T) +
  annotate("text", x=25, y=3.5, size=8, 
           label=lm_eqn(height~age, df=old), parse = T) +
  labs(x="Age", y="Height [m]", shape = "", title = "A") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"))
B1 <- ggplot(avg_vol, aes(x=TCSA, y=height/100)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x=tcsa_lab, y="Height [m]", shape = "", title = "B") +
  annotate("text", x=350, y=3.25, size=8, 
           label = lm_eqn(height~poly(TCSA, 2, raw=T)), parse = TRUE) +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0, 0.3), "cm"))
multiplot(A1, B1, cols=2)
dev.off()

report_lm(height~age, df=young)
report_lm(height~age, df=old)
confint(segmented(lm(height~age, data=avg_vol), seg.Z=~age))

#report_lm(spread~age, df=young)
#report_lm(spread~age, df=old)
#report_lm(volume~age, df=young)
#report_lm(volume~age, df=old)
#confint(segmented(lm(spread~age, data=avg_vol), seg.Z=~age))
#confint(segmented(lm(volume~age, data=avg_vol), seg.Z=~age))

report_lm(height~TCSA)

# Fig 3
pdf("Fig3.pdf", width=6, height=5)
ggplot(avg_vol, aes(x=age, y=spread/(30.48*spacing_x))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  geom_hline(yintercept=1, linetype=2) +
  annotate("text", x=25, y=0.6, size=8, 
           label=lm_eqn(spread/(30.48*spacing_x)~poly(age, 2, raw=T), 
                        df=age_zero), parse = TRUE) +
  labs(x="Age", y="In-row Space Filling", shape = "") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
dev.off()

report_lm_poly(spread/(30.48*spacing_x)~poly(age, 2, raw=T))

# Fig 4
pdf("Fig4.pdf", width=10, height=8)
E3 <- ggplot(avg_vol, aes(x=age, y=(pi*(spread/200)^2*tree_hect/10000))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=25, y=0.25, size=8, 
           label=lm_eqn(((spread/200)^2*tree_hect)~poly(age, 2, raw=T), 
                        df=age_zero), parse = TRUE) +
  labs(x="Age", y=ha_ha, shape = "", title = "A") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0), "cm"))
E4 <- ggplot(avg_vol, aes(x=TCSA, y=(pi*(spread/200)^2*tree_hect/10000))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=300, y=0.25, size=8, 
           label=lm_eqn(((spread/200)^2*tree_hect)~poly(TCSA, 2, raw=T), 
                        df=age_zero), parse = TRUE) +
  labs(x=tcsa_lab, y=ha_ha, shape = "", title = "B") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0, 0), "cm"))
E5 <- ggplot(avg_vol, aes(x=age, y=volume*tree_hect/10000)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=23, y=0.25, size=8, 
           label=lm_eqn(tree_hect*volume~poly(age, 2, raw=T), df=age_zero), parse = TRUE) +
  labs(x="Age", y="Canopy Depth [m]", 
       shape = "", title = "C") +
  theme_classic(base_size=14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0.9), "cm"))
E6 <- ggplot(avg_vol, aes(x=TCSA, y=volume*tree_hect/10000)) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  labs(x=tcsa_lab, y="Canopy Depth [m]", 
       shape = "", title = "D") +
  annotate("text", x=350, y=0.1, size=8, 
           label = lm_eqn(tree_hect*volume~poly(TCSA, 2, raw=T)), parse = TRUE) +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0, 0.9), "cm"))
multiplot(E3, E5, E4, E6, cols=2)
dev.off()

report_lm_poly((pi*(spread/200)^2*tree_hect/10000)~poly(age, 2, raw=T))
report_lm_poly((pi*(spread/200)^2*tree_hect/10000)~poly(TCSA, 2, raw=T))
report_lm_poly(volume*tree_hect/10000~poly(age, 2, raw=T))
report_lm_poly(volume*tree_hect/10000~poly(TCSA, 2, raw=T))