### This code generates the publication figures and analysis for Utah Co tarts

## Initialize

library(dplyr)
library(segmented)
library(agricolae)
library(ggplot2)
source("multiplot.R")
source("block-summaries.R")

# Data Subsets
young <- filter(avg_vol, age_class=="young")
young_plus <- filter(avg_vol, age <= 15)
prime <- filter(avg_vol, age >= 11 & age<=22)
old <- filter(avg_vol, age_class=="old")
age_zero <- filter(avg_vol, !is.na(age))

# Label Expressions
tcsa_lab <- expression("TCSA [cm"^2*"]")
ha_ha <- expression("Canopy Area [ha"%.%"ha"^-1*"]")
per_ha <- expression("Tree Density [ha"^-1*"]")

## Figure Generation

# Fig 1
pdf("Fig1.pdf", width=10, height=5)
A1 <- ggplot(avg_vol, aes(x=age, y=spread/(30.48*spacing_x))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  geom_hline(yintercept=1, linetype=2) +
  annotate("text", x=25, y=0.6, size=8, 
           label=lm_eqn(spread/(30.48*spacing_x)~poly(age, 2, raw=T), 
                        df=avg_vol), parse = TRUE) +
  geom_text(aes(x=min(age), y=max(spread/(30.48*spacing_x)), label = "A"), 
            vjust = "inward", hjust = "inward", size = 12) +
  labs(x="Age", y="In-row Space Filling", shape = "") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
B1 <- ggplot(avg_vol, aes(x=age, y=(pi*(spread/200)^2*tree_hect/10000))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=25, y=0.25, size=8, 
           label=lm_eqn((pi*(spread/200)^2*tree_hect/10000)~poly(age, 2, raw=T), 
                        df=avg_vol), parse = TRUE) +
  labs(x="Age", y=ha_ha, shape = "") +
  geom_text(aes(x=min(age), y=max(pi*(spread/200)^2*tree_hect/10000), 
                label = "B"), vjust = "inward", hjust = "inward", size = 12) +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0.3, 0), "cm"))
multiplot(A1, B1, cols=2)
dev.off()

report_lm_poly(spread/(30.48*spacing_x)~poly(age, 2, raw=T), df=avg_vol)
report_lm_poly((pi*(spread/200)^2*tree_hect/10000)~poly(age, 2, raw=T), df=avg_vol)


# Fig 2
pdf("Fig2.pdf", width=6, height=5)
ggplot(age_zero, aes(x=age, y=TCSA, group=age_class)) +
  geom_smooth(method="lm", fill=NA, color='black', size=1.5, fullrange=F) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  ylim(NA, max(age_zero$TCSA)) +
  #geom_segment(aes(x=16, y=175, xend=14, yend=240), size=1,
  #             arrow=arrow(length=unit(0.08, "npc"))) +
  annotate("text", x=12, y=80, size=8, 
           label=lm_eqn(TCSA~age, df=young), parse = T) +
  annotate("text", x=25, y=200, size=8, 
           label=lm_eqn(TCSA~age, df=old), parse = T) +
  #annotate("text", x=16, y=150, size=8, label=lm_intercept(TCSA~age)) +
  labs(x="Age", y=tcsa_lab, shape = "") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
dev.off()

report_lm(TCSA~age, df=young)
report_lm(TCSA~age, df=old)
confint(segmented(lm(TCSA~age, data=avg_vol), seg.Z=~age))

# Fig 3
pdf("Fig3.pdf", width=6, height=5)
ggplot(avg_vol, aes(x=TCSA, y=(pi*(spread/200)^2*tree_hect/10000))) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2),
              fill='white', color = 'black', size=1.5) +
  geom_point(aes(shape=grower), size=5, bg="black") +
  scale_shape_manual(values=c(21:25)) +
  annotate("text", x=300, y=0.25, size=8, 
           label=lm_eqn((pi*(spread/200)^2*tree_hect/10000)~poly(TCSA, 2, raw=T), 
                        df=avg_vol), parse = TRUE) +
  labs(x=tcsa_lab, y=ha_ha, shape = "") +
  theme_classic(base_size = 14, base_family = "Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none",
        plot.margin = unit(c(0.3, 0.3, 0, 0), "cm"))
dev.off()

report_lm_poly((pi*(spread/200)^2*tree_hect/10000)~poly(TCSA, 2, raw=T), df=avg_vol)


report_lm_poly(((spread/200)^2*tree_hect)~poly(age, 2, raw=T), df=avg_vol)
report_lm(((spread/200)^2*tree_hect)~age, df=prime)

# Fig 2
pdf("Fig2-extra.pdf", width=10, height=5)
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
report_lm(height~TCSA)

# Fig 4
pdf("Fig4-extra.pdf", width=10, height=8)
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

## Additional curve fitting

report_lm(height~age, df=young)
report_lm(height~age, df=old)
confint(segmented(lm(height~age, data=avg_vol), seg.Z=~age))
report_lm(height~TCSA)
report_lm_poly(height~poly(age, 2, raw=T), df=age_zero)
report_lm_poly(spread~poly(age, 2, raw=T), df=age_zero)
report_lm_poly(volume~poly(age, 2, raw=T), df=age_zero)
report_lm(spread~age, df=young)
report_lm(spread~age, df=old)
report_lm(volume~age, df=young)
report_lm(volume~age, df=old)
confint(segmented(lm(spread~age, data=avg_vol), seg.Z=~age))
confint(segmented(lm(volume~age, data=avg_vol), seg.Z=~age))
report_lm_poly((pi*(spread/200)^2*tree_hect/10000)~poly(age, 2, raw=T))
report_lm_poly((pi*(spread/200)^2*tree_hect/10000)~poly(TCSA, 2, raw=T))
report_lm_poly(volume*tree_hect/10000~poly(age, 2, raw=T))
report_lm_poly(volume*tree_hect/10000~poly(TCSA, 2, raw=T))

## Tree density

median(avg_vol$tree_hect)
quant <- quantile(avg_vol$tree_hect)
by_dev <- between(avg_vol$tree_hect, 
                  mean(avg_vol$tree_hect)-sd(avg_vol$tree_hect), 
                  mean(avg_vol$tree_hect)+sd(avg_vol$tree_hect))
per_quant <- 100*sum(by_dev)/nrow(avg_vol)

## HortSci Review
mid <- filter(avg_vol, age>10 & age<15)
mid_high <- filter(avg_vol, age>15 & age<24)

fit <- aov((pi*(spread/2)^2*tree_hect)~grower, data=mid) #p=0.115
fit <- aov((pi*(spread/2)^2*tree_hect)~management, data=mid) #p=0.0551
fit <- aov((pi*(spread/2)^2*tree_hect)~grower, data=mid_high) #p=0.191
fit <- aov((pi*(spread/2)^2*tree_hect)~management, data=mid_high) #p=0.825

fit <- aov((tree_hect*volume)~grower, data=mid) #p=0.0754
fit <- aov((tree_hect*volume)~management, data=mid) #p=0.125
fit <- aov((tree_hect*volume)~grower, data=mid_high) #p=0.208
fit <- aov((tree_hect*volume)~management, data=mid_high) #p=0.785


# Management Differences
fit <- aov(tree_hect~grower, data = avg_vol)
summary(fit)  # p < 0.001
HSD.test(fit, "grower")$groups
tree_hectVgrower <- data.frame(grower = c(1:5), 
                               HSD = c("a", "ab", "b", "a", "a"))

fit <- aov(no_scaffolds~grower, data = avg_vol)
summary(fit)  # p < 0.001
HSD.test(fit, "grower")$groups
no_scaffoldsVgrower <- data.frame(grower = c(1:5), 
                               HSD = c("a", "b", "a", "ab", "ab"))

# Fig 5
pdf("Fig5.pdf", width=10, height=5)
F1 <- ggplot(avg_vol, aes(x=grower, y=tree_hect)) + 
  geom_boxplot() +
  geom_text(data = tree_hectVgrower, aes(label = HSD, y = 550), size=6) +
  labs(x="Grower", y=per_ha, title = "A") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
F2 <- ggplot(avg_vol, aes(x=grower, y=no_scaffolds)) + 
  geom_boxplot() +
  geom_text(data = no_scaffoldsVgrower, aes(label = HSD, y = 4.15), size=6) +
  labs(x="Grower", y="Scaffold Count", title = "B") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
multiplot(F1, F2, cols=2)
dev.off()

# Fig 6
source("growth-model.R")
pdf("Fig6.pdf", width=6, height=5)
ggplot(pred_fig, aes(x = parameter, y = mean, shape = grower)) +
  geom_errorbar(aes(ymin=mean-CI+0.003, ymax=mean+CI-0.003), width=0, position = position_dodge(0.1)) +
  geom_point(size=5, bg="black") +
  geom_point(aes(y=mean-CI), position = position_dodge(0.1)) +
  geom_point(aes(y=mean+CI), position = position_dodge(0.1)) +
  geom_line(aes(group = grower)) +
  scale_shape_manual(values=c(21:25)) +
  scale_x_discrete(labels=c("10","11","12","13")) +
  labs(x="Age [years]", y="Predicted Space Filling", shape = "") +
  theme_classic(base_size=14, base_family="Helvetica") +
  theme(axis.title=element_text(size=20), legend.position="none")
dev.off()
