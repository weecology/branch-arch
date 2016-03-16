### Figures for defense presentation

library(ggplot2)
source("../UtahTarts/multiplot.R")

branch_size <- read.csv("BranchSegments.csv", sep = ',', header = T)

png("./Graphics/defense-allometry-MD.png", width = 600, height = 400) 
ggplot(branch_size, aes(x=log10(diameter_mm), y=log10(tot_stem_m))) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(color=species), size=5, bg="black") +
  scale_color_manual(values=c("gold", "red")) +
  annotate("text", x=0.75, y=4, size=18, 
           label=lm_eqn(log10(tot_stem_m)~log10(diameter_mm), df=branch_size), parse = TRUE) +
  geom_abline(slope=2.67, size=2, linetype="dashed") + 
  labs(x="Log( Diameter )", y="Log( Mass )", color = "") +
  theme_classic(base_size = 36, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.9, 0.2))
dev.off()

png("./Graphics/defense-allometry-AV.png", width = 600, height = 400) 
ggplot(branch_size, aes(x=log10(tot_volume), y=log10(tot_area))) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(color=species), size=5, bg="black") +
  scale_color_manual(values=c("gold", "red")) +
  annotate("text", x=3, y=5, size=18, 
           label=lm_eqn(log10(tot_area)~log10(tot_volume), df=branch_size), parse = TRUE) +
  geom_abline(slope=0.75, size=2, linetype="dotted") + 
  geom_abline(slope=0.6, size=2, linetype="dashed") +
  labs(x="Log( Surface Area )", y="Log( Volume )", color = "") +
  theme_classic(base_size = 36, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.9, 0.2))
dev.off()

png("./Graphics/defense-allometry-LD.png", width = 600, height = 400)
ggplot(branch_size, aes(x=log10(diameter_mm), y=log10(tot_length))) +
  geom_smooth(method = "lm", fill='white', color = 'black', size = 2) +
  geom_point(aes(color=species), size=5, bg="black") +
  scale_color_manual(values=c("gold", "red")) +
  annotate("text", x=0.75, y=3.5, size=18, 
           label=lm_eqn(log10(tot_length)~log10(diameter_mm), df=branch_size), parse = TRUE) +
  geom_abline(slope=2, size=2, linetype="dotted") + 
  geom_abline(slope=0.67, size=2, linetype="dashed") +
  labs(x="Log( Diameter )", y="Log( Length )", color = "") +
  theme_classic(base_size = 36, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position=c(0.875, 0.1))
dev.off()