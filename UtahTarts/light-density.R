### cursory analysis of implications of selecting only low light values 
### (full_sun < 1000) for estimation of light environment

library(dplyr)
library(ggplot2)
source("multiplot.R")

avg_light <- read.csv("tree-averages-light.csv")

png("light.png", width = 600, height = 900)
A1 <- ggplot(avg_light, aes(x=low_light, y=avg_sugar_out)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(size=10, bg="black") +
  annotate("text", x=0.6, y=8, size=18, 
           label=lm_eqn(avg_sugar_out~low_light, df=avg_light), parse = TRUE) +
  labs(x="Light [% absorption]", y="Sugar Content [Brix]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
A2 <- ggplot(avg_light, aes(x=avg_scaffold_l, y=low_light)) +
  geom_smooth(method = "lm", formula = y ~ x,
              fill='white', color = 'black', size = 2) +
  geom_point(size=10, bg="black") +
  scale_x_continuous(limits=c(200,600)) +
  annotate("text", x=350, y=0.55, size=18, 
           label=lm_eqn(low_light~avg_scaffold_l, df=avg_light), parse = TRUE) +
  labs(x="Scaffold Length [cm]", y="Light [% absorption]", 
       shape = "", title = "A") +
  theme_classic(base_size = 24, base_family = "Helvetica") +
  theme(axis.title=element_text(size=36), legend.position="none")
multiplot(A1, A2, cols=1)
dev.off()