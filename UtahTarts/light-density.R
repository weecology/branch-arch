### cursory analysis of implications of selecting only low light values 
### (full_sun < 1000) for estimation of light environment

library(dplyr)

avg_light <- read.csv("tree-averages-light.csv")

hist(avg_light$low_light)

plot(avg_light$low_light, avg_light$avg_sugar_out)
test <- lm(avg_sugar_out~low_light, data=avg_light)
summary(test)$r.squared #0.336

plot(avg_light$avg_scaffold_l, avg_light$low_light)
test <- lm(low_light~avg_scaffold_l, data=avg_light)
summary(test)$r.squared #0.1779
