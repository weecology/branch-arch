### This script calculates branch angles per tree

library("dplyr")

data <- read.csv("BranchSegments.csv")
apple <- filter(data, species == "apple")
explicit <- filter(apple, declination != "NA")
by_rootstock <- group_by(explicit, tree)
all <- summarize(by_rootstock, avg_all = round(90-mean(declination), 1), 
                 sd_all = round(sd(declination), 0))
zeros <- filter(by_rootstock, declination <= 0)
ninties <- filter(by_rootstock, declination == 90)
middles <- filter(filter(by_rootstock, declination > 0), declination < 90)
zeros_n <- summarize(zeros, zeros_count = n())
ninties_n <- summarize(ninties, ninties_count = n())
middles_n <- summarize(middles, mid_count = n(), 
                       avg_angle = round(90-mean(declination), 1), 
                       sd_angle = round(sd(declination), 0))
angles <- left_join(left_join(left_join(zeros_n, ninties_n), middles_n), all)
angles <- mutate(angles, z_n_ratio = round(zeros_count / ninties_count, 2))

cum_yield <- read.csv("AppleYield.csv")
angles_yield <- left_join(angles, cum_yield)

summary(lm(angles_yield$cum_yield ~ angles_yield$zeros_count))$r.squared
summary(lm(angles_yield$cum_yield ~ angles_yield$ninties_count))$r.squared
summary(lm(angles_yield$cum_yield ~ angles_yield$mid_count))$r.squared
summary(lm(angles_yield$cum_yield ~ angles_yield$avg_angle))$r.squared
summary(lm(angles_yield$cum_yield ~ angles_yield$z_n_ratio))$r.squared
plot(angles_yield$z_n_ratio, angles_yield$cum_yield)
