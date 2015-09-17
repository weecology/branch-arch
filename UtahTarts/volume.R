# Generates canopy volume values for the Utah Co. tarts

library('hypervolume')
library('dplyr')

volume <- read.csv('volume.csv', sep = ',', header = T)

## Use sphere

volume_groups <- group_by(filter(volume, Direction!=0), Block, Tree, Direction)
rank_groups <- mutate(volume_groups, D_rank = rank(Distance_cm), H_rank = rank(Height_cm))
max_dist <- summarize(volume_groups, max_dist = max(Distance_cm))

avg_dist <- summarize(max_dist, avg_dist = mean(max_dist))
avg_sphere <- transmute(avg_dist, Tree,
                        avg_sphere = round(4/3*pi*(avg_dist/100)^3, 0))  # [m3]

max_dist_vol <- mutate(max_dist, volume = 4/3*pi*(max_dist/100)^3)
max_dist_vol_tree <- group_by(max_dist_vol, Block, Tree)
sum_sphere <- summarize(max_dist_vol, sum_sphere = round(sum(volume/4), 0))

## Use multiple elipse frustum (https://en.wikipedia.org/wiki/Tree_volume_measurement)

## Check for concavity

rank_groups_byHeight <- arrange(rank_groups, Height_cm)
## Output
