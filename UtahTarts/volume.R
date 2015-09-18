# Generates canopy volume values for the Utah Co. tarts

library('hypervolume')
library('dplyr')

get_V_frustum <- function(bottom, top){
  points <- north_or_northeast(bottom)
  top1 <- filter(top, Direction==points[1])$Distance_cm/100 + 
    filter(top, Direction==points[3])$Distance_cm/100
  top2 <- filter(top, Direction==points[2])$Distance_cm/100 + 
    filter(top, Direction==points[4])$Distance_cm/100
  bot1 <- filter(bottom, Direction==points[1])$Distance_cm/100 + 
    filter(bottom, Direction==points[3])$Distance_cm/100
  bot2 <- filter(bottom, Direction==points[2])$Distance_cm/100 + 
    filter(bottom, Direction==points[4])$Distance_cm/100
  height <- mean(top$Height_cm/100) - mean(bottom$Height_cm/100)
  V <- height*(pi/12)*(top1*top2 + bot1*bot2 + (top1*top2*bot1*bot2)^(1/2))
  return(V)
}

get_V_cone <- function(radius, height) {
  return(height * pi/3 * radius^2)
}

north_or_northeast <- function(ind_canopy_points) {
  if (dim(filter(ind_canopy_points, Direction=='NE'))[1] > 0) {
    return(c('NE', 'SE', 'SW', 'NW'))
  } else {
    return(c('N', 'E', 'S', 'W'))
  }
}

get_canopy_volume <- function(ind_canopy_points, heights) { 
  rank_1 <- filter(ind_canopy_points, H_rank==1)
  rank_2 <- filter(ind_canopy_points, H_rank==2)
  rank_3 <- filter(ind_canopy_points, H_rank==3)
    
  low_cone <- get_V_cone(mean(rank_1$Distance_cm/100), 
                         mean(rank_1$Height_cm/100) - min(heights$Height_cm/100))
  low_frustum <- get_V_frustum(rank_1, rank_2)
  high_frustum <- get_V_frustum(rank_2, rank_3)
  high_cone <- get_V_cone(mean(rank_3$Distance_cm/100), 
                         mean(rank_3$Height_cm/100) - max(heights$Height_cm/100))
  return(c(low_cone, low_frustum, high_frustum, high_cone))
}

volume <- read.csv('volume.csv', sep = ',', header = T)


## Check for concavity
volume_groups <- group_by(filter(volume, Direction!=0), Block, Tree, Direction)
rank_groups <- mutate(volume_groups, D_rank = rank(Distance_cm), H_rank = rank(Height_cm))

par(mfrow=c(3,3))
for (b in unique(volume$Block)) {
  block <- filter(volume, Block==b)
  plot(block$Distance_cm, block$Height_cm, main=b)
}

par(mfrow=c(3,3))
for (b in unique(volume$Block)) {
  block <- filter(volume_groups, Block==b)
  plot(block$Distance_cm, block$Height_cm, main=b, 
       ylim=c(50, 700), xlim=c(50, 400))
}

rank_sphere <- filter(rank_groups, H_rank==2, D_rank==3)
sphere_exceptions <- anti_join(rank_groups, rank_sphere)
sphere_exceptions <- arrange(filter(sphere_exceptions, H_rank==2), Block, Tree)


## Height
trunk_tree <- group_by(filter(volume, Direction==0), Block, Tree)
height_tree <- summarize(trunk_tree, height = max(Height_cm))
height_block <- summarize(group_by(height_tree, Block), avg_height = mean(height))


## Volume by Sphere
max_dist <- summarize(volume_groups, max_dist = max(Distance_cm))

avg_dist <- summarize(max_dist, avg_dist = mean(max_dist))
avg_sphere <- transmute(avg_dist, Tree,
                        avg_sphere = round(4/3*pi*(avg_dist/100)^3, 0))  # [m3]

max_dist_vol <- mutate(max_dist, volume = 4/3*pi*(max_dist/100)^3)
max_dist_vol_tree <- group_by(max_dist_vol, Block, Tree)
sum_sphere <- summarize(max_dist_vol, sum_sphere = round(sum(volume/4), 0))


## Use multiple elipse frustum (https://en.wikipedia.org/wiki/Tree_volume_measurement)

frustum_volumes <- c()
colnames(frustum_volumes) <- c('block', 'tree', 'total_V', 'bottom_frust', 
                               'top_frust', 'bot_cone', 'top_cone') 
for (b in unique(volume$Block)) {
  block_points <- filter(rank_groups, Block==b)
  block_heights <- filter(trunk_tree, Block==b)
  for (t in unique(block$Tree)) {
    tree_points <- filter(block_points, Tree==t)
    tree_heights <- filter(block_heights, Tree==t)
    volumes <- get_canopy_volume(tree_points, tree_heights)
    total_volume <- sum(volumes)
    bottom_frust <- round(volumes[2] / total_volume * 100, 0)
    top_frust <- round(volumes[3] / total_volume * 100, 0)
    frust_set <- c(b, t, round(total_volume, 0), bottom_frust, top_frust, 
                   round(volumes[1], 2), round(volumes[4], 2))
    frustum_volumes <- rbind(frustum_volumes, frust_set) 
  }
}


## Output
