# Generates canopy volume values for the Utah Co. tarts

#library('hypervolume')
library('dplyr')

get_V_frustum <- function(bottom, top){
  points <- north_or_northeast(bottom)
  top1 <- filter(top, direction==points[1])$distance_cm/100 + 
    filter(top, direction==points[3])$distance_cm/100
  top2 <- filter(top, direction==points[2])$distance_cm/100 + 
    filter(top, direction==points[4])$distance_cm/100
  bot1 <- filter(bottom, direction==points[1])$distance_cm/100 + 
    filter(bottom, direction==points[3])$distance_cm/100
  bot2 <- filter(bottom, direction==points[2])$distance_cm/100 + 
    filter(bottom, direction==points[4])$distance_cm/100
  height <- mean(top$height_cm/100) - mean(bottom$height_cm/100)
  V <- height*(pi/12)*(top1*top2 + bot1*bot2 + (top1*top2*bot1*bot2)^(1/2))
  return(V)
}

get_V_cone <- function(radius, height) {
  return(height * pi/3 * radius^2)
}

north_or_northeast <- function(ind_canopy_points) {
  if (dim(filter(ind_canopy_points, direction=='NE'))[1] > 0) {
    return(c('NE', 'SE', 'SW', 'NW'))
  } else {
    return(c('N', 'E', 'S', 'W'))
  }
}

get_canopy_volume <- function(ind_canopy_points, heights) { 
  rank_1 <- filter(ind_canopy_points, H_rank==1)
  rank_2 <- filter(ind_canopy_points, H_rank==2)
  rank_3 <- filter(ind_canopy_points, H_rank==3)
    
  low_cone <- get_V_cone(mean(rank_1$distance_cm/100), 
                         mean(rank_1$height_cm/100) - min(heights$height_cm/100))
  low_frustum <- get_V_frustum(rank_1, rank_2)
  high_frustum <- get_V_frustum(rank_2, rank_3)
  high_cone <- get_V_cone(mean(rank_3$distance_cm/100), 
                         mean(rank_3$height_cm/100) - max(heights$height_cm/100))
  return(c(low_cone, low_frustum, high_frustum, high_cone, 
           max(heights$height_cm)))
}

volume <- read.csv('volume.csv', sep = ',', header = T)


## Check for concavity
volume_groups <- group_by(filter(volume, direction!=0), block, tree, direction)
rank_groups <- mutate(volume_groups, D_rank = rank(distance_cm), H_rank = rank(height_cm))

par(mfrow=c(3,3))
for (b in unique(volume$block)) {
  block <- filter(volume, block==b)
  #plot(block$distance_cm, block$height_cm, main=b)
}

par(mfrow=c(3,3))
for (b in unique(volume$block)) {
  block <- filter(volume_groups, block==b)
  #plot(block$distance_cm, block$height_cm, main=b, 
  #     ylim=c(50, 700), xlim=c(50, 400))
}

rank_sphere <- filter(rank_groups, H_rank==2, D_rank==3)
sphere_exceptions <- anti_join(rank_groups, rank_sphere)
sphere_exceptions <- arrange(filter(sphere_exceptions, H_rank==2), block, tree)


## Height
trunk_tree <- group_by(filter(volume, direction==0), block, tree)
height_tree <- summarize(trunk_tree, height = max(height_cm))
height_block <- summarize(group_by(height_tree, block), avg_height = mean(height))


## Volume by Sphere
max_dist <- summarize(volume_groups, max_dist = max(distance_cm))

avg_dist <- summarize(max_dist, avg_dist = mean(max_dist))
avg_sphere <- transmute(avg_dist, tree,
                        spread = round(avg_dist*2, 0),
                        avg_sphere = round(4/3*pi*(avg_dist/100)^3, 0))  # [m3]

max_dist_vol <- mutate(max_dist, volume = 4/3*pi*(max_dist/100)^3)
max_dist_vol_tree <- group_by(max_dist_vol, block, tree)
sum_sphere <- summarize(max_dist_vol, sum_sphere = round(sum(volume/4), 0))


## Use multiple elipse frustum (https://en.wikipedia.org/wiki/tree_volume_measurement)

frustum_volumes <- data.frame()
 
for (b in unique(volume$block)) {
  block_points <- filter(rank_groups, block==b)
  block_heights <- filter(trunk_tree, block==b)
  for (t in unique(block$tree)) {
    tree_points <- filter(block_points, tree==t)
    tree_heights <- filter(block_heights, tree==t)
    volumes <- get_canopy_volume(tree_points, tree_heights)
    total_volume <- sum(volumes[1:4])
    bottom_frust <- round(volumes[2] / total_volume * 100, 0)
    top_frust <- round(volumes[3] / total_volume * 100, 0)
    frust_set <- data.frame(block=b, tree=t, frustum=round(total_volume, 0), 
                            bottom_frust=bottom_frust, top_frust=top_frust, 
                            bot_cone=round(volumes[1], 2), 
                            top_cone=round(volumes[4], 2),
                            height=volumes[5])
    frustum_volumes <- rbind(frustum_volumes, frust_set) 
  }
}

## Output
volumes_out <- left_join(left_join(avg_sphere, sum_sphere), frustum_volumes)
#write.csv(volumes_out, "canopy-size.csv")
