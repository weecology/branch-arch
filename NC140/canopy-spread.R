### This code calculates canopy spread for all apple trees

library('dplyr')

data <- filter(read.csv('../GeneralAllometry/CanopyVolume.csv'),
               species == 'apple')

trunk_position <- filter(data, location == 4)
east <- filter(trunk_position, position == 'ed')
west <- filter(trunk_position, position == 'wd')
spread <- transmute(east,
                    tree = tree,
                    spread = east$m + west$m)            
#write.csv(spread, 'CanopySpread.csv')

east_pos <- group_by(filter(data, position == 'ed'), tree)
east_max <- summarize(east_pos, east = max(m))
west_pos <- group_by(filter(data, position == 'wd'), tree)
west_max <- summarize(west_pos, west = max(m))

spread_max <- transmute(east_max,
                    tree = tree,
                    spread = east_max$east + west_max$west) 
#write.csv(spread_max, 'CanopySpread.csv')
