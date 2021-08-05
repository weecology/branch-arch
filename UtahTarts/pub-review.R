### Yield estimation for trees age 10-20

library("tidyverse")
source("block-summaries.R")

## Yield Estimation
target <- avg_vol %>% 
  filter(age >= 10 & age <=20)

mean_2012 <- mean(target$tree_yield_2012, na.rm=TRUE)
mean_2013 <- mean(target$tree_yield_2013, na.rm=TRUE)
mean_2014 <- mean(target$tree_yield_2014, na.rm=TRUE)        
mean_2015 <- mean(target$tree_yield_2015, na.rm=TRUE) 
mean_4 <- mean(target$avg_yield_4, na.rm=TRUE) 

means <- c(mean_2012, mean_2013, mean_2014, mean_2015, mean_4)

mean_years <- mean(c(mean_2012, mean_2013, mean_2014, mean_2015))

more_2012 <- nrow(filter(target, tree_yield_2012 > 100))/43
more_2013 <- nrow(filter(target, tree_yield_2013 > 100))/43
more_2014 <- nrow(filter(target, tree_yield_2014 > 100))/43
more_2015 <- nrow(filter(target, tree_yield_2015 > 100))/43
more_4 <- nrow(filter(target, avg_yield_4 > 100))/43

mores <- c(more_2012, more_2013, more_2014, more_2015, more_4)

same_2012 <- nrow(filter(target, tree_yield_2012 > 80 & tree_yield_2012 < 100))/43
same_2013 <- nrow(filter(target, tree_yield_2013 > 80 & tree_yield_2013 < 100))/43
same_2014 <- nrow(filter(target, tree_yield_2014 > 80 & tree_yield_2014 < 100))/43
same_2015 <- nrow(filter(target, tree_yield_2015 > 80 & tree_yield_2015 < 100))/43
same_4 <- nrow(filter(target, avg_yield_4 > 80 & avg_yield_4 < 100))/43

sames <- c(same_2012, same_2013, same_2014, same_2015, same_4)

yield_output <- data.frame(year = c("2012", "2013", "2014", "2015", "all"), 
                           avg = means, more = mores, same = sames)

## Farm S

S <- filter(avg_vol, grower == "S")
qplot(S$tree_hect, S$tree_yield_2014)
qplot(S$age, S$tree_yield_2014)

mean(avg_vol$tree_yield_2014, na.rm=TRUE)
