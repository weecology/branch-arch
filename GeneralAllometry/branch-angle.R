### This script summarizes branch angles for Montmorency cherry.

library(dplyr)

get_theta <- function(length, opp_length, angle) {
  theta = 90 - asin(opp_length/length) * 180 / pi
  return(theta)
}

data <- read.csv("BranchSegments.csv")
cherry <- data %>%
  filter(species == "cherry") %>%
  filter(tree == 15)
thetas <- cherry %>%
  rowwise() %>%
  transmute(theta = get_theta(length_cm, opp_length_cm, declination))
declinations <- c(cherry$declination, thetas$theta)
zeros <- subset(declinations, declinations <= 0)
nineties <- subset(declinations, declinations == 90)
middle <- subset(declinations, declinations > 0 & declinations < 90)
