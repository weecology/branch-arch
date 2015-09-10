library('dplyr')

tree_sum <- read.csv("TreeSummary.csv")
yield <- read.csv("AppleYield.csv", sep =',', head=T)
tree_yield <- inner_join(tree_sum, yield)
TCSA <- pi*(tree_yield$trunk_diam_cm/2)^2  # Also as ...$trunk_diam_cm/2

model <- aov(TCSA ~ as.factor(rootstocks))
summary_TCSA <- summary(model)
test  <- duncan.test(model, "as.factor(rootstocks)")
duncan_TCSA  <- test$groups

model <- aov((tree_yield$cum_yield/TCSA) ~ as.factor(rootstocks))
summary_YE <- summary(model)
test  <- duncan.test(model, "as.factor(rootstocks)")
duncan_YE  <- test$groups