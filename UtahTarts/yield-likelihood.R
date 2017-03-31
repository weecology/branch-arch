### Likelihood Models for Yield

library(dplyr)
library(rjags)
source("block-summaries.R")

rm_na <- filter(avg_vol, !is.na(tree_yield_2014))

x <- rm_na$age
y <- rm_na$tree_yield_2014
N <- nrow(rm_na)

pred_vars <- c()
for (i in 1:nrow(avg_vol)){
  pred_vars <- c(pred_vars, paste("y.hat[", i, "]", sep=""))
} 

jags <- jags.model("models/regression.bug",
                   data = list("x" = avg_vol$cum_BCSA,
                               "y" = avg_vol$TCSA,
                               "N" = nrow(avg_vol)),
                   n.chains = 4,
                   n.adapt = 100)

output <- jags.samples(jags, c("a", "b", pred_vars), 1000)
#as.numeric(output["y.hat[1]"][[1]])
           
output <- dic.samples(jags, c("a", "b"), 1000)
output <- coda.samples(jags, c("a", "b", pred_vars), 1000)

pred_mean <- c()
for (y_hat in pred_vars){
  pred_mean <- c(pred_mean, summary(output)$statistics[y_hat, 1])
}

plot(avg_vol$TCSA, pred_mean)
abline(0,1)
