library(tidyverse)
library(rjags)

source("block-summaries.R")
avg_vol <- avg_vol %>%
  mutate(space_fill=spread/(30.48*spacing_x))

output <- c()
output_mean <- c()
output_SD <- c()
output_CI <- c()
r_square <- c()
for (i in 1:5){
  df <- avg_vol %>%
    filter(grower==i)
  
  pred_vars <- c()
  for (j in 1:nrow(df)){
    pred_vars <- c(pred_vars, paste("y.hat[", j, "]", sep=""))
  } 
  
  jags <- jags.model("models/spacing_model.bug",
                     data = list("x" = df$age,
                                 "y" = df$space_fill,
                                 "N" = nrow(df)),
                     n.chains = 4,
                     n.adapt = 10000)
  
  dic <- dic.samples(jags, var=vars_list, n.iter=1000)
  print(dic)
  
  output[[i]] <- coda.samples(jags, c("a", "b1","b2", "pred_10", "pred_11", 
                                      "pred_12", "pred_13", pred_vars), 1000)
  output_mean <- rbind(output_mean, summary(output[[i]])$statistics[1:7, 1])
  output_SD <- rbind(output_SD, summary(output[[i]])$statistics[1:7, 2])
  output_CI <- rbind(output_CI, 2*summary(output[[i]])$statistics[1:7, 2]/sqrt(nrow(df)))
  
  
  pred_mean <- c()
  for (y_hat in pred_vars){
    pred_mean <- c(pred_mean, summary(output[[i]])$statistics[y_hat, 1])
  }
  r2 <- lm(pred_mean~df$space_fill)
  r_square[i] <- summary(r2)$r.squared
}

output_mean_fig <- data.frame(output_mean) %>%
  mutate(grower = factor(c(1,2,3,4,5))) %>%
  gather("parameter", "mean", a:pred_13)

output_SD_fig <- data.frame(output_SD) %>%
  mutate(grower = factor(c(1,2,3,4,5))) %>%
  gather("parameter", "SD", a:pred_13)

output_CI_fig <- data.frame(output_CI) %>%
  mutate(grower = factor(c(1,2,3,4,5))) %>%
  gather("parameter", "CI", a:pred_13)

output_fig <- output_mean_fig %>%
  left_join(output_SD_fig) %>%
  left_join(output_CI_fig)

pred_fig <- filter(output_fig, parameter %in% 
                     c("pred_10", "pred_11", "pred_12", "pred_13"))
