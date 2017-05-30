### Likelihood Models for Yield

library(dplyr)
library(rjags)
source("block-summaries.R")

run_likelihood_test <- function(df, model_path, data_list, vars_list, 
                                runs=10000, plot=T, samp=F) {
  pred_vars <- c()
  for (i in 1:nrow(df)){
    pred_vars <- c(pred_vars, paste("y.hat[", i, "]", sep=""))
  } 
  
  jags <- jags.model(model_path,
                     data = data_list,
                     n.chains = 4,
                     n.adapt = 1000)
  
  samples <- jags.samples(jags, c(vars_list, pred_vars), runs)
  dic <- dic.samples(jags, var=vars_list, n.iter=1000)
  output <- coda.samples(jags, c(vars_list, pred_vars), 1000)
  print(dic)
  
  if (plot == T) {
    pred_mean <- c()
    for (y_hat in pred_vars){
      pred_mean <- c(pred_mean, summary(output)$statistics[y_hat, 1])
    }
    plot(data_list$y, pred_mean)
    abline(0,1)
    r2 <- lm(pred_mean~data_list$y)
    print(summary(r2)$r.squared)
  }
  
  if (samp == T) {
    return(samples)
  }
}

# Simple Test
data_list <- list("x" = avg_vol$cum_BCSA,
                  "y" = avg_vol$TCSA,
                  "N" = nrow(avg_vol))
vars_list <- c("a", "b")
run_likelihood_test(avg_vol, "models/regression.bug", data_list, vars_list)

# Model 0: yield by age
rm_na <- filter(avg_vol, !is.na(tree_yield_2014))
data_list <- list("x" = rm_na$age,
                  "y" = rm_na$tree_yield_2014,
                  "N" = nrow(rm_na))
vars_list <- c("a", "b")
run_likelihood_test(rm_na, "models/regression.bug", data_list, vars_list)

# Model 1: yield by results suite
rm_na <- filter(avg_vol, !is.na(tree_yield_2014))
data_list <- list("x1" = rm_na$age,
                  "x2" = rm_na$TCSA,
                  "x3" = rm_na$height,
                  "x4" = rm_na$spread,
                  "x5" = rm_na$tree_hect,
                  "y" = rm_na$tree_yield_2014,
                  "N" = nrow(rm_na))
vars_list <- c("a", "b1", "b2", "b3", "b4", "b5")
run_likelihood_test(rm_na, "models/model_01.bug", data_list, vars_list)

# Model 2: avg_yield by results suite
rm_na <- filter(avg_vol, !is.na(avg_yield_4))
data_list <- list("x1" = rm_na$age,
                  "x2" = rm_na$TCSA,
                  "x3" = rm_na$height,
                  "x4" = rm_na$spread,
                  "x5" = rm_na$tree_hect,
                  "y" = rm_na$tree_yield_2014,
                  "N" = nrow(rm_na))
vars_list <- c("a", "b1", "b2", "b3", "b4", "b5")
run_likelihood_test(rm_na, "models/model_01.bug", data_list, vars_list)

# Model 3: yield by tree size
rm_na <- filter(avg_vol, !is.na(tree_yield_2014))
data_list <- list("x1" = rm_na$TCSA,
                  "x2" = rm_na$height,
                  "x3" = rm_na$spread,
                  "y" = rm_na$tree_yield_2014,
                  "N" = nrow(rm_na))
vars_list <- c("a", "b1", "b2", "b3")
run_likelihood_test(rm_na, "models/model_03.bug", data_list, vars_list)

# Model 4: grower yield
rm_na <- filter(avg_vol, !is.na(tree_yield_2014))
ch <- filter(rm_na, grower.x=="CH")
data_list <- list("x1" = ch$TCSA,
                  "x2" = ch$height,
                  "x3" = ch$spread,
                  "y" = ch$tree_yield_2014,
                  "N" = nrow(ch))
vars_list <- c("a", "b1", "b2", "b3")
run_likelihood_test(ch, "models/model_03.bug", data_list, vars_list)


