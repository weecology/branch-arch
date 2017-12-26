### Likelihood Models for Yield

library(dplyr)
library(rjags)
source("block-summaries.R")

## Data Standardization
avg_vol_std <- avg_vol %>%
  transmute(block = block,
            block_code = block_code,
            grower = grower,
            TCSA = (TCSA - mean(avg_vol$TCSA))/sd(avg_vol$TCSA),
            mass = (mass - mean(avg_vol$mass))/sd(avg_vol$mass),
            cum_BCSA = (cum_BCSA - mean(avg_vol$cum_BCSA))/sd(avg_vol$cum_BCSA),
            no_scafolds = no_scaffolds,
            scaffold_l = (scaffold_l - mean(avg_vol$scaffold_l, na.rm = TRUE))/
              sd(avg_vol$scaffold_l, na.rm = TRUE),
            scaffold_d = (scaffold_d - mean(avg_vol$scaffold_d))/sd(avg_vol$scaffold_d),
            angles = (angles - mean(avg_vol$angles))/sd(avg_vol$angles),
            height = (height - mean(avg_vol$height))/sd(avg_vol$height),
            spread = (spread - mean(avg_vol$spread))/sd(avg_vol$spread),
            volume = (volume - mean(avg_vol$volume))/sd(avg_vol$volume),
            top_size = (top_size - mean(avg_vol$top_size))/sd(avg_vol$top_size),
            depth_trunk = (depth_trunk - mean(avg_vol$depth_trunk))/sd(avg_vol$depth_trunk),
            depth_mid = (depth_mid - mean(avg_vol$depth_mid))/sd(avg_vol$depth_mid),
            spacing_x = (spacing_x - mean(avg_vol$spacing_x))/sd(avg_vol$spacing_x),
            spacing_y = (spacing_y - mean(avg_vol$spacing_y))/sd(avg_vol$spacing_y),
            tree_yield_2012 = (tree_yield_2012 - mean(avg_vol$tree_yield_2012, na.rm = TRUE))/
              sd(avg_vol$tree_yield_2012, na.rm = TRUE),
            tree_yield_2013 = (tree_yield_2013 - mean(avg_vol$tree_yield_2013, na.rm = TRUE))/
              sd(avg_vol$tree_yield_2013, na.rm = TRUE),
            tree_yield_2014 = (tree_yield_2014 - mean(avg_vol$tree_yield_2014, na.rm = TRUE))/
              sd(avg_vol$tree_yield_2014, na.rm = TRUE),
            tree_yield_2015 = (tree_yield_2015 - mean(avg_vol$tree_yield_2015, na.rm = TRUE))/
              sd(avg_vol$tree_yield_2015, na.rm = TRUE),
            age = (age - mean(avg_vol$age))/sd(avg_vol$age),
            tree_hect = (tree_hect - mean(avg_vol$tree_hect))/sd(avg_vol$tree_hect),
            biennial = (biennial - mean(avg_vol$biennial, na.rm = TRUE))/
              sd(avg_vol$biennial, na.rm = TRUE),
            avg_yield_4 = (avg_yield_4 - mean(avg_vol$avg_yield_4, na.rm = TRUE))/
              sd(avg_vol$avg_yield_4, na.rm = TRUE),
            avg_yield_3 = (avg_yield_3 - mean(avg_vol$avg_yield_3, na.rm = TRUE))/
              sd(avg_vol$avg_yield_3, na.rm = TRUE),
            avg_yield_2f = (avg_yield_2f - mean(avg_vol$avg_yield_2f, na.rm = TRUE))/
              sd(avg_vol$avg_yield_2f, na.rm = TRUE),
            avg_yield_2b = (avg_yield_2b - mean(avg_vol$avg_yield_2b, na.rm = TRUE))/
              sd(avg_vol$avg_yield_2b, na.rm = TRUE))

## Model

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
rm_na <- filter(avg_vol_std, !is.na(tree_yield_2014))
data_list <- list("x" = rm_na$age,
                  "y" = rm_na$tree_yield_2014,
                  "N" = nrow(rm_na))
vars_list <- c("a", "b")
run_likelihood_test(rm_na, "models/regression.bug", data_list, vars_list)


# Model 1: yield by results suite
rm_na <- filter(avg_vol_std, !is.na(tree_yield_2014))
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
rm_na <- filter(avg_vol_std, !is.na(avg_yield_4))
data_list <- list("x1" = rm_na$age,
                  "x2" = rm_na$TCSA,
                  "x3" = rm_na$height,
                  "x4" = rm_na$spread,
                  "x5" = rm_na$tree_hect,
                  "y" = rm_na$avg_yield_4,
                  "N" = nrow(rm_na))
vars_list <- c("a", "b1", "b2", "b3", "b4", "b5")
run_likelihood_test(rm_na, "models/model_01.bug", data_list, vars_list)

# Model 3: yield by tree size
rm_na <- filter(avg_vol_std, !is.na(tree_yield_2014))
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


