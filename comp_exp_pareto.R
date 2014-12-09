get_mle_expo = function(num_list){
  # Obtain the MLE parameter of the exponential distribution
  return(1 / mean(num_list))
}

get_mle_pareto = function(num_list){
  # Obtain the MLE parameter of the loglinear distribution
  x_m = min(num_list)
  alpha = length(num_list) / sum(log(num_list) - log(x_m))
  return(alpha)
}

get_aicc_expo_pareto = function(num_list_orig){
  # Return the AICc values of the exponential and the loglinear distributions
  # Exponential distribution has 1 parameter while loglinear has 2
  num_list = num_list_orig[!is.na(num_list_orig)]
  par_exp = get_mle_expo(num_list)
  alpha = get_mle_pareto(num_list)
  loglik_exp = sum(dexp(num_list, par_exp, log = T))
  loglik_pareto = sum(log(alpha) + alpha * log(min(num_list)) - (alpha + 1) * log(num_list))
  AICc_exp = 2 * 1 - 2 * loglik_exp + 2 * 1 * 2 / (length(num_list) - 1 - 1)
  AICc_pareto = 2 * 2 - 2 * loglik_pareto + 2 * 2 * 3 / (length(num_list) - 2 - 1)
  return(c(AICc_exp, AICc_pareto))
}