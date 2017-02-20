rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

fit_prepared_dataset = function(prepared_dataset){
  rstan::stan(fit = stanmodels$hierarchical_no_correlation, data = prepared_dataset, iter = 200)

}

