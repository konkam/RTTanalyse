# library(rstan)
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#' @export
fit_prepared_dataset = function(prepared_dataset){

  stanmodel_object = stanmodels$hierarchical_no_correlation

  rstan::sampling(object = stanmodel_object,
           data = prepared_dataset,
           iter = 10000, cores = 1,
           control = list(adapt_delta = 0.90))
}

#' @export
fit_prepared_dataset_uniform_priors = function(prepared_dataset){

  stanmodel_object = stanmodels$hierarchical_no_correlation_uniform_priors

  rstan::sampling(object = stanmodel_object,
           data = prepared_dataset,
           iter = 10000, cores = 1,
           control = list(adapt_delta = 0.90))
}

#' @export
fit_prepared_dataset_with_pp_check = function(prepared_dataset){

  stanmodel_object = stanmodels$hierarchical_no_correlation_with_ppred

  rstan::sampling(object = stanmodel_object,
                  data = prepared_dataset,
                  iter = 10000, cores = 1,
                  control = list(adapt_delta = 0.90))
}
