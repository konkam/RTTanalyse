library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#' @export
fit_prepared_dataset = function(prepared_dataset){

  stanmodel_object = stanmodels$hierarchical_no_correlation

  sampling(object = stanmodel_object, data = prepared_dataset, iter = 200, cores = 1)
}

