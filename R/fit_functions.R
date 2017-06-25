library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#' @export
fit_prepared_dataset = function(prepared_dataset){

  stanmodel_object = stanmodels$hierarchical_no_correlation

  sampling(object = stanmodel_object,
           data = prepared_dataset,
           iter = 10000, cores = 1,
           control = list(adapt_delta = 0.90))
}

#' @export
fit_prepared_dataset_uniform_priors = function(prepared_dataset){

  stanmodel_object = stanmodels$hierarchical_no_correlation_uniform_priors

  sampling(object = stanmodel_object,
           data = prepared_dataset,
           iter = 10000, cores = 1,
           control = list(adapt_delta = 0.90))
}

hierarchical_tktd_model_jags = "

model
{

  lks_sigma <- 0.5 * tan(lks_sigma_unif) ## lks_sigma sim cauchy(0,0.5)
  lNEC_sigma <- 0.5 * tan(lNEC_sigma_unif)
  lke_sigma <- 0.5 * tan(lke_sigma_unif)
  lm0_sigma <- 0.5 * tan(lm0_sigma_unif)

  #vectorised should work

  lm0 = lm0_mu + lm0_sigma * lm0_raw;
  lks = lks_mu + lks_sigma * lks_raw;
  lNEC = lNEC_mu + lNEC_sigma * lNEC_raw;
  lke = lke_mu + lke_sigma * lke_raw;

  m0 <- 10^lm0
  ke <- 10^lke
  ks <- 10^lks
  NEC <- 10^lNEC

  for (i in 1:ndat) {

  #######################
  #All tricks below are necessary because of the NEC threshold and the unavailability of procedural if/else conditions. Both outcomes of ifelse are evaluated so they must be finite numbers.
  # Working on the log probability might provide better numerical stability
  ###################

  help_var[i] <- ifelse(x[i]>NEC[species[i]], 0, 1.1) #To make sure that the log does not give an error

  tNEC[i] <- ifelse(x[i]>NEC[species[i]], -1/ke[species[i]] * log(1-NEC[species[i]]/(x[i] + help_var[i]*NEC[species[i]])), maxt+1) #1 trick to ensure positivity of the log and to avoid division by 0, another trick to ensure that the second condition (help_var2) is satisfiable only if the first is

  help_var2[i] <- ifelse(t[i]>tNEC[i], 1, 0)
  tref[i] <- max(tprec[i],tNEC[i])

  log_psurv[i] <- - m0[species[i]] * (t[i]-tprec[i]) - help_var2[i] * ks[species[i]]*( (x[i]-NEC[species[i]]) * (t[i]-tref[i]) + 1/ke[species[i]] * x[i] * exp(-ke[species[i]]*tref[i]) * ( exp(-ke[species[i]] * (t[i] - tref[i])) - 1) )

  ############################################################################

  psurv[i] = exp( log_psurv[i] );
  }

  lks_mu ~ dunif(-7, 2)
  lNEC_mu ~ dunif(log(minc)/log(10)-1,log(maxc)/log(10)+1)
  lke_mu ~ dunif(-7, 0)
  lm0_mu ~ dunif(-7, -1)
  # real<lower = 0, upper = 2> lks_sigma;
  # real<lower = 0, upper = 2> lNEC_sigma;
  # real<lower = 0, upper = 2> lke_sigma;
  # real<lower = 0, upper = 2> lm0_sigma;
  lks_sigma_unif  ~ dunif(0, arcsin(1)) # For the Half-Cauchy priors on sigma
  lNEC_sigma_unif  ~ dunif(0, arcsin(1))
  lke_sigma_unif  ~ dunif(0, arcsin(1))
  lm0_sigma_unif  ~ dunif(0, arcsin(1))

  for (i in 1:nspecies)
  {
  lm0_raw[i] ~ dnorm(0, 1)
  lke_raw[i] ~ dnorm(0, 1)
  lks_raw[i] ~ dnorm(0, 1)
  lNEC_raw[i] ~ dnorm(0, 1)
  }

  for (i in 1:ndat)
  {
  y[i] ~ dbin(psurv[i], Nprec[i])
  }
}"

#' @export
fit_prepared_dataset_jags = function(prepared_dataset){

  tmpf=tempfile()
  tmps=file(tmpf,"w")
  cat(hierarchical_tktd_model_jags,file=tmps)
  close(tmps)

  subdat = prepared_dataset[c("minc", "maxc", "maxt", "ndat", "nspecies", "Nprec","t", "species", "tprec", "x", "y")]

  print(subdat)

  runjags::run.jags(model = tmpf,
           data = subdat,
           method = "parallel",
           monitor = c("lke_mu",
                       "lke_sigma",
                       "lks_mu",
                       "lks_sigma",
                       "lm0_mu",
                       "lm0_sigma",
                       "lNEC_mu",
                       "lNEC_sigma",
                       "lke",
                       "lks",
                       "lm0",
                       "lNEC"))
}
