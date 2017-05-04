##########################in this model, mostly all the stochastic links have been vectorised.
###############deterministic links does not make much difference when vectorised.
############## The normal distribution for the lower level of the hierarchy are specified using the non-centered parameterisation, see Stan Manual.
############# The half-Cauchy distributions are also re-parameterised, see Stan Manual.
############# The operations on the probabilities are performed on the logprob, but this is not sufficient to avoid all numerical errors
data {
  int<lower=0> ndat; // Number of data points
  int<lower=0> nspecies; // Number of species
  real<lower=0> minc; // minimum concentration
  real<lower=0> maxc; // maximum concentration
  real<lower=0> x[ndat]; // Concentration
  real<lower=0> t[ndat]; // measurement date
  real<lower=0> tprec[ndat]; // previous measurement date
  int<lower=0> y[ndat]; // number of survivors
  int<lower=0> Nprec[ndat]; // Initial number of species
  int<lower=0> species[ndat]; // index of species number
  real<lower=0> ke_min;
  real<lower=0> ke_max;
  real<lower=0> ks_min;
  real<lower=0> ks_max;
}

parameters {
  real<lower=log(minc)/log(10)-1,upper=log(maxc)/log(10)+1> lNEC_mu;
  real<lower = -7, upper = -1> lm0_mu;
  # real<lower = 0, upper = 2> lNEC_sigma;
  # real<lower = 0, upper = 2> lm0_sigma;
  real<lower = 0, upper = pi()/2> lNEC_sigma_unif;
  real<lower = 0, upper = pi()/2> lm0_sigma_unif;
  vector[nspecies] lm0_raw;
  vector[nspecies] lNEC_raw;
  real<lower = 0> ke;
  real<lower = 0> ks;
  // vector[nspecies] lke_raw;
}

transformed parameters {
  real lNEC_sigma;
  real lm0_sigma;

  vector[nspecies] lm0;
  vector[nspecies] lNEC;

  lNEC_sigma = 0.5 * tan(lNEC_sigma_unif);
  lm0_sigma = 0.5 * tan(lm0_sigma_unif);

  lm0 = lm0_mu + lm0_sigma * lm0_raw;
  lNEC = lNEC_mu + lNEC_sigma * lNEC_raw;

}

model {
  vector[nspecies] NEC;
  vector[nspecies] m0;
  vector[ndat] log_psurv;
  vector[ndat] psurv;
  vector[ndat] tNEC;
  vector[ndat] tref;

  # lke_sigma ~ cauchy(0,0.5); // half-Cauchy due to constraint to be positive
  # lks_sigma ~ cauchy(0,0.5);
  # lm0_sigma ~ cauchy(0,0.5);
  # lNEC_sigma ~ cauchy(0,0.5);

  for (i in 1:nspecies){
  ###############there is no vectorised version of the power function in STAN
    m0[i]=pow(10.,lm0[i]);
    NEC[i]=pow(10.,lNEC[i]);
  }

  for (i in 1:ndat)
  {

    log_psurv[i] = - m0[species[i]] * (t[i]-tprec[i]);

    if (x[i]>NEC[species[i]]){
        tNEC[i] = -1/ke * log(1-NEC[species[i]]/x[i]);

      if (t[i]>tNEC[i]){
        tref[i] = fmax(tprec[i],tNEC[i]);
        log_psurv[i] = log_psurv[i] - ks*( (x[i]-NEC[species[i]]) * (t[i]-tref[i]) + 1/ke * x[i] * exp(-ke*tref[i]) * ( exp(-ke * (t[i] - tref[i])) - 1) );
      }
    }

          psurv[i] = exp( log_psurv[i] );
          # psurv[i] = fmax(psurv[i],pow(10.,-6.));######this is to make initialization easier

  }


  lNEC_raw ~ normal(0, 1);
  lm0_raw ~ normal(0, 1);
  ke ~ normal(0.5 * (ke_min + ke_max), 0.25 * (ke_max - ke_min));
  ks ~ lognormal(0.5 * (log10(ks_min) + log10(ks_max)), 0.25 * (log10(ks_max) - log10(ks_min)));

  y ~ binomial(Nprec,psurv);
}
