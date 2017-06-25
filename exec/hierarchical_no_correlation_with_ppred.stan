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
}

parameters {
  real<lower=-7,upper=0> lks_mu;
  real<lower=log(minc)/log(10)-1,upper=log(maxc)/log(10)+1> lNEC_mu;
  real<lower=-7,upper=0> lke_mu;
  real<lower = -7, upper = -1> lm0_mu;
  # real<lower = 0, upper = 2> lks_sigma;
  # real<lower = 0, upper = 2> lNEC_sigma;
  # real<lower = 0, upper = 2> lke_sigma;
  # real<lower = 0, upper = 2> lm0_sigma;
  real<lower = 0, upper = pi()/2> lks_sigma_unif;
  real<lower = 0, upper = pi()/2> lNEC_sigma_unif;
  real<lower = 0, upper = pi()/2> lke_sigma_unif;
  real<lower = 0, upper = pi()/2> lm0_sigma_unif;
  vector[nspecies] lm0_raw;
  vector[nspecies] lks_raw;
  vector[nspecies] lNEC_raw;
  vector[nspecies] lke_raw;
}

transformed parameters {
  real lks_sigma;
  real lNEC_sigma;
  real lke_sigma;
  real lm0_sigma;

  vector[nspecies] lm0;
  vector[nspecies] lks;
  vector[nspecies] lNEC;
  vector[nspecies] lke;

  vector[nspecies] ks;
  vector[nspecies] NEC;
  vector[nspecies] ke;
  vector[nspecies] m0;
  vector[ndat] log_psurv;
  vector[ndat] psurv;
  vector[ndat] tNEC;
  vector[ndat] tref;


  lks_sigma = 0.5 * tan(lks_sigma_unif); // lks_sigma sim cauchy(0,0.5)
  lNEC_sigma = 0.5 * tan(lNEC_sigma_unif);
  lke_sigma = 0.5 * tan(lke_sigma_unif);
  lm0_sigma = 0.5 * tan(lm0_sigma_unif);

  lm0 = lm0_mu + lm0_sigma * lm0_raw;
  lks = lks_mu + lks_sigma * lks_raw;
  lNEC = lNEC_mu + lNEC_sigma * lNEC_raw;
  lke = lke_mu + lke_sigma * lke_raw;

  for (i in 1:nspecies){
  ###############there is no vectorised version of the power function in STAN
    m0[i]=pow(10.,lm0[i]);
    ks[i]=pow(10.,lks[i]);
    NEC[i]=pow(10.,lNEC[i]);
    ke[i]=pow(10.,lke[i]);
  }

  for (i in 1:ndat)
  {

    log_psurv[i] = - m0[species[i]] * (t[i]-tprec[i]);
    tNEC[i] = -1;
    tref[i] = -1;

    if (x[i]>NEC[species[i]]){
        tNEC[i] = -1/ke[species[i]] * log(1-NEC[species[i]]/x[i]);

      if (t[i]>tNEC[i]){
        tref[i] = fmax(tprec[i],tNEC[i]);
        log_psurv[i] = log_psurv[i] - ks[species[i]]*( (x[i]-NEC[species[i]]) * (t[i]-tref[i]) + 1/ke[species[i]] * x[i] * exp(-ke[species[i]]*tref[i]) * ( exp(-ke[species[i]] * (t[i] - tref[i])) - 1) );
      }
    }
          psurv[i] = exp( log_psurv[i] );
  }
}

model {
  lks_raw ~ normal(0, 1);
  lNEC_raw ~ normal(0, 1);
  lke_raw ~ normal(0, 1);
  lm0_raw ~ normal(0, 1);

  y ~ binomial(Nprec,psurv);
}

generated quantities {
  vector[ndat] yrep;

  for (i in 1:ndat) {
    // preferred Stan syntax as of version 2.10.0
    yrep[i] = binomial_rng(Nprec[i], psurv[i]);
  }
}
