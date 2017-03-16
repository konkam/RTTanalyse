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
  real<lower=0> m0_min;
  real<lower=0> m0_max;
  real<lower=0> ke_min;
  real<lower=0> ke_max;
  real<lower=0> ks_min;
  real<lower=0> ks_max;
}

parameters {
  real<lower=log(minc)/log(10)-1,upper=log(maxc)/log(10)+1> lNEC_mu;
  # real<lower = 0, upper = 2> lNEC_sigma;
  real<lower = 0, upper = pi()/2> lNEC_sigma_unif;
  real<lower = 0> m0;
  real<lower = 0> ks;
  real<lower = 0> ke;
  vector[nspecies] lNEC_raw;
}

transformed parameters {
  real lNEC_sigma;
  vector[nspecies] lNEC;
  lNEC_sigma = 0.5 * tan(lNEC_sigma_unif); // lNEC_sigma sim cauchy(0,0.5)
  lNEC = lNEC_mu + lNEC_sigma * lNEC_raw;

}

model {
  vector[nspecies] NEC;
  vector[ndat] log_psurv;
  vector[ndat] psurv;
  vector[ndat] tNEC;
  vector[ndat] tref;

  for (i in 1:nspecies){
  ###############there is no vectorised version of the power function in STAN
    NEC[i]=pow(10.,lNEC[i]);
  }

  for (i in 1:ndat)
  {

    log_psurv[i] = - m0 * (t[i]-tprec[i]);

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
  m0 ~ lognormal(0.5/log(10) * (log(m0_min) + log(m0_max)), 0.125/log(10) * (log(m0_max) - log(m0_min)));
  ke ~ lognormal(0.5/log(10) * (log(ke_min) + log(ke_max)), 0.125/log(10) * (log(ke_max) - log(ke_min)));
  ks ~ lognormal(0.5/log(10) * (log(ks_min) + log(ks_max)), 0.125/log(10) * (log(ks_max) - log(ks_min)));

  y ~ binomial(Nprec,psurv);
}
