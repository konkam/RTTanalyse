##########################in this model, mostly all the stochastic links have been vectorised.
###############deterministic links does not make much difference when vectorised.
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
  real<lower=-7,upper=2> lks_mu;
  real<lower=log(minc)/log(10)-1,upper=log(maxc)/log(10)+1> lNEC_mu;
  real<lower=-7,upper=2> lke_mu;
  real<lower = -7, upper = 2> lm0_mu;
# real<lower = 0, upper = 2> lks_sigma;
# real<lower = 0, upper = 2> lNEC_sigma;
# real<lower = 0, upper = 2> lke_sigma;
# real<lower = 0, upper = 2> lm0_sigma;
  real<lower = 0> lks_sigma;
  real<lower = 0> lNEC_sigma;
  real<lower = 0> lke_sigma;
  real<lower = 0> lm0_sigma;
  vector[nspecies] lm0;
  vector[nspecies] lks;
  vector[nspecies] lNEC;
  vector[nspecies] lke;
}


model {
  vector[nspecies] ks;
  vector[nspecies] NEC;
  vector[nspecies] ke;
  vector[nspecies] m0;
  vector[ndat] psurv;
  vector[ndat] tNEC;
  vector[ndat] tref;

  lke_sigma ~ cauchy(0,2.5); // half-Cauchy due to constraint to be positive
  lks_sigma ~ cauchy(0,2.5);
  lm0_sigma ~ cauchy(0,2.5);
  lNEC_sigma ~ cauchy(0,2.5);

  for (i in 1:nspecies){
###############there is no vectorised version of the power function in STAN
    m0[i]=pow(10.,lm0[i]);
    ks[i]=pow(10.,lks[i]);
    NEC[i]=pow(10.,lNEC[i]);
    ke[i]=pow(10.,lke[i]);
  }



  for (i in 1:ndat)
  {

    psurv[i] = exp(- m0[species[i]] * (t[i]-tprec[i]) );

    if (x[i]>NEC[species[i]]){
      tNEC[i] = -1/ke[species[i]]*log(1-NEC[species[i]]/x[i]);

      if (t[i]>tNEC[i]){
        tref[i] = fmax(tprec[i],tNEC[i]);
        psurv[i] = psurv[i]* exp(- ks[species[i]]*( (x[i]-NEC[species[i]]) * (t[i]-tref[i]) + 1/ke[species[i]] * x[i] * ( exp(-ke[species[i]]*t[i])-exp(-ke[species[i]]*tref[i])) ));
      }
    }
    psurv[i] = fmax(psurv[i],pow(10.,-6.));######this is to make initialization easier

  }


  lks ~ normal(lks_mu, lks_sigma);
  lNEC ~ normal(lNEC_mu, lNEC_sigma);
  lke ~ normal(lke_mu, lke_sigma);
  lm0 ~ normal(lm0_mu, lm0_sigma);

  y ~ binomial(Nprec,psurv);
}
