rpi_mu = function(n, dat){
  runif(n = n, min = log10(dat$minc) - 1, max = log10(dat$maxc)+1)
}

rpi_sigma = function(n){
  rcauchy(n = n, location = 0, scale = .5) %>% abs
}

#' @export
rpi_lNEC = function(n, dat){
  rnorm(n = n, mean = rpi_mu(n = n, dat = dat), sd = rpi_sigma(n = n))
}

rpi_NEC = function(n, dat){
  10 ** rpi_lNEC(n = n, dat = dat)
}

get_density = function(sample){

  dens_ = density(sample)

  tibble(xs = dens_$x, dens = dens_$y)
}

get_density_grid = function(sample, grid){

  ft = sample %>%
    fit_kernel_mixture

  tibble(xs = grid, dens = ft$dmixkernel(grid))

}

# source('mixnorm.R')

fit_kernel_mixture = function(smpl){
    res = list()

    K = function(x){
      1/sqrt(2*pi) * exp(-x^2/2)
    }



    n = length(smpl)

    hn = 1.06 * sd(smpl) * n^(-1/5) # Silverman (1986) AMISE

    dmixkernel_nonvec = function(x){
      1/(n*hn) * sum(sapply(X = smpl, FUN = function(xi) K((x-xi)/hn)))
    }

    pmixkernel_nonvec = function(x){
      1/(n) * sum(sapply(X = smpl, FUN = function(xi) pnorm(q = (x-xi)/hn, mean = 0, sd = 1)))
    }

    rmixkernel = function(ndata){
      rmixnorm(n = ndata, mus = smpl, sigmas = hn, probs = 1/n*rep(1,n))
    }

    res   %>%
      (function(x){
        x$dmixkernel = function(x) sapply(x, dmixkernel_nonvec)
        x$pmixkernel = function(x) sapply(x, pmixkernel_nonvec)
        x$rmixkernel = rmixkernel
        x$qmethod = 'normal_kernel_mixture'
        x$data_ = smpl
        x %>%
          return}) %>%
      return()
}
