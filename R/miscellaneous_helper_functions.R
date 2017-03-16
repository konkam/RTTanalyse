get_all_connex_subsets = function(xx){
  scale_ = min(xx[-1]-xx[1:(length(xx)-1)])
  xx %>%
    (function(z) z/scale_) %>%
    (function(tt){
      mask1 = round((tt + 1) - c(tt[-1],tt[1]))!=0 #those not followed by a contiguous value
      mask2 = round((tt - 1) - c(tt[length(tt)],tt[1:(length(tt)-1)]))!=0#those not preceded by a contiguous value
      #       print(xx)
      #       print(tt)
      #       print(mask1)
      data.frame(y0 = tt[mask2],
                 y1 = (tt[mask1] + max(tt %>% abs)*.001))*scale_ %>%
        return
    }) %>%
    return
}

group_by_and_summarise_mult = function(df, group, sum_fun){
  df[[group]] %>%
    unique %>%
    lapply(FUN = function(x){
      df[df[[group]]==x,] %>%
        sum_fun %>%
        data.frame( ., group = x)
    }) %>%
    Reduce(rbind,.)
}


make_lgrid = function(x){
  lmin = min(x) %>%
    log10
  lmax = max(x) %>%
    log10
  seq(lmin, lmax, length.out = 1000) %>%
    10**.
}

colMedians = function(x){
  apply(X = x, MARGIN = 2, FUN = median)
}

format_res = function(x,ks,NEC,ke,m0_,t){
  expand.grid(x,t) %>%
    cbind(mapply(FUN = function(x__,t__) {psurv(x = x__,
                                                ks = ks,
                                                NEC = NEC,
                                                ke = ke,
                                                m0 = m0_,
                                                t = t__)}, .$Var1,.$Var2) ) %>%
    setNames(c('x','t','psurv'))
}

fsurv = function(x_,ks,NEC,ke,m0_,t_) { exp(-(m0_+ks*max(0,x_*(1-exp(-ke*t_)) - NEC))*t_)}

psurv = function(x,t,ke,ks,m0,NEC){
  p = exp(-m0 * t)
  if( x > NEC ){
    tNEC = -1/ke * log( 1 - NEC/x)
    if(t > tNEC){
      p = p * exp(- ks * ( (x - NEC) * (t-tNEC) + x/ke * ( exp(-ke*t) - exp(-ke*tNEC) ) ) )
    }
  }
  return(p)
}

colmedians = function(x){
  if(is.null(dim(x))|length(dim(x))==1) median(x)
  else apply(X = x, MARGIN = 2, FUN = median)
}

fill_along = function(x, npoints = 100, log_ = F){
  if(log_) 10**seq(from = min(x) %>% log10, to = max(x) %>% log10, length.out = npoints)
  else seq(from = min(x), to = max(x), length.out = npoints)
}

