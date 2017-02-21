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
