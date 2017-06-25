#' @export
#'
prepare_dat = function(preprocessed_dataset){
  preprocessed_dataset %>%
    (function(df){
      maxc = max(df$concentration)
      minc = min(df$concentration[df$concentration>0])#this can cause pb with the priors otherwise
      maxt = max(df$time)
      df %>%
        (function(x){

          converter_list = x$species %>%
            unique %>%
            (function(xx){
              c(xx,seq_along(xx)) %>%
                setNames(c(seq_along(xx),xx))
            })

          converter_function = function(xxx){ sapply(X = xxx, FUN = function(yy) converter_list[[as.character(yy)]]) }

          list( 'x' = x$concentration,
                'y' = x$number_alive,
                't' = x$time,
                'tprec' = x$tprec,
                'Nprec' = x$nprec,
                'Neff' = x$effective_number,
                'maxc' = maxc,
                'minc' = minc,
                'maxt' = maxt,
                'ndat' = length(x$concentration),
                'lNEC_prior_factor' = 3,
                'converter' = converter_function,
                'species' = x$species %>%
                  converter_function %>%
                  as.numeric,
                'nspecies' = x$species %>%
                  unique %>%
                  length,
                "delta_t_min" = x %>%
                  group_by(species) %>%
                  mutate(delta_t_min = time %>%
                           unique() %>%
                           sort %>%
                           diff %>%
                           min()) %>%
                  ungroup() %>%
                  .$delta_t_min %>%
                  min(),
                "delta_c_min" = x %>%
                  group_by(species) %>%
                  mutate(delta_c_min = x$concentration %>%
                           unique() %>%
                           sort %>%
                           diff %>%
                           min()) %>%
                  ungroup() %>%
                  .$delta_c_min %>%
                  min(),
                "delta_c_max" = x %>%
                  group_by(species) %>%
                  mutate(delta_c_max = x$concentration %>%
                           unique() %>%
                           sort %>%
                           diff %>%
                           max()) %>%
                  ungroup() %>%
                  .$delta_c_max %>%
                  max(),
                m0_max = -1/min(x$time) * log(0.5),
                m0_min = -1/max(x$time) * log(0.99),
                ke_max = -1/min(x$time) * log(0.001),
                ke_min = -1/max(x$time) * log(0.999)
          ) %>%
            (function(lst){
              append(lst,
                     list("ks_max" = -1/(lst[["delta_c_min"]] * min(x$time)) * log(0.001),
                     "ks_min" = -1/(lst[["delta_c_max"]] * max(x$time)) * log(0.999))
              )
            })
        })
    })

}
