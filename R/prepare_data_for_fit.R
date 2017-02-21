#' @export
#'
prepare_dat = function(preprocessed_dataset){
  preprocessed_dataset %>%
    (function(df){
      maxc = max(df$concentration)
      minc = min(df$concentration[df$concentration>0])#this can cause pb with the priors otherwise
      df %>%
        (function(x){
          converter_list = x$species %>%
            unique %>%
            (function(xx){
              c(xx,seq_along(xx)) %>%
                setNames(c(seq_along(xx),xx))
            } )
          converter_function = function(xxx){ sapply(X = xxx, FUN = function(yy) converter_list[[as.character(yy)]]) }
          list( 'x' = x$concentration,
                'y' = x$number_alive,
                't' = x$time,
                'tprec' = x$tprec,
                'Nprec' = x$nprec,
                'Neff' = x$effective_number,
                'maxc' = maxc,
                'minc' = minc,
                'ndat' = length(x$concentration),
                'lNEC_prior_factor' = 3,
                'converter' = converter_function,
                'species' = x$species %>%
                  converter_function %>%
                  as.numeric,
                'nspecies' = x$species %>%
                  unique %>%
                  length
          )
        })
    })

}
