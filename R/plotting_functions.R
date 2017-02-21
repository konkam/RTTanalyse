#' @export
plot_raw_data <- function(renamed_dataset){

  renamed_dataset <- renamed_dataset %>%
    assert_dataset_format() %>%
    subset(!is.na(number_alive))

  tid <- "test_id" %in% names(renamed_dataset)

  if (tid){
    p <- renamed_dataset %>%
      ggplot(aes(x = time,
                 y = number_alive,
                 colour = concentration,
                 group = interaction(concentration, test_id)))
  }
  else{
    p <- renamed_dataset %>%
      ggplot(aes(x = time,
                 y = number_alive,
                 colour = concentration,
                 group = concentration))
  }

  p +
    geom_line() +
    facet_wrap(~ species, scales = "free") +
    geom_point() +
    scale_y_continuous(breaks= scales::pretty_breaks(), limits = c(0, NA)) +
    # scale_y_continuous(limits = c(0, NA), labels = function (x) floor(x)) +
    ggthemes::theme_few() +
    ylab("Number alive") +
    xlab("Time") +
    scale_color_continuous(name = "Concentration")%>% # +
    # ggthemes::scale_colour_ptol() %>%
    return
}

#' @export
plot_raw_dataset = function(raw_wide_dataset, concentration_col, species_col, time_col_names, time_stamps){

  raw_wide_dataset %>%
    convert_concentrations_to_numbers(col_conc = concentration_col) %>%
    .[,c('concentration', species_col, time_col_names)] %>%
    setNames(c('concentration', 'species', time_stamps) %>% as.character()) %>%
    mutate(test_id = seq_along(concentration)) %>%
    gather(key = time, value = number_alive, -concentration, -species, -test_id) %>%
    plot_raw_data()

}


dhalf_Cauchy = function(x, mu, sigma){2/(pi*sigma*(1+((x-mu)/sigma)**2))}

#' @export
prior_posterior_plot_with_prepared_dataset = function(fit, prepared_dataset){

  watch_params = c('lks_mu','lks_sigma','lNEC_mu','lNEC_sigma','lke_mu','lke_sigma','lm0_mu','lm0_sigma')

  priors = list(
    tibble(grid_ = seq(-8,4,length.out = 100)) %>%
      mutate(lks_mu = dunif(grid_,-7,2)) %>%
      mutate(lke_mu = lks_mu) %>%
      mutate(lm0_mu = lks_mu) %>%
      gather(param, value, lks_mu:lm0_mu),
    tibble(grid_ = seq(-1,10, length.out = 100)) %>%
      mutate(lks_sigma = dhalf_Cauchy(grid_,0,2.5)) %>%
      mutate(lke_sigma = lks_sigma) %>%
      mutate(lm0_sigma = lks_sigma) %>%
      mutate(lNEC_sigma = lks_sigma) %>%
      gather(param, value, lks_sigma:lNEC_sigma),
    tibble(grid_ = seq(log10(prepared_dataset$minc)-2,log10(prepared_dataset$maxc)+2, length.out = 100)) %>%
      mutate(value = dunif(grid_,log10(prepared_dataset$minc)-1,log10(prepared_dataset$maxc)+1),
             param = 'lNEC_mu')
          ) %>%
    bind_rows()

  posterior = extract(fit, pars = watch_params) %>% as_tibble() %>%
    gather(param, value)

  posterior %>%
    ggplot(aes(x = value, y = ..density..)) +
    geom_histogram() +
    facet_wrap(~param, scales = "free") +
    geom_line(data = priors, aes(x = grid_, y = value), colour = 'red') +
    ggthemes::theme_few()
}

#' @export
plot_NEC_with_prepared_dataset = function(fit, prepared_dataset){

  posterior = extract(fit, pars = "lNEC") %>%
    .[[1]] %>%
    as_tibble() %>%
    setNames(seq(ncol(.)) %>%
               prepared_dataset$converter()) %>%
    gather(species, value)

  posterior %>%
    split(factor(.$species)) %>%
    lapply(FUN = function(y){
      y %>%
        .$value %>%
        hist(., plot = F, breaks = length(.)/50) %>%
        (function(z){
          z$mids[z %>%
                   .$counts %>%
                   (function(xx) xx/sum(xx)) %>%
                   HDIofGrid %>%
                   .$indices] %>%
            tibble(lNEC = ., species = y$species[[1]], l_est = z$mids[z$density %>% which.max])
        })
    }) %>%
    bind_rows() %>%
    (function(zzz){
      zzz %>%
        group_by_and_summarise_mult(group = 'species', sum_fun = function(xx) get_all_connex_subsets(xx$lNEC)) %>%
        rename(species = group) %>%
        inner_join(zzz %>% select(-lNEC) %>% unique)
    }) %>%
    (function(to_plot){
      to_plot %>%
        ggplot(aes(x = 10**y0, xend = 10**y1, y = factor(species), yend = factor(species))) +
        geom_segment(lineend = 'square', size  = 2) +
        geom_point(aes(x = 10**l_est), colour = 'red') +
        scale_x_log10() +
        ylab('') +
        xlab('NEC') +
        ggthemes::theme_few()
      })

}
