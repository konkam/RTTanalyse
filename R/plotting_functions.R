#' @export
plot_raw_data <- function(renamed_dataset, vs = "t"){

  renamed_dataset <- renamed_dataset %>%
    assert_dataset_format() %>%
    subset(!is.na(number_alive))

  tid <- "test_id" %in% names(renamed_dataset)


  if (vs == "t"){

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

    p = p +
      xlab("Time") +
      geom_line() +
      scale_color_continuous(name = "Concentration")
  }

  else{

      if ( (renamed_dataset$time %>% unique() %>% length()) < 12 ){

        p <- renamed_dataset %>%
          ggplot(aes(x = concentration,
                     y = number_alive,
                     colour = time %>% factor(),
                     group = time %>% factor())) +
          ggthemes::scale_color_ptol(name = "Time")

      }

      else{
        p <- renamed_dataset %>%
          ggplot(aes(x = concentration,
                     y = number_alive,
                     colour = time %>% as.numeric(),
                     group = time %>% factor())) +
          scale_color_continuous(name = "Time")
      }

    p = p +  xlab("Concentration")


    if (min(renamed_dataset$concentration) > 0){
      p = p + scale_x_log10()
    }
  }

  p +
    facet_wrap(~ species, scales = "free") +
    geom_point() +
    scale_y_continuous(breaks= scales::pretty_breaks(), limits = c(0, NA)) +
    # scale_y_continuous(limits = c(0, NA), labels = function (x) floor(x)) +
    ggthemes::theme_few() +
    ylab("Number alive")

}

#' @export
plot_raw_dataset = function(raw_wide_dataset, concentration_col, species_col, time_col_names, time_stamps, vs = "t"){

  raw_wide_dataset %>%
    convert_concentrations_to_numbers(col_conc = concentration_col) %>%
    .[,c('concentration', species_col, time_col_names)] %>%
    setNames(c('concentration', 'species', time_stamps) %>% as.character()) %>%
    mutate(test_id = seq_along(concentration)) %>%
    gather(key = time, value = number_alive, -concentration, -species, -test_id) %>%
    plot_raw_data(vs = vs)

}


dhalf_Cauchy = function(x, mu, sigma){2/(pi*sigma*(1+((x-mu)/sigma)**2))}

#' @export
prior_posterior_plot_with_prepared_dataset = function(fit, prepared_dataset){

  watch_params = c('lks_mu','lks_sigma','lNEC_mu','lNEC_sigma','lke_mu','lke_sigma','lm0_mu','lm0_sigma')

  priors = list(
    tibble(grid_ = seq(-8,4,length.out = 100)) %>%
      mutate(lks_mu = dunif(grid_,-7,0)) %>%
      mutate(lke_mu = lks_mu) %>%
      gather(param, value, -grid_),
    tibble(grid_ = seq(-8,4,length.out = 100)) %>%
      mutate(lm0_mu = dunif(grid_,-7,-1)) %>%
      gather(param, value, -grid_),
    tibble(grid_ = seq(-1,10, length.out = 100)) %>%
      mutate(lks_sigma = dhalf_Cauchy(grid_,0,0.5)) %>%
      mutate(lke_sigma = lks_sigma) %>%
      mutate(lm0_sigma = lks_sigma) %>%
      mutate(lNEC_sigma = lks_sigma) %>%
      gather(param, value, -grid_),
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
        # scale_x_log10() +
        ylab('') +
        xlab('No Effect Concentration') +
        ggthemes::theme_few()
      })

}

#' @export
plot_the_fit_hierarchical = function(fit, vs = 'c', data_for_fit = prepare_the_data_hierarchical()){
# This is with median parameters
  colmedians = function(x){
    if(is.null(dim(x))|length(dim(x))==1) median(x)
    else apply(X = x, MARGIN = 2, FUN = median)
  }

  fill_along = function(x, npoints = 100, log_ = F){
    if(log_) 10**seq(from = min(x) %>% log10, to = max(x) %>% log10, length.out = npoints)
    else seq(from = min(x), to = max(x), length.out = npoints)
  }

  # mcmctot = fit %>%
  #   extract_params()
  # m0__ = mcmctot %>%
  #   lapply(FUN = colmedians) %>%
  #   .$lm0 %>%
  #   10**.

  median_species_params = fit %>% get_median_species_params()

  if(vs=='c'){

    if (min(data_for_fit$x) > 0){
      log_conc = T
    }
    else{
      log_conc = F
    }

    p = data_for_fit$species %>%
      unique %>%
      lapply(FUN = function(xx){
        filter = data_for_fit$species==xx
        format_res(x = data_for_fit$x[filter] %>% fill_along(log_ = log_conc, npoints = 200), ks = median_species_params$ks[xx], NEC = median_species_params$NEC[xx],
                   ke = median_species_params$ke[xx], m0_ = median_species_params$m0[xx], t = data_for_fit$t[filter] %>% unique) %>%
              mutate(species = data_for_fit$converter(xx))
          }) %>%
      Reduce(rbind,.) %>%
      ggplot(aes(x = x, y = psurv, colour = t, group = t)) +
      geom_line() +
      facet_wrap(~species, scales = 'free_x') +
      geom_point(data = data.frame(data_for_fit[c('x','y','Neff','t', 'species')]) %>% mutate(species = data_for_fit$converter(species)), mapping = aes(x = x, y = y/Neff)) +
      scale_color_continuous(low = 'darkgreen', high = 'yellow', name = "Time") +
      #               scale_color_discrete() +
      # scale_x_log10()+
      xlab('Concentration') +
      ylab("Survival probability") +
      ggthemes::theme_few()

    if (min(data_for_fit$x) > 0){
      p = p + scale_x_log10()
    }

    return(p)
  }

  else if(vs=='t'){
        data_for_fit$species %>%
          unique %>%
          lapply(FUN = function(xx){
            filter = data_for_fit$species==xx
            format_res(x = data_for_fit$x[filter] %>%
                         unique,
                       ks = median_species_params$ks[xx],
                       NEC = median_species_params$NEC[xx],
                       ke = median_species_params$ke[xx],
                       m0_ = median_species_params$m0[xx],
                       t = data_for_fit$t[filter] %>%
                         c(0) %>%
                         fill_along(log_ = F, npoints = 300) ) %>%
              mutate(species = data_for_fit$converter(xx))
          }) %>%
      Reduce(rbind,.) %>%
      ggplot(aes(x = t, y = psurv, colour = x, group = x)) +
      geom_line() +
      geom_point(data = data.frame(data_for_fit[c('x','y','Neff','t', 'species')]) %>%
                   mutate(species = data_for_fit$converter(species)),
                 mapping = aes(y = y/Neff)) +
      # scale_color_continuous(low = 'blue', high = 'red', trans = 'log10') +
      scale_color_continuous(name = "Concentration") +
      facet_wrap(~species, scales = 'free_x') +
      xlab('Time') +
      ylab("Survival probability") +
      ggthemes::theme_few()
    }
}

#' @export
# extract_params = function(fit){
#     fit %>%
#       rstan::extract() %>%
#       as_tibble() %>%
#       return()
# }

#' @export
extract_params_jags = function(fit){
  if(is(fit) == "mcmc.list"){
    fit %>%
      lapply(FUN = as_tibble) %>%
      bind_rows() %>%
      return
  }
  else if(is(fit) == "runjags"){
    fit$mcmc %>%
      lapply(FUN = as_tibble) %>%
      bind_rows() %>%
      return
  }
  else stop("unrecognized object")
}

extract_species_param = function(fit, param_name){
  if(is(fit) == "stanfit"){
    fit %>%
      rstan::extract() %>%
      .[[param_name]] %>%
      return
  }
  else if(is(fit) %in% c("runjags","mcmc.list")){
    mcmctot = extract_params_jags(fit)

    if (param_name %in% names(mcmctot)){
      mcmctot[[param_name]] %>%
        return
    }
    else {
      res = mcmctot %>%
      select(starts_with(paste(param_name, "[", sep='')))
      if (ncol(res)==0) stop("wrong parameter name")
      return(res)
    }
  }
}

get_median_species_params = function(fit){
  lm0s = fit %>% extract_species_param("lm0") %>% colmedians()
  lkss = fit %>% extract_species_param("lks") %>% colmedians()
  lkes = fit %>% extract_species_param("lke") %>% colmedians()
  lNECs = fit %>% extract_species_param("lNEC") %>% colmedians()

  tmp = list("m0" = 10^lm0s, "ks" = 10^lkss, "ke" = 10^lkes, "NEC" = 10^lNECs)

  nspecies = tmp %>% sapply(length) %>% max()

  tmp %>%
    lapply(FUN = function(x){if(length(x)==1){rep(x,nspecies)} else{x}}) %>%
    setNames(names(tmp)) %>%
    return()
}

plot_the_fit_one_species = function(fit, vs = 'c', data_for_fit){
  median_params = fit %>%
    extract_params %>%
    colmedians() %>%
    (function(df){
      if( !("NEC" %in% names(df)) ) df %>% c("NEC" = 10**df[["lNEC"]])
      else df
    }) %>%
    (function(df){
      if( !("m0" %in% names(df)) ) df %>% c("m0" = 10**df[["lm0"]])
      else df
    }) %>%
    (function(df){
      if( !("ke" %in% names(df)) ) df %>% c("ke" = 10**df[["lke"]])
      else df
    }) %>%
    (function(df){
      if( !("ks" %in% names(df)) ) df %>% c("ks" = 10**df[["lks"]])
      else df
    })

  if(vs=='c'){

    if (min(data_for_fit$x) > 0){
      log_conc = T
    }
    else{
      log_conc = F
    }
    p = format_res(x = data_for_fit$x %>% fill_along(log_ = log_conc, npoints = 200),
                   ks = median_params["ks"],
                   NEC = median_params["NEC"],
                   ke = median_params["ke"],
                   m0_ = median_params["m0"],
                   t = data_for_fit$t %>% unique) %>%
      ggplot(aes(x = x, y = psurv, colour = t, group = t)) +
      geom_line() +
      geom_point(data = data.frame(data_for_fit[c('x','y','Neff','t', 'species')]), mapping = aes(x = x, y = y/Neff)) +
      scale_color_continuous(low = 'darkgreen', high = 'yellow', name = "Time") +
      xlab('Concentration') +
      ylab("Survival probability") +
      ggthemes::theme_few()
    if (min(data_for_fit$x) > 0){
      p = p + scale_x_log10()
    }

    return(p)
  }

  else if(vs=='t'){
    format_res(x = data_for_fit$x %>% unique(),
               ks = median_params["ks"],
               NEC = median_params["NEC"],
               ke = median_params["ke"],
               m0_ = median_params["m0"],
               t = data_for_fit$t %>%
                 c(0)%>%
                 fill_along(log_ = F, npoints = 300)
    ) %>%
      ggplot(aes(x = t, y = psurv, colour = x, group = x)) +
      geom_line() +
      geom_point(data = data.frame(data_for_fit[c('x','y','Neff','t', 'species')]) %>%
                   mutate(species = data_for_fit$converter(species)),
                 mapping = aes(y = y/Neff)) +
      scale_color_continuous(name = "Concentration") +
      xlab('Time') +
      ylab("Survival probability") +
      ggthemes::theme_few()
  }

}
