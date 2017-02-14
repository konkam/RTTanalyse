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
    ggthemes::theme_few() %>% # +
    # ggthemes::scale_colour_ptol() %>%
    return
}
