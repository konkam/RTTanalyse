assert_dataset_format <- function(renamed_dataset){

  tid <- "test_id" %in% names(renamed_dataset)

  res <- renamed_dataset %>%
    mutate(time = as.numeric(time),
           concentration = as.numeric(concentration),
           number_alive = as.numeric(number_alive),
           species = as.character(species)
           )

  if (tid){
    res %>%
      mutate(test_id = as.character(test_id)) %>%
      return()
  }
  else{
    res %>%
      return()
  }
}
