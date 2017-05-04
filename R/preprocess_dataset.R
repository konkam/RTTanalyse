#' Find the previous measurement time for a given measurement time and test_id
#'
#' The long dataset is obtained using \code{gather_(dataset, "time", "number_alive", time_cols)}. test_id must by a variable identifying in an unique manner the replicate. The dataset must not contain NAs.
#'
#' @param long_dataset A dataset in the long format, with one line for each measurement in time.
#' @param test_id_ A unique identifier for each replicate.
#' @param time The time for which we want to compute the preceding time.
#' @return The previous time found in the dataset.
#' @examples
#' dataset = read_csv('data/RTT_dataV2_ImidaclopridCorrected.csv') %>%
#'  mutate(test_id = seq(nrow(.)))

#' concentration_col = "Concentration"
#' species_col = "Identifier"
#' time_cols = c("0h", "24h", "48", "72", "R96h")
#' time_stamps = c(0,24,48,72,96)
#' ordered_time_cols = time_cols[order(time_stamps)]

#' make_time_col_name_time_stamp_converter = function(time_col_names, time_stamps){
#'   cv = c(time_col_names, time_stamps) %>% setNames(c(time_stamps, time_col_names) %>% as.character())
#'   cv_fun = function(ids){
#'     sapply(ids, function(x) cv[x])
#'   }
#'   return(cv_fun)
#' }

#' time_col_name_time_stamp_converter = make_time_col_name_time_stamp_converter(time_col_names = time_cols, time_stamps = time_stamps)
#'
#' long_dataset = gather_(dataset, "time", "number_alive", time_cols) %>%
#' mutate(time = time_col_name_time_stamp_converter(time) %>% as.numeric()) %>%
#'   subset(!is.na(number_alive))
#'
#'   find_previous_measurement_time(long_dataset = long_dataset, test_id = 5, time = 96)
#'
find_previous_measurement_time = function(long_dataset, test_id_, time){
  long_dataset %>%
    subset(test_id == test_id_) %>%
    dplyr::select(time) %>%
    unlist %>%
    sort %>%
    (function(x) {
      id = which(x==time)
      if(id %>% length == 0) stop(paste('There is no measurement at',time,'H for test id:',test_id_))
      if(id==1) stop(paste(time, "is already the first measurement time"))
      else x[id-1] %>% as.numeric()
    }
    )
}


#' Find the previous measurement number of animals alive after having found the previous time
#'
#' The long dataset is obtained using \code{gather_(dataset, "time", "number_alive", time_cols)}. test_id must by a variable identifying in an unique manner the replicate. The dataset must not contain NAs.
#'
#' @param long_dataset A dataset in the long format, with one line for each measurement in time.
#' @param tprec The time for which we want to compute the preceding time.
#' @param test_id_ A unique identifier for each replicate.
#' @return The previous time found in the dataset.
#' @examples
#' dataset = read_csv('data/RTT_dataV2_ImidaclopridCorrected.csv') %>%
#'  mutate(test_id = seq(nrow(.)))

#' concentration_col = "Concentration"
#' species_col = "Identifier"
#' time_cols = c("0h", "24h", "48", "72", "R96h")
#' time_stamps = c(0,24,48,72,96)
#' ordered_time_cols = time_cols[order(time_stamps)]

#' make_time_col_name_time_stamp_converter = function(time_col_names, time_stamps){
#'   cv = c(time_col_names, time_stamps) %>% setNames(c(time_stamps, time_col_names) %>% as.character())
#'   cv_fun = function(ids){
#'     sapply(ids, function(x) cv[x])
#'   }
#'   return(cv_fun)
#' }

#' time_col_name_time_stamp_converter = make_time_col_name_time_stamp_converter(time_col_names = time_cols, time_stamps = time_stamps)
#'
#' long_dataset = gather_(dataset, "time", "number_alive", time_cols) %>%
#'   mutate(time = time_col_name_time_stamp_converter(time) %>% as.numeric()) %>%
#'   subset(!is.na(number_alive)) %>%
#'   subset(time>0)
#'
#'   find_previous_number_of_survivors(long_dataset = long_dataset, test_id = 5, find_previous_measurement_time(long_dataset = long_dataset, test_id = 5, time = 96))
#'
find_previous_number_of_survivors = function(long_dataset, tprec, test_id_){
  long_dataset %>%
    subset(test_id == test_id_&time==tprec) %>%
    .$number_alive %>%
    as.numeric()
}


add_prec_time_and_number = function(long_dataset){
  long_dataset %>%
    group_by(test_id) %>%
    mutate(first_time = min(time, na.rm = T)) %>%
    subset(time>first_time) %>%
    mutate(tprec = mapply(FUN = function(tid,t){find_previous_measurement_time(long_dataset = long_dataset, test_id_ = tid, time = t)}, test_id, time)) %>%
    mutate(nprec = mapply(FUN = function(tid,t){find_previous_number_of_survivors(long_dataset = long_dataset, test_id_ = tid, tprec = t)}, test_id, tprec)) %>%
    ungroup
}

compute_effective_number_of_individuals = function(long_dataset){
  long_dataset %>%
    group_by(test_id) %>%
    mutate(effective_number = max(nprec, na.rm = T)) %>% #This is for the case where there are no lost to follow up
    ungroup
}

convert_concentrations_to_numbers = function(df_in, col_conc){
  df = df_in
  df$concentration = df[, col_conc] %>%
    unlist()
  if (is.numeric(df$concentration)) return(df)
  else{
    df$concentration = df$concentration %>%
    stringr::str_extract("\\(?[0-9,.]+\\)?") %>%
    as.numeric() %>%
    ifelse(test = is.na(.), yes = 0, no = .)#Change this when the value of the control is given
  return(df)
    }
}


preprocess_long_dataset = function(long_dataset){
  long_dataset %>%
    convert_concentrations_to_numbers(col_conc = "concentration") %>%
    mutate(number_alive = as.numeric(number_alive)) %>%
    subset(!is.na(number_alive)) %>%
    add_prec_time_and_number %>%
    compute_effective_number_of_individuals
}

make_time_col_name_time_stamp_converter = function(time_col_names, time_stamps){
  cv = c(time_col_names, time_stamps) %>% setNames(c(time_stamps, time_col_names) %>% as.character())
  cv_fun = function(ids){
    sapply(ids, function(x) cv[x])
  }
  return(cv_fun)
}

convert_to_long_dataset = function(raw_wide_dataset, concentration_col, species_col, time_col_names, time_stamps){

  time_col_name_time_stamp_converter = make_time_col_name_time_stamp_converter(time_col_names = time_col_names, time_stamps = time_stamps)

  raw_wide_dataset %>%
    convert_concentrations_to_numbers(col_conc = concentration_col) %>%
    mutate(test_id = seq_along(concentration)) %>%
    gather_("time", "number_alive", time_col_names) %>%
    mutate(time = time_col_name_time_stamp_converter(time) %>% as.numeric()) %>%
    subset(!is.na(number_alive)) %>%
    mutate_(species = species_col)
}

#' @export
preprocess_dataset = function(raw_wide_dataset, concentration_col, species_col, time_col_names, time_stamps){
  raw_wide_dataset %>%
    convert_to_long_dataset(concentration_col = concentration_col, species_col = species_col, time_col_names = time_col_names, time_stamps = time_stamps) %>%
    preprocess_long_dataset
}
