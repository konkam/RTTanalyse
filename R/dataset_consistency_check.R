#' Check that the number of survivors does not increase
#'
#' @inheritParams get_mask_for_unsorted_rows
#' @return \code{TRUE} if the number of survivors does not increase, else \code{FALSE}
#' @export
check_that_the_number_of_survivors_does_not_increase <- function(raw_wide_dataset, time_col_names, time_stamps){

  !any(get_mask_for_unsorted_rows(raw_wide_dataset, time_col_names, time_stamps)) %>%
    return
}

#' Get boolean mask for unsorted rows
#'
#' @param raw_wide_dataset The loaded dataset.
#' @param concentration_col_name The name of the column containing the concentrations.
#' @param time_col_names The name of the column containing number of animals alive at for a specific time.
#' @param time_stamps The dates at which the number of animals alive is measured, in the same order as time_col_names
#' @return A mask vector with \code{TRUE} for unsorted rows and \code{false} for sorted rows.
#' @examples
#'
get_mask_for_unsorted_rows = function(raw_wide_dataset, time_col_names, time_stamps){

  ordered_cols_times = time_col_names[order(time_stamps, decreasing = T)]

  raw_wide_dataset %>%
    .[, ordered_cols_times] %>%
    mutate_all(as.numeric) %>%
    apply(MARGIN = 1, FUN = function(vec){vec %>%
        # rev %>%
        is.unsorted(na.rm = TRUE, strictly = FALSE)}) %>%
    unlist %>%
    return
}

#' Get the rows where the number of survivors increases
#'
#' @inheritParams get_mask_for_unsorted_rows
#' @return The rows where the number of survivors increases.
get_rows_where_the_number_of_survivors_increases = function(raw_wide_dataset, time_columns, time_stamps){

  is_unsorted_mask = get_mask_for_unsorted_rows(raw_wide_dataset, time_col_names, time_stamps)

  raw_wide_dataset %>%
    subset(is_unsorted_mask) %>%
    return
}

#' Check that species are tested for more than one concentration
#'
#' @param dataset The loaded dataset. Must contain only one contaminant.
#' @param concentration_col_name The name of the column containing the concentrations.
#' @param time_col_name The name of the column containing the time stamps.
#' @param n_alive_col_name The name of the column containing the number of animals alive.
#' @param species_identifier_col_name The name of the column containing a single identifier per species.
#' @param test_id_col_name The name of the column containing a identifier to dis-ambiguate several tests for the same species.
#' @param keep_only Choose whether to keep the whole dataset or only the necessary columns
#' @return The dataset with columns \code{number_alive}, \code{concentration}, \code{time} and \code{species} and eventually the \code{test_id}.
#' @examples
#' library(tidyverse)
#' dataset = read_csv('../data/RTT_dataV2.csv') %>%
#'     setNames(nm = sapply(X = names(.), FUN = function(xx) gsub(' ','_',xx)))
#'
#'Finish when some more pre-processing is done
#'
#'dataset %>%
#'    rename()
check_that_species_are_tested_for_more_than_one_concentration = function(wide_dataset){
  print("todo")
}
