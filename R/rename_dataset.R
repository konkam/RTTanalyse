#' Specify which are the interesting column of a dataset and rename them for standardized use.
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
#' @export
rename_dataset <- function(dataset,
                           concentration_col_name,
                           time_col_name,
                           n_alive_col_name,
                           species_identifier_col_name,
                           test_id_col_name = NULL, keep_only = FALSE){

  final_columns <- c("number_alive", "concentration", "time", "species")

  renamed_dataset <- dataset %>%
    rename_(number_alive = n_alive_col_name,
            concentration = concentration_col_name,
            time = time_col_name,
            species = species_identifier_col_name)

  if (!is.null(test_id_col_name)){
    renamed_dataset <- renamed_dataset %>%
      rename_(test_id = test_id_col_name)
    final_columns = c(final_columns, "test_id")
  }

  if (keep_only){
    renamed_dataset <- renamed_dataset[, final_columns]
  }

  renamed_dataset %>% return
}
