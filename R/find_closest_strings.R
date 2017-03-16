#' Find the closest pairs of strings from a vector of strings.
#'
#' This is using the Jaro-Winckler distance to identify the closest strings,
#' see https://journal.r-project.org/archive/2014-1/loo.pdf
#'
#' @param strings_ A vector of strings. If the vector contain duplicates strings, they will be removed.
#' @param quantile_ Proportion of the closest pairs of strings to return. For quantile_ = 0.1, the best 10 percent pairs are returned.
#'
#' @return A matrix containing the "quantile_" closest pairs of strings.
#'
#' @examples
#' library(tidyverse)
#' read_csv('../data/RTT_dataV2.csv') %>%
#'     .$Identifier %>%
#'     find_closest_strings
#'
#' @export
find_closest_strings = function(strings_, quantile_ = 0.01){

  print("The closest names are:")

  unique_strings = unique(strings_)
  stringdist::stringdistmatrix(unique_strings, method = "jw", p = 0.1) %>%
    (function(a){
      cut(a, quantile(a, probs=c(0, quantile_) ), include.lowest=TRUE)
      }) %>%
    as.numeric %>%
    is.na %>%
    (function(id_to_remove){
      combn(unique_strings,2)[,!id_to_remove]
    })
}

