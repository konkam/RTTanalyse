library(tidyverse)
# context("rename dataset function")

dataset <- read_csv('../../../data/RTT_dataV2.csv') %>%
  setNames(nm = sapply(X = names(.), FUN = function(xx) gsub(' ','_',xx))) %>%
  # rename(Notes_b = `Notes b`) %>%
  mutate(unique_sp_id = mapply(paste, Order, Notes_b, Family, Genus, Species) %>% gsub('NA', '', .)) %>%
  gather(Time, n_alive, `0h`:R96h) %>%
  mutate(Time = sapply(FUN = function(x) {gsub('h','',x) %>% gsub('R','',.)} , Time)) %>%
  mutate(Time = as.numeric(Time)) %>%
  mutate(Concentration = sapply(FUN = function(x) {gsub('ug/L','',x) %>% gsub('R','',.)} , Concentration)) %>%
  mutate(Concentration = sapply(FUN = function(x) {gsub('Control','0',x) %>% gsub('R','',.)} , Concentration)) %>%
  mutate(Concentration = as.numeric(Concentration))

test_that("After renaming, gets the right number of columns", {
  expect_equal(rename_dataset(dataset = dataset, concentration_col_name = 'Concentration',
                              time_col_name = 'Time',
                              n_alive_col_name = 'n_alive',
                              species_identifier_col_name = 'Identifier', keep_only = TRUE) %>%
                 dim %>%
                 .[2], 4)
  expect_equal(rename_dataset(dataset = dataset, concentration_col_name = 'Concentration',
                              time_col_name = 'Time',
                              n_alive_col_name = 'n_alive',
                              species_identifier_col_name = 'Identifier',
                              test_id_col_name = 'Test_number', keep_only = TRUE) %>%
                 dim %>%
                 .[2], 5)
})
