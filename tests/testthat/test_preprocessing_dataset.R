library(tidyverse)
# context("dataset pre-processing functions")

dataset = read_csv('../../../data/RTT_dataV2_ImidaclopridCorrected.csv') %>%
  mutate(test_id = seq(nrow(.)))

concentration_col = "Concentration"
species_col = "Identifier"
time_cols = c("0h", "24h", "48", "72", "R96h")
time_stamps = c(0,24,48,72,96)
ordered_time_cols = time_cols[order(time_stamps)]

make_time_col_name_time_stamp_converter = function(time_col_names, time_stamps){
  cv = c(time_col_names, time_stamps) %>%
    setNames(c(time_stamps, time_col_names) %>% as.character())
  cv_fun = function(ids){
    sapply(ids, function(x) cv[x])
  }
  return(cv_fun)
}

time_col_name_time_stamp_converter = make_time_col_name_time_stamp_converter(time_col_names = time_cols, time_stamps = time_stamps)

long_dataset = gather_(dataset, "time", "number_alive", time_cols) %>%
  mutate(time = time_col_name_time_stamp_converter(time) %>% as.numeric()) %>%
  subset(!is.na(number_alive))



test_that("Gets some previous times right", {
  expect_equal(find_previous_measurement_time(long_dataset = long_dataset, test_id = 5, time = 96), 24)
  expect_equal(find_previous_measurement_time(long_dataset = long_dataset, test_id = 28, time = 96), 72)
})

test_that("Fails when given the first value", {
  expect_error(find_previous_measurement_time(long_dataset = long_dataset, test_id = 5, time = 0), "0 is already the first measurement time")
})

test_that("Gets some previous times right", {
  expect_equal(find_previous_number_of_survivors(long_dataset = long_dataset, test_id = 5, find_previous_measurement_time(long_dataset = long_dataset, test_id = 5, time = 96)), 11)
})
