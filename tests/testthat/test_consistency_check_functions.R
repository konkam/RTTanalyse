library(tidyverse)
# context("dataset consistency check functions")

concentration_col = "Concentration"
species_col = "Identifier"
time_cols = c("0h", "24h", "48", "72", "R96h")
time_stamps = c(0,24,48,72,96)

test_that("The functions recognise datasets which do not increase", {
  expect_true(read_csv('../../../data/RTT_dataV2_ImidaclopridCorrected.csv') %>%
                check_that_the_number_of_survivors_does_not_increase(time_col_names = time_cols, time_stamps = time_stamps) )

  expect_false(read_csv('../../../data/RTT_dataV2_Malathion.csv') %>%
                check_that_the_number_of_survivors_does_not_increase(time_col_names = time_cols, time_stamps = time_stamps) )
})
