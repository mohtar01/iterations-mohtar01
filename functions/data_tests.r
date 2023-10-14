# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests

test_stations_metadata_colnames <-
  function(df) {
    
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  } # The purpose of this function is if to check that the column names are equal to
# the expencted column names, which are the ones in the example rows at the end of Problem 2

test_stations_metadata_nrows <-
  function(df) {
    
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    } # This function just checks for the total amount of rows of the dataframe
  }# If there are less than 10000 rows or more than 5000 the output is Pass, else Fail.

test_stations_metadata_coltypes <-
  function(df) {
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }# This function checks if the column specifications are the ones that are expected
  }#For example the "id" and "name" columns should be oftype character, and the rest of type "double"
  
test_stations_metadata_nmissing <-
  function(df) {
    max_miss_vals <- 200
    
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }#This function checks whether there are more or less than 200 missing values
  }# If more, Fail, and if less the output is Pass

test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }#This function checks if latestData column utilizes UTC timezone- Pass if yes and Fail if no.
  }


test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)
    test_stations_metadata_coltypes(df)
    test_stations_metadata_nmissing(df)
    test_stations_metadata_nrows(df)
    test_stations_metadata_latestdata_timezone(df)
  }
# This function should runs all the other functions above




