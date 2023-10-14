transform_metadata_to_df <- function(metadata) {
  
  #A sub function to extract and process each entry
  # I stumbles on some problems with employing an anonymous function so I just did it like this:
  # R code to modify the process_entry function to handle Unix timestamp.
  
  process_entry <- function(x) {
    # Initialize formatted_datetime_str to NA
    formatted_datetime_str = NA
    
    if (!is.null(x$latestData$volumeByHour)) {
      if (grepl("^\\d+$", x$latestData$volumeByHour)) {
        # Convert from Unix timestamp
        datetime_str = as.POSIXct(as.numeric(x$latestData$volumeByHour), origin="1970-01-01", tz="UTC")
        formatted_datetime_str = format(datetime_str, format="%Y-%m-%d %H")
      } else if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}", x$latestData$volumeByHour)) {
        # Convert from ISO 8601 formatted date-time string
        datetime_str = lubridate::ymd_hms(x$latestData$volumeByHour, tz="UTC")
        formatted_datetime_str = format(datetime_str, format="%Y-%m-%d %H")
      }
    }
    
    # Create a tibble
    tibble::tibble(
      id = x$id,
      name = x$name,
      latestData = formatted_datetime_str, 
      lat = x$location$coordinates$latLon$lat,
      lon = x$location$coordinates$latLon$lon   
    )
  }
  
  # Sub-function to safely convert list entry to dataframe row
  #Again, i wasnt able to make the safely function work so did it the manual way.
  safe_to_data_frame <- function(x) {
    df <- data.frame(
      id = ifelse(is.null(x$id), NA, x$id),
      name = ifelse(is.null(x$name), NA, x$name),
      latestData = ifelse(is.null(x$latestData), NA, x$latestData),
      lat = ifelse(is.null(x$lat), NA, x$lat),
      lon = ifelse(is.null(x$lon), NA, x$lon),
      stringsAsFactors = FALSE
    )
    return(df)
  }
  
  # Process each element in the metadata
  #I use the functions above to create a list with the values from the input list
  list_data <- lapply(metadata$trafficRegistrationPoints, process_entry)
  
  # Convert the new list into a df
  df <- do.call(rbind, lapply(list_data, safe_to_data_frame))
  
  return(df)
}



to_iso8601 <- function(datetime_str, offset_days) {
  # Convert the string to a POSIXct
  datetime_obj = as.POSIXct(datetime_str, format="%Y-%m-%d %H", tz="UTC")
  
  # Add the offset in days
  adjusted_datetime_obj = datetime_obj + as.difftime(offset_days, units="days")
  
  # Convert the adjusted object to the desired ISO8601 format
  formatted_str = format(adjusted_datetime_obj, format="%Y-%m-%dT%H:%M:%SZ")
  
  return(formatted_str)
}

#test
sample_datetime = "2023-10-05 10"
offset_sample = 2
converted_datetime_with_offset = to_iso8601(sample_datetime, offset_sample)
print(converted_datetime_with_offset)




