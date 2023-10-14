transform_metadata_to_df <- function(metadata) {
  
  #A sub function to extract and process each entry
  # I stumbles on some problems with employing an anonymous function so I just did it like this:
  process_entry <- function(x) {
    datetime_str = gsub("T", " ", substr(x$latestData$volumeByHour, 1, 19)) # Remove 'T' and get 'YYYY-MM-DD HH:MM:SS' format
    list(
      id = x$id,
      name = x$name,
      date = format(as.POSIXct(datetime_str, format="%Y-%m-%d %H:%M:%S", tz="UTC"), "%Y-%m-%d %H:%M:%S"), # Convert to POSIXct with UTC timezone and then format as string
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
      date = ifelse(is.null(x$date), NA, x$date),
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


