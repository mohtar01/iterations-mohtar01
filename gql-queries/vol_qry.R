library(glue)

vol_qry <- function(id, from, to) {
  query <- glue::glue('
  {{
    trafficData(trafficRegistrationPointId: "{id}") {{
      volume {{
        byHour(from: "{from}", to: "{to}") {{
          edges {{
            node {{
              from
              to
              total {{
                volumeNumbers {{
                  volume
                }}
              }}
            }}
          }}
        }}
      }}
    }}
  }}
  ', id = id, from = from, to = to)
  
  return(query)
}


#test

a <-GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)
a


#Task5
transform_volumes <- function(data) {
  # Extracting the relevant lists containing the time and volume data
  volumes_list <- data$trafficData$volume$byHour$edges
  
  # Using purrr's map to extract 'from' and 'volume' and then converting the list to a dataframe
  df <- purrr::map_dfr(volumes_list, ~{
    tibble::tibble(
      from = lubridate::ymd_hms(.x$node$from, tz="UTC"),
      volume = .x$node$total$volumeNumbers$volume
    )
  })
  
  return(df)
}
