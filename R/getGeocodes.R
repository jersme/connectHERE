geocodeAddress <- function(url = setURL(type = "geocoding"), app_id, app_code, address) {

  # create a API readable address string
  address <- gsub(pattern = " ", replacement = "%20", x = address)
  search_text <- paste0("searchtext=", address)

  # create full url
  call_url <- paste0(url, "geocode.json?", search_text ,"&app_id=", app_id, "&app_code=", app_code)

  # call api
  api_call <- httr::GET(url = call_url)

  # transfor results
  api_call_content <- httr::content(api_call, as = "text")
  api_call_json <- jsonlite::fromJSON(txt = api_call_content, flatten = TRUE)
  api_call_df <- as.data.frame(api_call_json$Response$View$Result)
  api_call_coord <- api_call_df[[9]][[1]]


  results <- list(matchLevel = api_call_df[1,2],
                  matchType = api_call_df[1,3],
                  matchQuality = api_call_df[1, 4:6],
                  coordinates = test_call_coord,
                  locationId = api_call_df[1,7],
                  locationType  = api_call_df[1,8],
                  mapView = api_call_df[1, 12:15],
                  addressLabel = api_call_df[1,16],
                  country = api_call_df[1,17],
                  state = api_call_df[1,18],
                  county = api_call_df[1,19],
                  city = api_call_df[1,20],
                  district = api_call_df[1,21],
                  street = api_call_df[1,22],
                  houseNumber = api_call_df[1,23],
                  postalCode = api_call_df[1,24])

  # return results
  return(results)
}



