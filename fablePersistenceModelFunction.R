
# Function carry out a random walk forecast
RW_daily_forecast <- function(site, var, h,
                              bootstrap = FALSE, boot_number = 200, 
                              transformation = 'none', verbose = TRUE,...) {
  # Work out when the forecast should start
  forecast_starts <- target %>%
    dplyr::filter(!is.na(observed) & siteID == site & variable == var) %>%
    # Start the day after the most recent non-NA value
    dplyr::summarise(start_date = max(time) + lubridate::days(1)) %>% # Date
    dplyr::mutate(h = (Sys.Date() - start_date) + h) %>% # Horizon value
    dplyr::ungroup()
  
  if (verbose == T) {
    message(
      site,
      ' ',
      var,
      ' forecast with transformation = ',
      transformation,
      ' and bootstrap = ',
      bootstrap
    )
  }
  
  # filter the targets data set to the site_var pair
  targets_use <- target %>%
    dplyr::filter(siteID == site,
                  variable == var) %>%
    tsibble::as_tsibble(key = c('variable', 'siteID'), index = 'time') %>%
    # add NA values up to today (index)
    tsibble::fill_gaps(.end = Sys.Date()) %>%
    # Remove the NA's put at the end, so that the forecast starts from the last day with an observation,
    # rather than today
    dplyr::filter(time < forecast_starts$start_date)
  
  if (nrow(targets_use) == 0) {
    message('no targets available, no forecast run')
    empty_df <- data.frame('variable' = character(),
                           'siteID' = character(),
                           '.model' = character(),
                           'time' = lubridate::ymd(),
                           '.rep' = character(),
                           '.sim' = numeric())
    
    return(empty_df)
    
  } else {
    if (transformation == 'log') {
      RW_model <- targets_use %>%
        fabletools::model(RW = fable::RW(log(observed)))
    }
    if (transformation == 'log1p') {
      RW_model <- targets_use %>%
        fabletools::model(RW = fable::RW(log1p(observed)))
    }
    if (transformation == 'none') {
      RW_model <- targets_use %>%
        fabletools::model(RW = fable::RW(observed))
    }
    if (transformation == 'sqrt') {
      RW_model <- targets_use %>%
        fabletools::model(RW = fable::RW(sqrt(observed)))
    }
    
    if (bootstrap == T) {
      forecast <- RW_model %>% fabletools::generate(
        h = as.numeric(forecast_starts$h),
        bootstrap = T,
        times = boot_number
      )
    }  else
      forecast <- RW_model %>% fabletools::forecast(h = as.numeric(forecast_starts$h))
    message('forecast finished')
    return(forecast)
  }
  
}