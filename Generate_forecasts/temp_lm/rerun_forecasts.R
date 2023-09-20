source("./Generate_forecasts/temp_lm/forecast_model.R")

### Some code to fill in missing forecasts
# Dates of forecasts 
end_date <- paste(Sys.Date() - days(2), '00:00:00') #Yesterday's forecasts might not have been processed. Wait to redo
challenge_s3_region <- "data"
challenge_s3_endpoint <- "ecoforecast.org"

# for each theme, check if file is in bucket
for (theme in model_themes) {
  this_year <- data.frame(date = as.character(paste0(seq.Date(as_date('2023-01-01'), to = as_date(end_date), by = 'day'), ' 00:00:00')),
                          exists = NA)
  
  for (i in 1:nrow(this_year)) {
    forecast_file <- paste0(theme,"-", as_date(this_year$date[i]), '-tg_temp_lm.csv.gz')
    
    this_year$exists[i] <- suppressMessages(aws.s3::object_exists(object = file.path("raw", theme, forecast_file),
                                                                  bucket = "neon4cast-forecasts",
                                                                  region = challenge_s3_region,
                                                                  base_url = challenge_s3_endpoint))
  }
  
  # which dates do you need to generate forecasts for?
  missed_dates <- this_year |> 
    filter(exists == F) |> 
    pull(date) |> 
    as_date()
  
  for (i in 1:length(missed_dates)) {
    
    forecast_date <- missed_dates[i]
    # Generate the forecasts
    tryCatch({
      generate_tg_forecast(forecast_date = forecast_date,
                           forecast_model = forecast_model,
                           model_themes = theme,
                           model_id = model_id)
    }, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})
    
    # Submit forecast!
    #neon4cast::submit(forecast_file = file.path('Forecasts', fARIMA_file),
    #                  ask = F, s3_region = 'data', s3_endpoint = 'ecoforecast.org')
  }
}