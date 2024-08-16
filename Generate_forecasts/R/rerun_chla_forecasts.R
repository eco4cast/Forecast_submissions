# Code to re-run forecasts that were trained with inaccurate chl-a data

END <- as_date('2024-07-01') #Re-run if forecasts have not been re-run after this date, fixing calibration issue

# Dates of forecasts 
end_date <- as_date('2024-07-01') #Don't need to redo anything after this date
challenge_s3_region <- "data"
challenge_s3_endpoint <- "ecoforecast.org"

# for each theme, check if file is in bucket
missed_dates <- data.frame(date = as.character(paste0(seq.Date(as_date('2023-01-01'), 
                                                            to = as_date(end_date), 
                                                            by = 'day'), ' 00:00:00'))
)

models <- c("auto_adam", "ETS", "ARIMA", "TBATS", 
            "temp_lm", "temp_lm_all_sites",
            "humidity_lm", "humidity_lm_all_sites",
            "precip_lm", "precip_lm_all_sites")

for(model in models){
  source(paste0("./Generate_forecasts/", model,"/forecast_model.R"))
  
  for (i in 1:nrow(missed_dates)) {
    forecast_date <- as.Date(missed_dates$date[[i]])
    forecast_themes <- "aquatics"
    
    message(paste0("Running chl-a forecasts for: ", forecast_date))
    # Generate the forecasts
    tryCatch({
      generate_tg_forecast(forecast_date = forecast_date,
                           forecast_model = forecast_model,
                           model_themes = forecast_themes,
                           model_id = model_id,
                           noaa = F,
                           vars_manual = "chla")
    }, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})
  }
}
