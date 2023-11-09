source("./Generate_forecasts/auto_adam/forecast_model.R")

### Some code to fill in missing forecasts
# Dates of forecasts 
end_date <- paste(Sys.Date() - days(2), '00:00:00') #Yesterday's forecasts might not have been processed. Wait to redo
challenge_s3_region <- "data"
challenge_s3_endpoint <- "ecoforecast.org"

# for each theme, check if file is in bucket
this_year <- data.frame(date = as.character(paste0(seq.Date(as_date('2023-01-01'), 
                                                            to = as_date(end_date), 
                                                            by = 'day'), 
                                                   ' 00:00:00')))

for (theme in model_themes) {
  for (i in 1:nrow(this_year)) {
    forecast_file <- paste0(theme,"-", as_date(this_year$date[i]), '-tg_auto_adam.csv.gz')
    
    this_year[i,theme] <- suppressMessages(aws.s3::object_exists(object = file.path("raw", theme, forecast_file),
                                                                  bucket = "neon4cast-forecasts",
                                                                  region = challenge_s3_region,
                                                                  base_url = challenge_s3_endpoint))
  }
}

# which dates do you need to generate forecasts for?
missed_dates <- this_year
# any days that do not have all themes
missed_dates <- missed_dates[!rowSums(missed_dates[model_themes])==length(model_themes),]
# generate theme names
themes <- apply(missed_dates[-1], 1, function(x) list(names(which(!x))))
missed_dates$themes <- themes

for (i in 1:nrow(missed_dates)) {
  
  forecast_date <- missed_dates$date[[i]]
  forecast_themes <- missed_dates$themes[[i]][[1]]
  
  if(!identical(forecast_themes, c("beetles","ticks")) | wday(forecast_date, label=TRUE)=="Sun"){
    message(paste0("Running forecasts for ", forecast_date,". Themes: ", forecast_themes))
    # Generate the forecasts
    tryCatch({
      generate_tg_forecast(forecast_date = forecast_date,
                           forecast_model = forecast_model,
                           model_themes = forecast_themes,
                           model_id = model_id)
    }, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})
  }
}
