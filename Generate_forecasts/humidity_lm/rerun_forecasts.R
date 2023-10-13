source("./Generate_forecasts/humidity_lm/forecast_model.R")

END <- as_date('2023-10-10') #Re-run if forecasts have not been re-run after Oct 10, fixing meteorology issue

### Some code to fill in missing forecasts
# Dates of forecasts 
end_date <- paste(Sys.Date() - days(2), '00:00:00') #Yesterday's forecasts might not have been processed. Wait to redo
challenge_s3_region <- "data"
challenge_s3_endpoint <- "ecoforecast.org"

# for each theme, check if file is in bucket
this_year <- data.frame(date = as.character(paste0(seq.Date(as_date('2023-01-01'), 
                                                            to = as_date(end_date), 
                                                            by = 'day'), ' 00:00:00'))
)

for (theme in model_themes) {
  for (i in 1:nrow(this_year)) {
    forecast_file <- paste0(theme,"-", as_date(this_year$date[i]), '-tg_humidity_lm.csv.gz')
    
    this_year[i,theme] <- suppressMessages(aws.s3::object_exists(object = file.path("raw", theme, forecast_file),
                                                                 bucket = "neon4cast-forecasts",
                                                                 region = challenge_s3_region,
                                                                 base_url = challenge_s3_endpoint))
    
    if (this_year[i,theme]) {
      modified <- attr(suppressMessages(aws.s3::head_object(object = file.path("raw", theme, forecast_file),
                                                            bucket = "neon4cast-forecasts",
                                                            region = challenge_s3_region,
                                                            base_url = challenge_s3_endpoint)), 
                       "last-modified")
      
      this_year[i,theme] <- ifelse(parse_date_time(gsub('GMT', '', str_split_1(modified, ', ')[2]),
                                                   orders = "%d %b %Y %H:%M:%S") >= END, 
                                   T, F)
    }
  }
}

# which dates do you need to generate forecasts for?
missed_dates <- this_year
# don't need to run ticks and beetles except on Sunday
missed_dates[wday(missed_dates$date, label=TRUE) != "Sun", c("ticks", "beetles")] <- T
# any days that do not have all themes
missed_dates <- missed_dates[!rowSums(missed_dates[model_themes])==length(model_themes),]
# generate theme names
themes <- apply(missed_dates[-1], 1, function(x) list(names(which(!x))))
missed_dates$themes <- themes

for (i in 1:nrow(missed_dates)) {
  
  forecast_date <- missed_dates$date[[i]]
  forecast_themes <- missed_dates$themes[[i]][[1]]
  
  message(paste0("Running forecasts for: ", as.Date(forecast_date),
                 ".\nThemes: ", paste0(forecast_themes, collapse = ", "), "."))
  # Generate the forecasts
  tryCatch({
    generate_tg_forecast(forecast_date = forecast_date,
                         forecast_model = forecast_model,
                         model_themes = forecast_themes,
                         model_id = model_id)
  }, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})
}