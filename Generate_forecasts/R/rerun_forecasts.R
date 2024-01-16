END <- as_date('2023-11-13') #Re-run if forecasts have not been re-run after Nov 13, fixing calibration issue

rerun_forecasts <- function(model_id, forecast_model, model_themes, END, noaa = T, all_sites = F) {
  ### Some code to fill in missing forecasts
  # Dates of forecasts 
  end_date <- paste(Sys.Date() - days(2), '00:00:00') #Yesterday's forecasts might not have been processed. Wait to redo
  challenge_s3_region <- "data"
  challenge_s3_endpoint <- "ecoforecast.org"
  
  # Get all the submissions 
  submissions <- aws.s3::get_bucket_df("bio230014-bucket01", 
                                       prefix = "challenges/forecasts/raw",
                                       region = "sdsc",
                                       base_url = "osn.xsede.org",
                                       max = Inf)
  
  # for each theme, check if file is in bucket
  this_year <- data.frame(date = as.character(paste0(seq.Date(as_date('2024-01-01'), 
                                                              to = as_date(end_date), 
                                                              by = 'day'), ' 00:00:00'))
  )
  
  for (theme in model_themes) {
    for (i in 1:nrow(this_year)) {
      forecast_file <- paste0(theme,"-", as_date(this_year$date[i]), '-', model_id, '.csv.gz')
      
      this_year[i,theme] <- nrow(dplyr::filter(submissions, stringr::str_detect(Key, forecast_file))) > 0
      
      if (this_year[i,theme]) {
        all_modified <- dplyr::filter(submissions, stringr::str_detect(Key, forecast_file))
        modified <- max(as_datetime(all_modified$LastModified))
        
        this_year[i,theme] <- ifelse(modified >= END, T, F)
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
    
    forecast_date <- as.Date(missed_dates$date[[i]])
    forecast_themes <- missed_dates$themes[[i]][[1]]
    
    message(paste0("Running forecasts for: ", forecast_date,
                   ".\nThemes: ", paste0(forecast_themes, collapse = ", "), "."))
    # Generate the forecasts
    tryCatch({
      generate_tg_forecast(forecast_date = forecast_date,
                           forecast_model = forecast_model,
                           model_themes = forecast_themes,
                           model_id = model_id,
                           noaa = noaa)
    }, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})
  }
}
