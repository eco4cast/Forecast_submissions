# tg_humidity_lm_all_sites model
# written by ASL, 21 Jan 2023



#### Step 0: load packages

library(tidyverse)
library(neon4cast)
library(lubridate)
library(rMR)
library(glue)
source("ignore_sigpipe.R")
library(tsibble)
library(fable)
library(arrow)
source("download_target.R")
source("./Generate_forecasts/R/load_met.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks") #By default, run model across all themes, except terrestrial 30min (not currently configured)
model_id = "tg_humidity_lm_all_sites"

#### Define the forecast model for a site
forecast_model <- function(target_variable,
                           sites,
                           noaa_past_mean,
                           noaa_future_daily,
                           target,
                           horiz,
                           step,
                           theme,
                           forecast_date) {
  
  message(paste0("Running ",target_variable," at all sites"))
  
  # Get site information for elevation
  #site_info <- site_data |> dplyr::filter(field_site_id == site)
  
  # Merge in past NOAA data into the targets file, matching by date and site.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable %in% c(target_variable), 
                  site_id %in% sites) |> 
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean%>%
                       filter(site_id %in% sites), by = c("datetime", "site_id"))
  
  if(sum(!is.na(site_target$relative_humidity)&!is.na(site_target[target_variable]))==0){
    message(paste0("No historical humidity data that corresponds with target observations. Skipping forecasts for this variable."))
    return()
    
  } else {
    # Fit linear model based on past data: target variable = m * humidity + b
    fit <- lm(get(target_variable) ~ relative_humidity+site_id, data = site_target)
    good_sites = unique(site_target$site_id[!is.na(site_target$relative_humidity)&!is.na(site_target[target_variable])])
    
    #  Get 30-day predicted humidity ensemble at the site
    noaa_future <- noaa_future_daily%>%
      filter(site_id%in%sites)
    
    # use the linear model (predict.lm) to forecast target variable for each ensemble member
    forecast <- 
      noaa_future |> 
      filter(site_id %in% good_sites) |>
      mutate(prediction = predict(fit, tibble(relative_humidity, site_id)),
             variable = target_variable)
    
    # Format results to EFI standard
    forecast <- forecast |>
      mutate(reference_datetime = forecast_date,
             family = "ensemble",
             model_id = model_id) |>
      select(model_id, datetime, reference_datetime,
             site_id, family, parameter, variable, prediction)
  }
}

generate_tg_forecast(forecast_date = Sys.Date(),
                     forecast_model = forecast_model,
                     model_themes = model_themes,
                     model_id = model_id,
                     all_sites = T)

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
    forecast_file <- paste0(theme,"-", as_date(this_year$date[i]), '-tg_humidity_lm_all_sites.csv.gz')
    
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
                           model_id = model_id,
                           all_sites = T)
    }, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})
    
    # Submit forecast!
    #neon4cast::submit(forecast_file = file.path('Forecasts', fARIMA_file),
    #                  ask = F, s3_region = 'data', s3_endpoint = 'ecoforecast.org')
  }
}