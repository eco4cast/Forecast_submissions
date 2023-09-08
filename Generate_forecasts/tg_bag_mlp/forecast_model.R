# Bag mlp - mtry and min_n parameters tuned on all target historical data (date of tuning on file name in tg_bag_mlp/trained_models/) and final fit to forecast 
# Trained entirely on available meteorological observations at each site


#### Step 0: load packages
library(here)
library(tidyverse)
library(tidymodels)
library(neon4cast)
library(lubridate)
library(rMR)
library(glue)
library(decor)
library(tsibble)
library(fable)
library(arrow)
library(bundle)
library(parsnip)
library(recipes)
library(magrittr)
library(baguette)
#library(ranger)
here::i_am("Forecast_submissions/Generate_forecasts/tg_bag_mlp/forecast_model.R")
source(here("Forecast_submissions/download_target.R"))
source(here("Forecast_submissions/ignore_sigpipe.R"))  #might fail locally, but necessary for git actions to exit properly or something
source("./Generate_forecasts/R/load_met.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks") #By default, run model across all themes, except terrestrial 30min (not currently configured)
model_id = "tg_bag_mlp"

#### Define the forecast model for a site
forecast_model <- function(site,
                           noaa_past_mean,
                           noaa_future_daily,
                           target_variable,
                           target,
                           horiz,
                           step,
                           theme,
                           forecast_date) {
  
  message(paste0("Running site: ", site))
  
  mod_file <- list.files(here("Forecast_submissions/Generate_forecasts/tg_bag_mlp/trained_models/"), pattern = paste(theme, site, target_variable, sep = "-"))
  
  if(length(mod_file)==0){
    message(paste0("No trained model for site ",site,". Skipping forecasts at this site."))
    return()
    
    } else {

    #  Get 30-day predicted temperature ensemble at the site
    noaa_future <- noaa_future_daily%>%
      filter(site_id == site)|>
      drop_na() #dropping NAs necessary for some models to run
    
  #generate predictions with trained model

    
    mod_fit <- readRDS(here(paste0("Forecast_submissions/Generate_forecasts/tg_bag_mlp/trained_models/",mod_file)))
    
    predictions <- predict(unbundle(mod_fit),new_data = noaa_future)%>%
      rename(prediction = ".pred")
    
    variables <- c('air_temperature',
                   "surface_downwelling_longwave_flux_in_air",
                   "surface_downwelling_shortwave_flux_in_air",
                   "precipitation_flux",
                   "air_pressure",
                   "relative_humidity",
                   "air_temperature",
                   "northward_wind",
                   "eastward_wind")
    
    forecast <- noaa_future %>% 
      bind_cols(predictions) |> 
      mutate(site_id = site,
             variable = target_variable)
  
    # Format results to EFI standard
    forecast <- forecast |>
      mutate(reference_datetime = forecast_date,
             family = "ensemble",
             model_id = model_id) |>
      dplyr::select(model_id, datetime, reference_datetime,
             site_id, family, parameter, variable, prediction)
    return(forecast)
  }
}

generate_tg_forecast(forecast_date = Sys.Date(),
                     forecast_model = forecast_model,
                     model_themes = model_themes,
                     model_id = model_id)

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
    forecast_file <- paste0(theme,"-", as_date(this_year$date[i]), '-tg_bag_mlp.csv.gz')
    
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