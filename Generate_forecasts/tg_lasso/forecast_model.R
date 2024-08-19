# lasso model - regularization parameter tuned on all target historical data (date of tuning on file name in tg_lasso/trained_models/) and final fit to forecast 
# MAKE SURE TO CHANGE METADATA ONCE NUMBER OF VARIABLES SELECTED



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
library(ranger)
here::i_am("Forecast_submissions/Generate_forecasts/tg_lasso/forecast_model.R")
source(here("Forecast_submissions/download_target.R"))
source(here("Forecast_submissions/ignore_sigpipe.R"))  #might fail locally, but necessary for git actions to exit properly or something #TESTING
source("./Generate_forecasts/R/load_met.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks") #By default, run model across all themes, except terrestrial 30min (not currently configured)
model_id = "tg_lasso"



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
  
  variables <- c('air_temperature',
                 "surface_downwelling_longwave_flux_in_air",
                 "surface_downwelling_shortwave_flux_in_air",
                 "precipitation_flux",
                 "air_pressure",
                 "relative_humidity",
                 "air_temperature",
                 "northward_wind",
                 "eastward_wind")
  
  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable %in% c(target_variable), 
                  site_id == site,
                  datetime < forecast_date) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean%>%
                       filter(site_id == site), 
                     by = c("datetime", "site_id"))
  
  
  #  Get 30-day predicted NOAA ensemble at the site
  noaa_future <- noaa_future_daily%>%
    filter(site_id == site)
  
  # Call saved model fits
  mod_file <- list.files(here("Forecast_submissions/Generate_forecasts/tg_lasso/trained_models/"), pattern = paste(theme, site, target_variable, sep = "-"))
  
  if(!file_test("-f", here(paste0("Forecast_submissions/Generate_forecasts/tg_lasso/trained_models/",mod_file)))){
    message(paste0("No trained model for site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    
    mod_fit <- readRDS(here(paste0("Forecast_submissions/Generate_forecasts/tg_lasso/trained_models/",mod_file)))
    
    # forecast step
    predictions <- predict(unbundle(mod_fit),
                           new_data = noaa_future)|>
      rename(prediction = ".pred")
    
    forecast <- noaa_future %>% 
      #select(datetime, parameter, all_of(variables)) %>% 
      bind_cols(predictions) |> 
      mutate(site_id = site,
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




