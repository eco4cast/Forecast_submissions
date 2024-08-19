# lasso model - regularization parameter tuned on all target historical data (date of tuning on file name in tg_lasso_all_sites/trained_models/) and final fit to forecast 
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
library(glmnet)
here::i_am("Forecast_submissions/Generate_forecasts/tg_lasso_all_sites/forecast_model.R")
source(here("Forecast_submissions/download_target.R"))
source(here("Forecast_submissions/ignore_sigpipe.R")) #might fail locally 
source("./Generate_forecasts/R/load_met.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks") #By default, run model across all themes, except terrestrial 30min (not currently configured)
model_id = "tg_lasso_all_sites"




forecast_model <- function(target_variable,
                           noaa_past_mean,
                           noaa_future_daily,
                           sites,
                           target,
                           horiz,
                           step,
                           theme,
                           forecast_date) {
  
  message(paste0("Running ",target_variable," at all sites"))
  
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
                  site_id %in% sites,
                  datetime < forecast_date) |> 
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean%>%
                       filter(site_id %in% sites), 
                     by = c("datetime", "site_id"))
  
  chla_sites <- target|>filter(variable == "chla")|>filter(!is.na(observation))|>pull(site_id)
  
  
  #  Get 30-day predicted NOAA ensemble at the site
  
  if(target_variable == "chla"){
    noaa_future <- noaa_future_daily%>%
      filter(site_id %in% chla_sites)
    
  } else {
    
    noaa_future <- noaa_future_daily%>%
      filter(site_id %in% sites)
  }
  
  # Call saved model fits
  mod_file <- list.files(here("Forecast_submissions/Generate_forecasts/tg_lasso_all_sites/trained_models/"), pattern = paste(theme, target_variable, sep = "-"))
  mod_fit <- readRDS(here(paste0("Forecast_submissions/Generate_forecasts/tg_lasso_all_sites/trained_models/",mod_file)))
  
  # forecast step
  predictions <- predict(unbundle(mod_fit),
                         new_data = noaa_future)|>
    rename(prediction = ".pred")
  
  forecast <- noaa_future %>% 
    bind_cols(predictions) |> 
    mutate(variable = target_variable)
  
  
  # Format results to EFI standard
  forecast <- forecast |>
    mutate(reference_datetime = forecast_date,
           family = "ensemble",
           model_id = model_id) |> 
    select(model_id, datetime, reference_datetime,
           site_id, family, parameter, variable, prediction)
  
}


