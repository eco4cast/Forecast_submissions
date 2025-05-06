# tg_XGBoost model
# written by ASL, 21 Jan 2023
# edited 2023-09-08 to consolidate and set up framework for filling in missed dates


#### Step 0: load packages

library(tidyverse)
library(neon4cast)
library(lubridate)
library(glue)
source("ignore_sigpipe.R")
library(tsibble)
library(fable)
library(arrow)
library(caret)
library(xgboost)
source("download_target.R")
source("./Generate_forecasts/R/load_met.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks") #By default, run model across all themes, except terrestrial 30min (not currently configured)
model_id = "tg_XGBoost"

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
  
  if(!target_variable%in%names(site_target)){
    message(paste0("No target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else if(sum(!is.na(site_target$air_temperature)&!is.na(site_target[target_variable]))==0){
    message(paste0("No historical air temp data that corresponds with target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    data <- site_target %>% 
      select(all_of(c(target_variable, "air_temperature", "air_pressure", 
                      "precipitation_flux", "relative_humidity", 
                      "surface_downwelling_shortwave_flux_in_air", "eastward_wind", 
                      "northward_wind", "surface_downwelling_longwave_flux_in_air"))) %>%
      na.omit()
    matrix <- as.matrix(data %>% select(-all_of(target_variable)))
    cv <- xgboost::xgb.cv(label = data[[target_variable]],
                          data = matrix,
                          nfold = 10,
                          nrounds = 100,
                          early_stopping_rounds = 2,
                          eval_metric = "rmse")
    nrounds <- cv$best_iteration
    fit <- xgboost(data = matrix,
                   label = data[[target_variable]],
                   nrounds = nrounds)
    pred <- predict(fit, matrix)
    #plot(site_target[[target_variable]])
    #lines(pred)
    
    # Get 30-day predicted temp ensemble at the site
    noaa_future <- noaa_future_daily%>%
      filter(site_id==site) |>
      select(all_of(c("datetime","parameter","air_temperature", "air_pressure", 
                      "precipitation_flux", "relative_humidity", 
                      "surface_downwelling_shortwave_flux_in_air", "eastward_wind", 
                      "northward_wind", "surface_downwelling_longwave_flux_in_air"))) %>%
      na.omit()
    
    # use the model to forecast target variable for each ensemble member
    forecast <- 
      noaa_future |> 
      mutate(site_id = site,
             prediction = predict(fit, 
                                  as.matrix(noaa_future %>% 
                                              select(-all_of(c("datetime","parameter"))))), #THIS IS THE FORECAST STEP
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
