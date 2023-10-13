# tg_humidity_lm model
# written by ASL, 21 Jan 2023
# edited 2023-09-08 to consolidate and set up framework for filling in missed dates


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
model_id = "tg_humidity_lm"

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
                  site_id == site) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean%>%
                       filter(site_id == site,
                              parameter <= 31), 
                     by = c("datetime", "site_id"))
  
  if(!target_variable%in%names(site_target)){
    message(paste0("No target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else if(sum(!is.na(site_target$relative_humidity)&!is.na(site_target[target_variable]))==0){
    message(paste0("No historical humidity data that corresponds with target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    # Fit linear model based on past data: target = m * relative_humidity + b
    fit <- lm(get(target_variable) ~ relative_humidity, data = site_target) #THIS IS THE MODEL
    
    #  Get 30-day predicted humidity ensemble at the site
    noaa_future <- noaa_future_daily%>%
      filter(site_id==site)
    
    # use the linear model (predict.lm) to forecast target variable for each ensemble member
    forecast <- 
      noaa_future |> 
      mutate(site_id = site,
             prediction = predict(fit, tibble(relative_humidity)), #THIS IS THE FORECAST STEP
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