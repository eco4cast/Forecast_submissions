# tg_auto_adam model
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
library(forecast)
library(smooth)
source("./Generate_forecasts/R/load_met.R")
source("./Generate_forecasts/R/generate_tg_forecast.R")
source("./Generate_forecasts/R/run_all_vars.R")

model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks") #By default, run model across all themes, except terrestrial 30min (not currently configured)
model_id = "tg_auto_adam"

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
  
  # Get site information for elevation
  #site_info <- site_data |> dplyr::filter(field_site_id == site)
  
  # Format site data for adam model
  site_target_raw <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable == target_variable, 
                  site_id == site,
                  datetime < forecast_date) |> 
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")
  
  if(!target_variable%in%names(site_target_raw)||sum(!is.na(site_target_raw[target_variable]))==0){
    message(paste0("No target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    
    if(theme %in% c("ticks","beetles")){
      site_target = site_target_raw %>%
        filter(wday(datetime,label = T)=="Mon")|>
        complete(datetime = full_seq(datetime,step),site_id)
      #Find the most recent Monday
      mon = Sys.Date()-abs(1-as.numeric(strftime(Sys.Date(), "%u")))
      h = as.numeric(floor((mon-max(site_target$datetime))/step)+horiz)
    } else {
      site_target = site_target_raw |>
        complete(datetime = full_seq(datetime,1),site_id)
      h = as.numeric(Sys.Date()-max(site_target$datetime)+horiz)
    }
    
    ts_data = as.ts(site_target[target_variable])
    #If all data are positive, apply the correct transformation
    if(sum(ts_data<0,na.rm=T)==0){#if there are no negative values, consider transformation
      ts_data[ts_data==0&!is.na(ts_data)]=0.0001
      ts_data_interp = na.interp(ts_data, lambda = "auto")
    } else {
      ts_data_interp = na.interp(ts_data)
    }
    
    # Fit adam model
    fit = auto.adam(ts_data_interp)
    
    # use the model to forecast target variable
    forecast_raw <- as.data.frame(forecast(fit,h=h,interval="prediction",level = .68))%>% #One standard deviation
      mutate(sigma = `Hi 0.68`-`Point Forecast`)
    
    forecast = data.frame(datetime = (1:h)*step+max(site_target$datetime),
                          reference_datetime = forecast_date,
                          site_id = site,
                          family = "normal",
                          variable = target_variable,
                          mu = as.numeric(forecast_raw$`Point Forecast`),
                          sigma = as.numeric(forecast_raw$sigma),
                          model_id = model_id)%>%
      pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
      select(model_id, datetime, reference_datetime,
             site_id, family, parameter, variable, prediction)
    return(forecast)
  }
}
