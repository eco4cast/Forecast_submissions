# tg_ets model
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
library(forecast)

source("generate_forecasts/R/load_met.R")
source("generate_forecasts/R/generate_tg_forecast.R")
source("generate_forecasts/R/run_all_vars.R")

model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks") #By default, run model across all themes, except terrestrial 30min (not currently configured)
model_id = "tg_ets"

#### Define the forecast model for a site
forecast_model <- function(site,
                           noaa_past_mean,
                           noaa_future_daily,
                           target_variable,
                           target,
                           horiz,
                           step) {
  
  message(paste0("Running site: ", site))
  
  # Get site information for elevation
  #site_info <- site_data |> dplyr::filter(field_site_id == site)
  
  # Format site data for arima model
  site_target_raw <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable == target_variable, 
                  site_id == site) |> 
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
    # Fit tbats with interpolated data
    fit = ets(ts_data_interp)
    
    # use the model to forecast target variable
    forecast_raw <- as.data.frame(forecast(fit,h=h,level=0.68))%>% #One SD
      mutate(sigma = `Hi 68`-`Point Forecast`)
    
    forecast = data.frame(datetime = (1:h)*step+max(site_target$datetime),
                          reference_datetime = Sys.Date(),
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
    forecast_file <- paste0(theme,"-", as_date(this_year$date[i]), '-tg_ets.csv.gz')
    
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
