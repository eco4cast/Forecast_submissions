# tg_arima model
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


#### Step 1: Define team name, team members, and theme

team_name <- "EFI Theory"

team_list <- list(list(individualName = list(givenName = "Abby", 
                                             surName = "Lewis"),
                       organizationName = "Virginia Tech",
                       electronicMailAddress = "aslewis@vt.edu")
)

model_id = "tg_arima"
model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks") #This model is only relevant for three themes. I am registered for all three
model_types = c("terrestrial","aquatics","phenology","beetles","ticks") #Replace terrestrial daily and 30min with terrestrial
#Options: aquatics, beetles, phenology, terrestrial_30min, terrestrial_daily, ticks


#### Step 2: Get NOAA driver data

# Skipping this for arima model

#### Step 3.0: Define the forecast model for a site
forecast_site <- function(site, target_variable, horiz,step) {
  
  message(paste0("Running site: ", site))
  
  # Get site information for elevation
  #site_info <- site_data |> dplyr::filter(field_site_id == site)
  
  # Format site data for arima model
  site_target_raw <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable == target_variable, 
                  site_id == site) |> 
    tidyr::pivot_wider(names_from = "variable", values_from = "observation")
  
  if(!target_variable%in%names(site_target_raw)){
    message(paste0("No target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    
    if(theme %in% c("ticks","beetles")){
      site_target = site_target_raw %>%
        mutate(observation=get(target_variable))%>%
        select(-{{target_variable}})%>%
        filter(wday(datetime,label = T)=="Mon")|>
        complete(datetime = full_seq(datetime,step),site_id)%>%
        tsibble::as_tsibble(key = c('site_id'), index = 'datetime')
      #Find the most recent Monday
      mon = Sys.Date()-abs(1-as.numeric(strftime(Sys.Date(), "%u")))
      h = as.numeric(floor((mon-max(site_target$datetime))/step)+horiz)
    } else {
      site_target = site_target_raw %>%
        mutate(observation=get(target_variable))%>%
        select(-{{target_variable}})|>
        complete(datetime = full_seq(datetime,1),site_id)%>%
        tsibble::as_tsibble(key = c('site_id'), index = 'datetime')
      h = as.numeric(Sys.Date()-max(site_target$datetime)+horiz)
    }
    
    # Fit arima model
    fit <- site_target %>%
      fabletools::model(model = fable::NNETAR(observation))
    
    # use the model to forecast target variable
    forecast_raw <- fit %>% fabletools::forecast(h = h)%>%
      mutate(do.call(rbind.data.frame, observation))
    
    forecast = forecast_raw%>%
      mutate(reference_datetime = Sys.Date(),
             family = "normal",
             variable = target_variable,
             model_id = model_id)%>%
      pivot_longer(cols = c(mu,sigma), names_to = "parameter",values_to = "prediction")%>%
      select(model_id, datetime, reference_datetime,
             site_id, family, parameter, variable, prediction)
    return(data.frame(forecast))
  }
}

#Quick function to repeat for all variables
run_all_vars = function(var,sites,forecast_site,horiz,step){
  
  message(paste0("Running variable: ", var))
  forecast <- map_dfr(sites,forecast_site,var,horiz,step)
  
}

### AND HERE WE GO! We're ready to start forecasting ### 
for (theme in model_themes) {
  if(!theme%in%c("beetles","ticks") | wday(Sys.Date(), label=TRUE)=="Sun"){ #beetles and ticks only want forecasts every Sunday
    #Step 1: Download latest target data and site description data
    target = download_target(theme)
    type = ifelse(theme%in% c("terrestrial_30min", "terrestrial_daily"),"terrestrial",theme)
    
    if("siteID" %in% colnames(target)){ #Sometimes the site is called siteID instead of site_id. Fixing here
      target = target%>%
        rename(site_id = siteID)
    }
    if("time" %in% colnames(target)){ #Sometimes the datetime column is instead labeled "time"
      target = target%>%
        rename(datetime = time)
    }
    
    site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") %>%
      filter(get(type)==1)
    sites = site_data$field_site_id
    
    #Set target variables
    if(type == "aquatics")           {vars = c("temperature","oxygen","chla")
                                                horiz = 30
                                                step = 1
                                                }
    if(type == "ticks")              {vars = c("amblyomma_americanum")
                                                horiz = 52 #52 weeks
                                                step = 7
                                                }
    if(type == "phenology")          {vars = c("gcc_90","rcc_90")
                                                horiz = 30 
                                                step = 1}
    if(type == "beetles")            {vars = c("abundance","richness")
                                                horiz = 52 #52 weeks
                                                step = 7}
    if(theme == "terrestrial_daily")  {vars = c("nee","le")
                                                horiz = 30
                                                step = 1}
    if(theme == "terrestrial_30min")  {vars = c("nee","le")
                                                horiz = 30
                                                step = 1/24/2}
  
    ## Test with a single site first!
    forecast <- map_dfr(vars,run_all_vars,sites[1],forecast_site,horiz,step)
    
    #Visualize the ensemble predictions -- what do you think?
    forecast %>%
      pivot_wider(names_from = parameter,values_from = prediction)%>%
      ggplot(aes(x = datetime)) +
      geom_ribbon(aes(ymax = mu + sigma, ymin = mu-sigma))+
      geom_line(aes(y = mu),alpha=0.3) +
      facet_wrap(~variable, scales = "free")
    
    # Run all sites -- may be slow!
    forecast <- map_dfr(vars,run_all_vars,sites,forecast_site,horiz,step)
    
    #Forecast output file name in standards requires for Challenge.
    # csv.gz means that it will be compressed
    file_date <- Sys.Date() #forecast$reference_datetime[1]
    model_id = "tg_temp_lm"
    forecast_file <- paste0(theme,"-",file_date,"-",model_id,".csv.gz")
    
    #Write csv to disk
    write_csv(forecast, forecast_file)
    
    #Generate metadata
    #metadata_file <- neon4cast::generate_metadata(forecast_file, team_list, model_metadata) #Function is not currently available
    
    # Step 5: Submit forecast!
    neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)
  }
}

