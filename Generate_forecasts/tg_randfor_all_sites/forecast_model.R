# random forest model - hyperparameters mtry and min_n tuned on all target historical data (date of tuning on file name in tg_randfor_all_sites/trained_models/) and final fit to forecast 
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
here::i_am("Forecast_submissions/Generate_forecasts/tg_randfor_all_sites/forecast_model.R")
source(here("Forecast_submissions/download_target.R"))
source(here("Forecast_submissions/ignore_sigpipe.R")) #might fail locally


#### Step 1: Define team name, team members, and theme

team_name <- "EFI Theory"

team_list <- list(list(individualName = list(givenName = "Abby", 
                                             surName = "Lewis"),
                       organizationName = "Virginia Tech",
                       electronicMailAddress = "aslewis@vt.edu"),
                  list(individualName = list(givenName = "Caleb", 
                                             surName = "Robbins"),
                       organizationName = "Baylor University",
                       electronicMailAddress = "Caleb_Robbins@baylor.edu")
)

model_id = "randfor"
model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks") #This model is only relevant for three themes
model_types = c("terrestrial","aquatics","phenology","beetles","ticks")
#Options: aquatics, beetles, phenology, terrestrial_30min, terrestrial_daily, ticks

#Create model metadata
model_metadata = list(
  forecast = list(
    model_description = list(
      forecast_model_id =  model_id, 
      type = "empirical",  
      repository = "https://github.com/EFI-Theory/Forecast_submissions" 
    ),
    initial_conditions = list(
      status = "absent"
    ),
    drivers = list(
      status = "propagates",
      complexity = 10, # CHANGE THIS BASED ON NUMBER OF VARIABLES
      propagation = list( 
        type = "ensemble", 
        size = 31) 
    ),
    parameters = list(
      status = "absent"
    ),
    random_effects = list(
      status = "absent"
    ),
    process_error = list(
      status = "absent"
    ),
    obs_error = list(
      status = "absent"
    )
  )
)
#metadata_file <- neon4cast::generate_metadata(forecast_file, team_list, model_metadata) #Function is not currently available


#### Step 2: Get NOAA driver data

forecast_date <- Sys.Date()
noaa_date <- Sys.Date() - lubridate::days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet

#We're going to get data for all sites relevant to this model, so as to not have to re-load data for the same sites
site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") %>%
  filter(if_any(matches(model_types),~.==1))
all_sites = site_data$field_site_id

# specify meteorological variables needed to make predictions
variables <- c('air_temperature',
"surface_downwelling_longwave_flux_in_air",
"surface_downwelling_shortwave_flux_in_air",
"precipitation_flux",
"air_pressure",
"relative_humidity",
"air_temperature",
"northward_wind",
"eastward_wind")

# Load stage 2 data
endpoint = "data.ecoforecast.org"
use_bucket <- paste0("neon4cast-drivers/noaa/gefs-v12/stage2/parquet/0/", noaa_date)
use_s3 <- arrow::s3_bucket(use_bucket, endpoint_override = endpoint, anonymous = TRUE)
noaa_future <- arrow::open_dataset(use_s3) |>
  dplyr::collect() |>
  dplyr::filter(site_id %in% all_sites,
                datetime >= forecast_date,
                variable == variables) 

# Format met forecasts
noaa_future_daily <- noaa_future |> 
  mutate(datetime = lubridate::as_date(datetime)) |> 
  # mean daily forecasts at each site per ensemble
  group_by(datetime, site_id, parameter, variable) |> 
  summarize(prediction = mean(prediction)) |>
  pivot_wider(names_from = variable, values_from = prediction) |>
  # convert to Celsius
  mutate(air_temperature = air_temperature - 273.15) |> 
  select(datetime, site_id, all_of(variables), parameter)


#### Step 3.0: Define the forecasts model for a site
forecast_all_sites <- function(sites,noaa_future_daily,target_variable) {
  message(paste0("Running ",target_variable," at all sites"))
  
  
  
  mod_file <- list.files(here("Forecast_submissions/Generate_forecasts/tg_randfor_all_sites/trained_models/"), pattern = paste(theme, target_variable, sep = "-"))
  
  if(!file.exists(here(paste0("Forecast_submissions/Generate_forecasts/tg_randfor_all_sites/trained_models/",mod_file)))){
    message(paste0("No trained model for site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    
    #  Get 30-day predicted temperature ensemble at the site
    noaa_future <- noaa_future_daily%>%
      filter(site_id %in% sites)|>
      drop_na() #necessary for ranger engine to fit random forest - won't accept NA
    
    #generate predictions with trained model
    mod_fit <- readRDS(here(paste0("Forecast_submissions/Generate_forecasts/tg_randfor_all_sites/trained_models/",mod_file)))
    
    predictions <- predict(unbundle(mod_fit),
                           new_data = noaa_future)|>
      rename(prediction = ".pred")
    
    
    forecast <- noaa_future %>% 
      select(all_of(variables), site_id) %>% 
      bind_cols(predictions) |> 
      mutate(#site_id = sites,
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



for (theme in model_themes) {
  if(!theme%in%c("beetles","ticks") | wday(Sys.Date(), label=TRUE)=="Sun"){ #beetles and ticks only want forecasts every Sunday
    #Step 1: Download latest target data and site description data
    target = download_target(theme)
    type = ifelse(theme%in% c("terrestrial_30min", "terrestrial_daily"),"terrestrial",theme)
    
    if("siteID" %in% colnames(target)){ #Sometimes the site is called siteID instead of site_id. Fixing here
      target = target%>%
        rename(site_id = siteID)
    }
    if("time" %in% colnames(target)){ #Sometimes the time column is instead labeled "datetime"
      target = target%>%
        rename(datetime = time)
    }
    
    site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") %>%
      filter(get(type)==1)
    sites = site_data$field_site_id
    #sites <- c("ARIK", "PRLA", "KING", "PRPO") #can be used for testing - 2 sites have chla
    
    #Set target variables
    if(theme == "aquatics")           {vars = c("temperature","oxygen","chla")}
    if(theme == "phenology")          {vars = c("gcc_90","rcc_90")}
    if(theme == "terrestrial_daily")  {vars = c("nee","le")}
    if(theme == "beetles")            {vars = c("abundance","richness")}
    if(theme == "ticks")              {vars = c("amblyomma_americanum")}
    
    ## Generate forecast
    forecast <- map_dfr(vars,possibly(~forecast_all_sites(noaa_future_daily = noaa_future_daily, sites = sites, target_variable = .), 
                                      otherwise = data.frame(prediction = NA_real_)))|>
      filter(!is.na(prediction))
    
    
    if(theme %in% c("beetles","ticks")){
      forecast = forecast%>% filter(wday(datetime, label=TRUE)=="Mon") #The beetles and ticks challenges only want weekly forecasts
    }
    #Forecast output file name in standards requires for Challenge.
    # csv.gz means that it will be compressed
    file_date <- Sys.Date() #forecast$reference_datetime[1]
    model_id = "tg_randfor_all_sites"
    forecast_file <- paste0(theme,"-",file_date,"-",model_id,".csv.gz")
    
    #Write csv to disk
    write_csv(forecast, forecast_file)
    
    # Step 5: Submit forecast!
    neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)
  }
}






