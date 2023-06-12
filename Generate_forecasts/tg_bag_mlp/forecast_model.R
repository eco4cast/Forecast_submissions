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
#library(ranger)
here::i_am("Forecast_submissions/Generate_forecasts/tg_bag_mlp/forecast_model.R")
source(here("Forecast_submissions/download_target.R"))
source(here("Forecast_submissions/ignore_sigpipe.R"))  #might fail locally, but necessary for git actions to exit properly or something


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

model_id = "tg_bag_mlp"
model_themes = c("terrestrial_daily","aquatics","phenology") 
model_types = c("terrestrial","aquatics","phenology") 
#Options: aquatics, beetles, phenology, terrestrial_30min, terrestrial_daily, ticks

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
  dplyr::select(datetime, site_id, all_of(variables), parameter)
message(nrow(noaa_future_daily))


#### Step 3.0: Define the forecasts model for a site
forecast_site <- function(site,noaa_future_daily,target_variable) {
  message(paste0("Running site: ", site))
  
  
  # Get site information for elevation
  site_info <- site_data |> dplyr::filter(field_site_id == site)
  
  mod_file <- list.files(here("Forecast_submissions/Generate_forecasts/tg_bag_mlp/trained_models/"), pattern = paste(theme, site, target_variable, sep = "-"))
  message(paste0("mod_file: ",mod_file))
  
  if(length(mod_file)==0){
    message(paste0("No trained model for site ",site,". Skipping forecasts at this site."))
    return()
    
    } else {

    #  Get 30-day predicted temperature ensemble at the site
    noaa_future <- noaa_future_daily%>%
      filter(site_id == site)|>
      drop_na() #dropping NAs necessary for some models to run
    message(paste0("NOAA future dimensions: ",paste(dim(noaa_future), collapse = " ")))
    
  #generate predictions with trained model

    
    mod_fit <- readRDS(here(paste0("Forecast_submissions/Generate_forecasts/tg_bag_mlp/trained_models/",mod_file)))
    message(paste0("Length of workflow: ",length(mod_fit)))
    message(paste0("Workflow: \n",paste(unbundle(mod_fit)$fit$fit$fit$imp, collapse = "\n")))
 
    predictions <- predict(unbundle(mod_fit),new_data = noaa_future)%>%
      rename(prediction = ".pred")
    message(paste0("colnames(predictions): ",colnames(predictions)))
    
    forecast <- noaa_future %>% 
      dplyr::select(all_of(variables)) %>% 
      bind_cols(predictions) |> 
      mutate(site_id = site,
             variable = target_variable)
    message(paste0("colnames(forecast): ",paste(colnames(forecast), collapse = ", ")))
    
  
    # Format results to EFI standard
    forecast <- forecast |>
      mutate(reference_datetime = forecast_date,
             family = "ensemble",
             model_id = model_id) |>
      dplyr::select(model_id, datetime, reference_datetime,
             site_id, family, parameter, variable, prediction)
    message(paste0("colnames(forecast): ",paste(colnames(forecast), collapse = ", ")))
    return(forecast)
  }
}

#Quick function to repeat for all variables
run_all_vars = function(var,sites,forecast_site,noaa_future_daily){
  
  message(paste0("Running variable: ", var))
  forecast <- map_dfr(sites,possibly(forecast_site, otherwise = data.frame(prediction = NA_real_)),noaa_future_daily,var)
  
}


for (theme in model_themes) {
  #Step 1: Download latest target data and site description data
#  target = download_target(theme)
  type = ifelse(theme%in% c("terrestrial_30min", "terrestrial_daily"),"terrestrial",theme)
  
  site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv")|>
    filter(get(type)==1)
  sites = site_data$field_site_id

  #Set target variables
  if(theme == "aquatics")           {vars = c("temperature","oxygen","chla")}
  if(theme == "phenology")          {vars = c("gcc_90","rcc_90")}
  if(theme == "terrestrial_daily")  {vars = c("nee","le")}
  #if(theme == "beetles")            {vars = c("abundance","richness")}
  #if(theme == "ticks")              {vars = c("amblyomma_americanum")}

  ## Generate forecast
  forecast <- map_dfr(vars,run_all_vars,sites,possibly(forecast_site, otherwise = data.frame(prediction = NA_real_)),noaa_future_daily)|>
    filter(!is.na(prediction))
  message(colnames(forecast))
  message(nrow(forecast))
  

  #Forecast output file name in standards requires for Challenge.
  # csv.gz means that it will be compressed
  file_date <- Sys.Date() #forecast$reference_datetime[1]
  model_id = "tg_bag_mlp"
  forecast_file <- paste0(theme,"-",file_date,"-",model_id,".csv.gz")
  
  
  #Write csv to disk
  write_csv(forecast, forecast_file)
  
  
  # Step 5: Submit forecast!
  neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)
}
