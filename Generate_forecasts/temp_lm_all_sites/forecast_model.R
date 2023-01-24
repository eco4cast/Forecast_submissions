# temp_lm_all_sites model
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



#### Step 1: Define team name, team members, and theme

team_name <- "EFI Theory"

team_list <- list(list(individualName = list(givenName = "Abby", 
                                             surName = "Lewis"),
                       organizationName = "Virginia Tech",
                       electronicMailAddress = "aslewis@vt.edu")
)

model_id = "temp_lm_all_sites"
model_themes = c("terrestrial_daily","aquatics","phenology") #This model is only relevant for three themes. I am registered for all three
model_types = c("terrestrial","aquatics","phenology") #Replace terrestrial daily and 30min with terrestrial
#Options: aquatics, beetles, phenology, terrestrial_30min, terrestrial_daily, ticks

#Create model metadata
model_metadata = list(
  forecast = list(
    model_description = list(
      forecast_model_id =  model_id, 
      type = "empirical",  
      repository = "https://github.com/abbylewis/EFI_Theory" 
    ),
    initial_conditions = list(
      status = "absent"
    ),
    drivers = list(
      status = "propagates",
      complexity = 1, #Just temperature
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

#Specify desired met variables
variables <- c('air_temperature')

#Code from Freya Olsson to download and format meteorological data (had to be modified to deal with arrow issue on M1 mac). Major thanks to Freya here!!

# Load stage 2 data
endpoint = "data.ecoforecast.org"
use_bucket <- paste0("neon4cast-drivers/noaa/gefs-v12/stage2/parquet/0/", noaa_date)
use_s3 <- arrow::s3_bucket(use_bucket, endpoint_override = endpoint, anonymous = TRUE)
noaa_future <- arrow::open_dataset(use_s3) |>
  dplyr::collect() |>
  dplyr::filter(site_id %in% all_sites,
                datetime >= forecast_date,
                variable == variables) #It would be more efficient to filter before collecting, but this is not running on my M1 mac

# Format met forecasts
noaa_future_daily <- noaa_future |> 
  mutate(datetime = lubridate::as_date(datetime)) |> 
  # mean daily forecasts at each site per ensemble
  group_by(datetime, site_id, parameter, variable) |> 
  summarize(prediction = mean(prediction)) |>
  pivot_wider(names_from = variable, values_from = prediction) |>
  # convert to Celsius
  mutate(air_temperature = air_temperature - 273.15) |> 
  select(datetime, site_id, air_temperature, parameter)

# Load stage3 data. 
noaa_past_mean <- read.csv("./Generate_forecasts/noaa_downloads/past_temp.csv")|> 
  mutate(datetime = lubridate::as_date(datetime))

# Plot met
jpeg("met_forecasts.jpg",width = 10, height = 10, units = "in", res = 300)
ggplot(noaa_future_daily, aes(x=datetime, y=air_temperature)) +
  geom_line(aes(group = parameter), alpha = 0.4)+
  geom_line(data = noaa_past_mean, colour = 'darkblue') +
  coord_cartesian(xlim = c(noaa_date - lubridate::days(60),
                           noaa_date + lubridate::days(35)))+
  facet_wrap(~site_id, scales = 'free')
dev.off()



#### Step 3.0: Define the forecasts model for a site
forecast_all_sites <- function(target_variable, sites,noaa_past_mean,noaa_future_daily) {
  
  message(paste0("Running ",target_variable," at all sites"))
  
  # Get site information for elevation
  #site_info <- site_data |> dplyr::filter(field_site_id == site)
  
  # Merge in past NOAA data into the targets file, matching by date and site.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable %in% c(target_variable), 
                  site_id %in% sites) |> 
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean%>%
                       filter(site_id %in% sites), by = c("datetime", "site_id"))
  
  if(sum(!is.na(site_target$air_temperature)&!is.na(site_target[target_variable]))==0){
    message(paste0("No historical air temp data that corresponds with target observations. Skipping forecasts for this variable."))
    return()
    
  } else {
    # Fit linear model based on past data: water temperature = m * air temperature + b
    fit <- lm(get(target_variable) ~ air_temperature+site_id, data = site_target)
    good_sites = unique(site_target$site_id[!is.na(site_target$air_temperature)&!is.na(site_target[target_variable])])
    
    #  Get 30-day predicted temperature ensemble at the site
    noaa_future <- noaa_future_daily%>%
      filter(site_id%in%sites)
    
    # use the linear model (predict.lm) to forecast water temperature for each ensemble member
    forecast <- 
      noaa_future |> 
      filter(site_id %in% good_sites) |>
      mutate(prediction = predict(fit, tibble(air_temperature, site_id)),
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

### AND HERE WE GO! We're ready to start forecasting ### 
for (theme in model_themes) {
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
  
  #Set target variables
  if(theme == "aquatics")           {vars = c("temperature","oxygen","chla")}
  if(theme == "phenology")          {vars = c("gcc_90","rcc_90")}
  if(theme == "terrestrial_daily")  {vars = c("nee","le")}

  #Test with a single variable first
  #forecast <- map_dfr(vars[1],forecast_all_sites,sites,noaa_past_mean,noaa_future_daily)
  #Visualize at one site
  #forecast_vis = forecast%>%
  #  filter(site_id == "WOOD")
  #forecast_vis |> 
  #  ggplot(aes(x = datetime, y = prediction, group = parameter)) +
  #  geom_line(alpha=0.3) +
  #  facet_wrap(~variable, scales = "free")
  
  # Run all variables -- may be slow!
  forecast <- map_dfr(vars,forecast_all_sites,sites,noaa_past_mean,noaa_future_daily)
  
  #Forecast output file name in standards requires for Challenge.
  # csv.gz means that it will be compressed
  file_date <- Sys.Date() #forecast$reference_datetime[1]
  model_id = "temp_lm_all_sites"
  forecast_file <- paste0(theme,"-",file_date,"-",model_id,".csv.gz")
  
  #Write csv to disk
  write_csv(forecast, forecast_file)
  
  #Generate metadata
  #metadata_file <- neon4cast::generate_metadata(forecast_file, team_list, model_metadata) #Function is not currently available
  
  # Step 5: Submit forecast!
  neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)
}

