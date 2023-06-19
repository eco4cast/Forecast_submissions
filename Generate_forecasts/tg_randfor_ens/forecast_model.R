# Random forest model - mtry and min_n parameters tuned on all target historical data (date of tuning on file name in tg_randfor/trained_models/) and final fit to forecast 
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
library(ranger)
here::i_am("Forecast_submissions/Generate_forecasts/tg_randfor_ens_parm_test/forecast_model.R")
source(here("Forecast_submissions/download_target.R"))
#source(here("Forecast_submissions/ignore_sigpipe.R"))  #might fail locally, but necessary for git actions to exit properly or something




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

model_id = "tg_randfor_ens"
model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks")
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
      complexity = 9, # CHANGE THIS BASED ON NUMBER OF VARIABLES
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

####### Download Forecasted driver data - Load stage 2 data
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


######## Download noaa_past_mean - historical driver data - Load stage 3
load_stage3 <- function(site,endpoint,variables){
  message('run ', site)
  use_bucket <- paste0("neon4cast-drivers/noaa/gefs-v12/stage3/parquet/", site)
  use_s3 <- arrow::s3_bucket(use_bucket, endpoint_override = endpoint, anonymous = TRUE)
  parquet_file <- arrow::open_dataset(use_s3) |>
    dplyr::collect() |>
    dplyr::filter(datetime >= lubridate::ymd('2017-01-01'),
                  variable %in% variables)|> #It would be more efficient to filter before collecting, but this is not running on my M1 mac
    na.omit() |> 
    mutate(datetime = lubridate::as_date(datetime)) |> 
    group_by(datetime, site_id, variable) |> 
    summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
    pivot_wider(names_from = variable, values_from = prediction) |> 
    # convert air temp to C
    mutate(air_temperature = air_temperature - 273.15)
}


if(file.exists(here("Forecast_submissions/Generate_forecasts/noaa_downloads/past_allmeteo.csv"))) {
  noaa_past_mean <- read_csv(here("Forecast_submissions/Generate_forecasts/noaa_downloads/past_allmeteo.csv"))
} else {
  noaa_past_mean <- map_dfr(all_sites, load_stage3,endpoint,variables)
}


#### Step 3.0: Define the forecasts model for a site


forecast_site <- function(site,noaa_past_mean, noaa_future_daily,target_variable) {
  message(paste0("Running site: ", site))
  
  
  # Get site information
  site_info <- site_data |> dplyr::filter(field_site_id == site)
  
  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable %in% c(target_variable), 
                  site_id == site) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean%>%
                       filter(site_id == site), by = c("datetime", "site_id"))|>
    drop_na()
  
  # Grab full workflow created in file train_model.R
  mod_file <- list.files(here("Forecast_submissions/Generate_forecasts/tg_randfor_ens_parm_test/trained_models/"), 
                         pattern = paste(theme, site, target_variable, sep = "-"))
  
  if(!file.exists(here(paste0("Forecast_submissions/Generate_forecasts/tg_randfor_ens_parm_test/trained_models/",mod_file)))){
    message(paste0("No trained model for site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    
    ### Generate ensemble fits
    final_mod <- readRDS(here(paste0("Forecast_submissions/Generate_forecasts/tg_randfor_ens_parm_test/trained_models/",mod_file)))
    
    n_splits <- 10
    split_fits <- site_target|>
      vfold_cv(v = n_splits)|> #split data into n random ~equal-sized partitions
      pull(splits)|> 
      map(~analysis(.))|> # take 'analysis' set of each split - so if n_splits = 10, each model fits on 90% of the data
      map(~fit(final_mod,.)) # adds about 5 seconds to refit 10 folds
    
    
    # Predictions to historical data from each ensemble member, average predictions, calculate variance of residuals
    mean_preds <- split_fits|>
      map(~predict(., site_target)|>bind_cols(site_target|>select(datetime,{{target_variable}})))|>
      list_rbind()|>
      group_by(datetime)|>
      summarize(across(c(.pred,{{target_variable}}), ~mean(.)))
    
    err_var <- mean_preds|>
      mutate(resid = .pred - get(target_variable))|>
      summarize(var(resid))|>pull()
    
    
    ### Predict forecasts
    #  Get 30-day predicted temperature ensemble at the site
    noaa_future <- noaa_future_daily%>%
      filter(site_id == site)|>
      drop_na() #dropping NAs necessary for ranger package random forest models to run
    
    #generate predictions with model ensemble
    
    #need to be able to identify the unique ensemble members
    predictions <- map(split_fits, ~predict(., noaa_future)|>bind_cols(noaa_future)|>rename(prediction = ".pred"))|>
      list_rbind(names_to = "param_set")|>
      mutate(parameter = str_c(parameter, "_",param_set)|>as_factor()|>as.numeric())|>
      #create a new value for unique driverXparameter set combination (ensemble member set)
      mutate(prediction = prediction + rnorm(n(),mean = 0, sd = sqrt(err_var))) 
    # if aquatics - norm fine, if beetles/ticks, likely need poisson or something
    #add estimate of process error noise - likely need to vary this for certain variables - abundances, for example
    forecast <- predictions|> 
      mutate(site_id = site,
             variable = target_variable)|>
      mutate(reference_datetime = forecast_date,
             family = "ensemble",
             model_id = model_id) |>
      select(model_id, datetime, reference_datetime,
             site_id, family, parameter, variable, prediction)
  }
}



#Quick function to repeat for all variables
run_all_vars = function(var,sites,forecast_site,noaa_past_mean,noaa_future_daily){
  
  message(paste0("Running variable: ", var))
  forecast <- map_dfr(sites,possibly(forecast_site, otherwise = data.frame(prediction = NA_real_)),noaa_past_mean,noaa_future_daily,var)
  
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
    
    site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv")|>
      filter(get(type)==1)
    sites = site_data$field_site_id
    
    #Set target variables
    if(theme == "aquatics")           {vars = c("temperature","oxygen","chla")}
    if(theme == "phenology")          {vars = c("gcc_90","rcc_90")}
    if(theme == "terrestrial_daily")  {vars = c("nee","le")}
    if(theme == "beetles")            {vars = c("abundance","richness")}
    if(theme == "ticks")              {vars = c("amblyomma_americanum")}
    
    ## Generate forecast
    forecast <- map_dfr(vars,run_all_vars,sites,
                        possibly(forecast_site, otherwise = data.frame(prediction = NA_real_)),noaa_past_mean,noaa_future_daily)|>
      filter(!is.na(prediction))
    
    
    #Forecast output file name in standards requires for Challenge.
    # csv.gz means that it will be compressed
    file_date <- Sys.Date() #forecast$reference_datetime[1]
    model_id = "tg_randfor_ens"
    forecast_file <- paste0(theme,"-",file_date,"-",model_id,".csv.gz")
    
    
    #Write csv to disk
    write_csv(forecast, forecast_file)
    
    
    # Step 5: Submit forecast!
    neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = TRUE)
  }
}

