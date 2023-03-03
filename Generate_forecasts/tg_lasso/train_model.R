#Script creates trained model for each site and target variable using Lasso regression


#### Step 1: Load libraries
library(here)
library(tidyverse)
library(tidymodels)
library(butcher)
library(bundle)
library(butcher)
library(neon4cast)
library(lubridate)
library(rMR)
library(glue)
library(decor)
#source("ignore_sigpipe.R")
library(tsibble)
library(fable)
library(arrow)

here::i_am("EFI_Theory/Generate_forecasts/tg_lasso/train_model.R")
source(here("EFI_Theory/download_target.R"))

# Set model types
model_themes = c("terrestrial_daily","aquatics","phenology") #This model is only relevant for three themes
model_types = c("terrestrial","aquatics","phenology")


#### Step 2: Get NOAA driver data

forecast_date <- Sys.Date()
noaa_date <- Sys.Date() - lubridate::days(1)  #Need to use yesterday's NOAA forecast because today's is not available yet
site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") %>%
  filter(if_any(matches(model_types),~.==1))
all_sites = site_data$field_site_id


# Specify desired met variables - all meteo
variables <- c('air_temperature',
               "surface_downwelling_longwave_flux_in_air",
               "surface_downwelling_shortwave_flux_in_air",
               "precipitation_flux",
               "air_pressure",
               "relative_humidity",
               "air_temperature",
               "northward_wind",
               "eastward_wind")

#Code from Freya Olsson to download and format meteorological data (had to be modified to deal with arrow issue on M1 mac). Major thanks to Freya here!!

# Load stage 3 data
noaa_date <- Sys.Date() - lubridate::days(1)
endpoint = "data.ecoforecast.org"
use_bucket <- paste0("neon4cast-drivers/noaa/gefs-v12/stage2/parquet/0/", noaa_date)



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


if(file.exists(here("EFI_Theory/Generate_forecasts/noaa_downloads/past_allmeteo.csv"))) {
  noaa_past_mean <- read_csv(here("EFI_Theory/Generate_forecasts/noaa_downloads/past_allmeteo.csv"))
} else {
  noaa_past_mean <- map_dfr(all_sites, load_stage3,endpoint,variables)
}



############################################ SET UP TRAINING LOOPS ###################################

##### Training function ##########

train_site <- function(site, noaa_past_mean, target_variable) {
  message(paste0("Running site: ", site))
  
  # Get site information for elevation
  site_info <- site_data |> dplyr::filter(field_site_id == site)
  
  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable %in% c(target_variable), 
                  site_id == site) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean%>%
                       filter(site_id == site), by = c("datetime", "site_id"))
  
  if(!target_variable%in%names(site_target)){
    message(paste0("No target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else if(sum(!is.na(site_target$air_temperature)&!is.na(site_target[target_variable]))==0){
    message(paste0("No historical air temp data that corresponds with target observations at site ",site,". Skipping forecasts at this site."))
    return()
    
  } else {
    # Tune and fit lasso model - making use of tidymodels
    
    #Recipe for training models
    rec_base <- recipe(site_target)|>
      step_rm(c("datetime", "site_id"))|>
      update_role(everything(), new_role = "predictor")|>
      update_role({{target_variable}}, new_role = "outcome")|>
      step_normalize(all_numeric(), -all_outcomes())|>
      step_naomit(all_outcomes(),all_numeric_predictors(), skip = TRUE)
    
    ## Set up tuning and fitting engine
    lambda_grid <- grid_regular(penalty(), levels = 100) #column of penalties evenly spread from 1e-10 to 1
    tune_spec <- linear_reg(penalty = tune(), mixture = 1)|>
      set_engine("glmnet") #lasso (mixture = 1) fit with glmnet package
    
    #k-fold cross-validation
    lasso_resamp <- vfold_cv(site_target, v = 10, repeats = 5) #define k-fold cross validation procedure 
    ## Assemble workflow and tune
    wf <- workflow() %>%
      add_recipe(rec_base)
    
    #Tune models
  #If running in parallel  
    library(doParallel)
    cl <- makePSOCKcluster(16)
    registerDoParallel(cl) 
    lasso_grid <- tune_grid(
      wf %>% add_model(tune_spec),
      resamples = lasso_resamp,
      grid = lambda_grid
    )
    
    ## Select best model via RMSE
    best_lasso<-lasso_grid|>
      select_best("rmse")
    
    #select model with best tuning parameter by RMSE, cross-validation approach
    final_lasso <- finalize_workflow(
      wf %>% add_model(tune_spec),
      best_lasso
    )
    
    final_fit <- fit(final_lasso, site_target)
    
    final_preds <- predict(final_fit, site_target)|>
      bind_cols(site_target)
      
    final_rmse<-rmse(final_preds, estimate = .pred, truth = {{target_variable}})
    
    #save model fit in minimal form
    res_bundle <-
      final_fit %>%            
      butcher() %>% 
      bundle()
    
    saveRDS(res_bundle, here(paste0("EFI_Theory/Generate_forecasts/tg_lasso/trained_models/", paste(theme, site, target_variable,"trained",Sys.Date(), sep = "-"), ".Rds")))
    tibble(theme = theme, site = site, target_variable = target_variable, rmse = final_rmse$.estimate, lambda = best_lasso$penalty)
    
  }
}


######### Loop to train all sites ########

for (theme in model_themes) {
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
    filter(get(type)==1) #filters the temperature
  
  sites = site_data$field_site_id
  
  #Set target variables for each theme
  if(theme == "aquatics")           {vars = c("temperature","oxygen","chla")}
  if(theme == "phenology")          {vars = c("gcc_90","rcc_90")}
  if(theme == "terrestrial_daily")  {vars = c("nee","le")}
  if(theme == "beetles")            {vars = c("abundance","richness")}
  if(theme == "ticks")              {vars = c("amblyomma_americanum")}
  
  site_var_combos <- expand_grid(vars, sites)|>
    rename(site = "sites", target_variable = "vars")
    #filter(site == "KING"|site == "ABBY")
  
  mod_summaries <- map2(site_var_combos$site, site_var_combos$target_variable, ~train_site(site = .x, target_variable = .y, noaa_past_mean = noaa_past_mean))|>
    compact()|>
    list_rbind()
  
  assign(x = paste0(theme, "_mod_summaries"), value = mod_summaries)
  
}

mod_sums_all <- syms(apropos("_mod_summaries"))|>
  map_dfr(~eval(.)|>bind_rows())|>
  write_csv(here("EFI_Theory/Generate_forecasts/tg_lasso/model_training_summaries.csv"))
