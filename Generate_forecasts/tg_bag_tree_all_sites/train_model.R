#Script creates trained model for each site and target variable using bag_tree

#### NOTE: Re-running this script will NOT overwrite previously saved models - if they are not deleted, the downstream forecasts
# using the saved model objects will not run correctly

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

here::i_am("Generate_forecasts/tg_bag_tree_all_sites/train_model.R")
source(here("download_target.R"))

# Set model types
model_themes = c("terrestrial_daily","aquatics","phenology","beetles","ticks") 
model_types = c("terrestrial","aquatics","phenology","beetles","ticks")



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
last_training_date <- as_date("2022-12-31")
endpoint = "data.ecoforecast.org"
use_bucket <- paste0("neon4cast-drivers/noaa/gefs-v12/stage2/parquet/0/", noaa_date)



load_stage3 <- function(site,endpoint,variables){
  message('run ', site)
  use_bucket <- paste0("neon4cast-drivers/noaa/gefs-v12/stage3/parquet/", site)
  use_s3 <- arrow::s3_bucket(use_bucket, endpoint_override = endpoint, anonymous = TRUE)
  parquet_file <- arrow::open_dataset(use_s3) |>
    dplyr::collect() |>
    dplyr::filter(datetime >= lubridate::ymd('2017-01-01'),
                  datetime <= last_training_date,
                  variable %in% variables)|> #It would be more efficient to filter before collecting, but this is not running on my M1 mac
    na.omit() |> 
    mutate(datetime = lubridate::as_date(datetime)) |> 
    group_by(datetime, site_id, variable) |> 
    summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
    pivot_wider(names_from = variable, values_from = prediction) |> 
    # convert air temp to C
    mutate(air_temperature = air_temperature - 273.15)
}


if(file.exists(here("Generate_forecasts/noaa_downloads/past_allmeteo.csv"))) {
  noaa_past_mean <- read_csv(here("Generate_forecasts/noaa_downloads/past_allmeteo.csv"))
} else {
  noaa_past_mean <- map_dfr(all_sites, load_stage3,endpoint,variables)
}



############################################ SET UP TRAINING LOOPS ###################################

##### Training function ##########


train_site <- function(sites, noaa_past_mean, target_variable) {
  message(paste0("Running ",target_variable," at all sites"))
  

  # Merge in past NOAA data into the targets file, matching by date.
  site_target <- target |>
    dplyr::select(datetime, site_id, variable, observation) |>
    dplyr::filter(variable %in% c(target_variable), 
                  site_id %in% sites) |>
    tidyr::pivot_wider(names_from = "variable", values_from = "observation") |>
    dplyr::left_join(noaa_past_mean%>%
                       filter(site_id %in% sites), by = c("datetime", "site_id"))|>
    drop_na() #removes non-complete cases - BEWARE
  

    # Tune and fit bag_tree model - making use of tidymodels
  n_folds <- 10
    #Recipe for training models
  rec_base <- recipe(site_target)|>
    step_rm(c("datetime"))|>
    update_role(everything(), new_role = "predictor")|>
    update_role({{target_variable}}, new_role = "outcome")|>
    step_normalize(all_numeric(), -all_outcomes())
  
    ## Set up tuning and fitting engine
  library(baguette)
  tune_bag_tree <- bag_tree(
    mode = "regression",
    tree_depth = tune(),
    min_n = tune(),
    cost_complexity = tune(),
    engine = "rpart"
  )
    
  #k-fold cross-validation
  bag_tree_resamp <- vfold_cv(site_target, v = n_folds, repeats = 5) #define k-fold cross validation procedure - data volume in splits prevents stratification by site_id

  
  ## Assemble workflow and tune
  wf <- workflow() %>%
    add_recipe(rec_base)
    
  #Tune models
  #If running in parallel  
  library(doParallel)
  cl <- makePSOCKcluster(14)
  registerDoParallel(cl) 
  bag_tree_grid <- tune_grid(
    wf %>% add_model(tune_bag_tree),
    resamples = bag_tree_resamp,
    grid = 40 
  )
  parallel::stopCluster(cl)
  rm(bag_tree_resamp)
  
  ## Select best model via RMSE
  best_bag_tree<-bag_tree_grid|>
    select_best(metric = "rmse")
  
  #select model with best tuning parameter by RMSE, cross-validation approach
  final_bag_tree <- finalize_workflow(
    wf %>% add_model(tune_bag_tree),
    best_bag_tree
  )
  
  final_fit <- fit(final_bag_tree, site_target)
  
  final_preds <- predict(final_fit, site_target)|>
    bind_cols(site_target)
    
  final_rmse<-final_preds|>rmse(estimate = .pred, truth = {{target_variable}})
  
  #save model fit in minimal form
  res_bundle <-
    final_fit %>%            
    butcher() %>% 
    bundle()
  
  saveRDS(res_bundle, here(paste0("Generate_forecasts/tg_bag_tree_all_sites/trained_models/", paste(theme, target_variable,"trained",Sys.Date(), sep = "-"), ".Rds")))
  tibble(theme = theme,  n_obs = nrow(site_target), target_variable = target_variable, rmse = final_rmse$.estimate, 
         tree_depth = best_bag_tree$tree_depth, min_n= best_bag_tree$min_n, cost_complexity = best_bag_tree$cost_complexity,
         last_target_date = last_training_date)
  
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
    filter(get(type)==1) 
  
  sites = site_data$field_site_id
  
  #Set target variables for each theme
  if(theme == "aquatics")           {vars = c("temperature","oxygen","chla")}
  if(theme == "phenology")          {vars = c("gcc_90","rcc_90")}
  if(theme == "terrestrial_daily")  {vars = c("nee","le")}
  if(theme == "beetles")            {vars = c("abundance","richness")}
  if(theme == "ticks")              {vars = c("amblyomma_americanum")}
  

  mod_summaries <- map(vars, possibly(
    ~train_site(sites = sites, target_variable = ., noaa_past_mean = noaa_past_mean), 
    otherwise = tibble(lambda = NA_real_)))|> #possibly only accepts static values so can't map '.x' into site or target_variable
    compact()|>
    list_rbind()|>
    drop_na()
  
  assign(x = paste0(theme, "_mod_summaries"), value = mod_summaries)
  
}

mod_sums_all <- syms(apropos("_mod_summaries"))|>
  map_dfr(~eval(.)|>bind_rows())|>
  write_csv(here("Generate_forecasts/tg_bag_tree_all_sites/model_training_summaries.csv"))

