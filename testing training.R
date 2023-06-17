#### Step 1: Load libraries
library(here)
library(ranger) #needed for random forest implementation
library(doParallel)
library(tidyverse)
library(tidymodels)
library(vip)
library(butcher)
library(bundle)
library(neon4cast)
library(lubridate)
library(rMR)
library(glue)
library(decor)
#source("ignore_sigpipe.R")
library(tsibble)
library(fable)
library(arrow)

here::i_am("Forecast_submissions/Generate_forecasts/tg_randfor_ens_parm_test/train_model.R")
source(here("Forecast_submissions/download_target.R"))

# Set model types
model_themes = "aquatics" #This model is only relevant for three themes
model_types = "aquatics"



############################################ SET UP TRAINING LOOPS ###################################

##### Training function ##########
target_variable <- "oxygen"
site <- "ARIK"
target = download_target("aquatics")
train_site(site = "ARIK", noaa_past_mean, target_variable = "temperature")

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

    # Tune and fit lasso model - making use of tidymodels
    site_target<-site_target|>
      drop_na()
    
    n_folds <- ifelse(nrow(site_target)<25, 5, 10)   #sets cross folds lower if number of complete obs is low
    
    #Recipe for training models
    rec_base <- recipe(site_target)|>
      step_rm(c("datetime", "site_id"))|>
      update_role(everything(), new_role = "predictor")|>
      update_role({{target_variable}}, new_role = "outcome")|>
      step_normalize(all_numeric(), -all_outcomes())
    
    ## Set up tuning and fitting engine
    
    tune_randfor <- rand_forest(
      mtry = tune(),
      trees = 500,
      min_n = tune()) |>
      set_mode("regression") %>%
      set_engine("ranger", importance = "impurity") 
    
    #k-fold cross-validation
    randfor_resamp <- vfold_cv(site_target, v = 2, repeats = 2)# define k-fold cross validation procedure 
    ## Assemble workflow and tune
    wf <- workflow() %>%
      add_recipe(rec_base)
    
    #Tune models
    #If running in parallel  
    library(doParallel)
    cl <- makePSOCKcluster(16) #SET 
    registerDoParallel(cl) 
    randfor_grid <- 
      tune_grid(
        wf %>% add_model(tune_randfor),
        resamples = randfor_resamp,
        grid = 2 #SET LOW FOR TESTING
      )
    
    ## Select best model via RMSE
    best_mod<-randfor_grid|>
      select_best("rmse")
    
    #select model with best tuning parameter by RMSE, cross-validation approach
    final_mod <- finalize_workflow(
      wf %>% add_model(tune_randfor),
      best_mod
    )
    
    full_training_fit <- fit(final_mod, site_target)
    
    
    ### summarize full data training fit
    
    vip <- full_training_fit|>extract_fit_parsnip()|>vip()|>pluck("data")|>
      pivot_wider(names_from = "Variable", values_from = "Importance", names_prefix = "importance_")
    
    final_preds <- predict(full_training_fit, site_target)|>
      bind_cols(site_target)
    
    final_rmse<-rmse(final_preds, estimate = .pred, truth = {{target_variable}})

    #save model fit in minimal form
    res_bundle <-
      full_training_fit %>%            
      butcher() %>% 
      bundle()
    
    saveRDS(final_mod, here(paste0("Forecast_submissions/Generate_forecasts/tg_randfor_ens_parm_test/trained_models/", 
                                    paste(theme, site, target_variable,"best_workflow",Sys.Date(), sep = "-"), ".Rds")))
    
    # collect information on full fit
    tibble(theme = theme, site = site, n_obs = nrow(site_target), n_vfolds = n_folds, target_variable = target_variable, rmse = final_rmse$.estimate, mtry = best_mod$mtry, min_n = best_mod$min_n)|>
      bind_cols(vip)
    
  }

model_themes <- "aquatics"
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
    rename(site = "sites", target_variable = "vars")|>
  filter(site == "ARIK") # for testing - 1 aq site and 1 terr site
  
  
  mod_summaries <- map2(site_var_combos$site, site_var_combos$target_variable, possibly(
    ~train_site(site = .x, target_variable = .y, noaa_past_mean = noaa_past_mean), 
    otherwise = tibble(mtry = NA_real_)))|> #possibly only accepts static values so can't map '.x' into site or target_variable
    compact()|>
    list_rbind()|>
    drop_na()
  
  assign(x = paste0(theme, "_mod_summaries"), value = mod_summaries)
  
}
