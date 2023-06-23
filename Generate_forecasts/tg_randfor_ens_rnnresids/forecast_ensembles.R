#This script will combine darts residuals with tidymodels predictions
library(here)
library(tidyverse)
here::i_am("Forecast_submissions/Generate_forecasts/tg_randfor_ens_rnnresids/forecast_ensembles.R")
# Total workflow
## train_model.R - Tidymodels trains hyperparameters
## fit_models.R - Tidymodels fits models (send to darts) and makes forecast predictions (change: DO NOT SUBMIT FORECASTS AT THSI POINT)
## predict_resids.py - Darts fits residual model and projects for forecast duration into future
## combine_and_forecast.R pulls darts residual predictions and combines with forecast predictions - submits


assemble_forecasts <- function(theme, site, target_variable){

  type = ifelse(theme%in% c("terrestrial_30min", "terrestrial_daily"),"terrestrial",theme)
  site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv")|>
    filter(get(type)==1)
  sites = site_data$field_site_id
  theme_sites <- expand_grid(sites, vars)

  # Pull predicted residuals
pull_predicted_resids <- function(theme, site, target_variable){
  
  resid_files <- list.files(here("Forecast_submissions/Generate_forecasts/tg_randfor_ens_rnnresids/resid_prediction_dfs"), 
                          pattern = "predicted_residuals")
  
  pred_resids <- map(resid_files, ~read_csv(here(paste0("Forecast_submissions/Generate_forecasts/tg_randfor_ens_rnnresids/resid_prediction_dfs/", 
                                             paste(theme, site, target_variable, sep = "-"), 
                                             "_predicted_residuals.csv")))|>  
                    rowwise()|>
                    group_by(datetime)|>summarize(upper95_resid = quantile(c_across(everything()),0.95))|>
                    mutate(datetime = as_datetime(datetime, format = "%m/%d/%Y"),
                           site_id = site, variable = target_variable))|>
  list_rbind()
}

pred_resids <- map2(theme_sites$sites, theme_sites$vars, 
                    possibly(~pull_predicted_resids(theme, site = .x, target_variable = .y),
                             otherwise = data.frame(upper95_resid = NA_real_)))|>
  list_rbind()|>
  filter(!is.na(upper95_resid))

#pull ml forecasted predictions
pull_forecasts <- function(theme){
  
  forecasts_file <- list.files(here("Forecast_submissions/Generate_forecasts/tg_randfor_ens_rnnresids/model_forecast_dfs"), 
                             pattern = theme)
  theme_forecasts <- read_csv(here(paste0("Forecast_submissions/Generate_forecasts/tg_randfor_ens_rnnresids/model_forecast_dfs/",
                                       forecasts_file)))
}
theme_forecasts <- pull_forecasts(theme)

#add resids to forecasts
forecast_ml_resids <- theme_forecasts|>
  group_by(datetime, site_id, variable)|>
  summarize(prediction = mean(prediction))|>
  left_join(pred_resids, by = c("datetime", "site_id", "variable"))|>
  mutate(prediction_upper = prediction + upper95_resid,
         prediction_lower = prediction - upper95_resid)

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
    
    forecast <- assemble_forecasts(theme, site, vars)
    
    
    #do some post-processing 

    #Forecast output file name in standards requires for Challenge.
    # csv.gz means that it will be compressed
    ##file_date <- Sys.Date() #forecast$reference_datetime[1]
    #model_id = "tg_randfor_ens_rnnresids"
    #forecast_file <- paste0(theme,"-",file_date,"-",model_id,".csv.gz")
    
    
    #Write csv to disk
    #write_csv(forecast, forecast_file)
    
    
    # Step 5: Submit forecast!
    # neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)
  }
}















########### TEST PLOTS ###############
predictions |>
  filter(parameter == 1)|>
  ggplot()+
  geom_line(aes(x = datetime, y = prediction))+
  geom_line(aes(x = datetime, y = prediction_upper), linetype = 2)+
  geom_line(aes(x = datetime, y = prediction_lower), linetype = 2)


predictions|>
  group_by(datetime)|>
  summarize(prediction = mean(prediction),
            prediction_upper = mean(prediction_upper),
            prediction_lower = mean(prediction_lower))|>
  ggplot()+
  geom_line(aes(x = datetime, y = prediction))+
  geom_line(aes(x = datetime, y = prediction_upper), linetype = 2)+
  geom_line(aes(x = datetime, y = prediction_lower), linetype = 2)


forecast|>filter(site_id == "ARIK", variable == "oxygen", !is.na(prediction_upper))|>
  ggplot()+
  geom_line(aes(x = datetime, y = prediction))+
  geom_line(aes(x = datetime, y = prediction_upper), linetype = 2)+
  geom_line(aes(x = datetime, y = prediction_lower), linetype = 2)

