generate_tg_forecast <- function(forecast_date,
                                 forecast_model,
                                 model_themes = model_themes,
                                 model_id = model_id,
                                 all_sites = F,
                                 noaa = T,
                                 vars_manual = NULL) {
  #### Step 1: Define model_themes and types
  model_types = model_themes
  #Replace terrestrial daily and 30min with terrestrial
  model_types[model_types %in% c("terrestrial_daily","terrestrial_30min")] <- "terrestrial" 
  
  #### Step 2: Get NOAA driver data
  if(noaa){ #Some forecasts do not use any noaa driver data--> in that case skip download
    forecast_date <- as.Date(forecast_date)
    load_met(forecast_date) #This function loads meteorology if and only if it does not already exist
    noaa_future_daily <- read.csv(paste0("./Generate_forecasts/noaa_downloads/noaa_future_daily_",forecast_date,".csv")) |> 
      mutate(datetime = lubridate::as_date(datetime))
    
    # Load stage3 data. 
    noaa_past_mean <- read.csv(paste0("./Generate_forecasts/noaa_downloads/noaa_past_mean_",forecast_date,".csv")) |> 
      mutate(datetime = lubridate::as_date(datetime))
  } else {
    forecast_date <- as.Date(forecast_date)
    noaa_future_daily <- NULL
    noaa_past_mean <- NULL
  }
  
  ### AND HERE WE GO! We're ready to start forecasting ### 
  for (theme in model_themes) {
    if(!theme%in%c("beetles","ticks") | wday(forecast_date, label=TRUE)=="Sun"){ #beetles and ticks only want forecasts every Sunday
      #Step 1: Download latest target data and site description data
      target = download_target(theme)
      type = ifelse(theme%in% c("terrestrial_30min", "terrestrial_daily"),"terrestrial",theme)
      
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
      
      if(!is.null(vars_manual)){
        vars = vars_manual
      }
      
      ## Test with a single site first!
      #forecast <- map_dfr(vars,run_all_vars,sites[23],forecast_model,noaa_past_mean,noaa_future_daily)
      
      #Visualize the ensemble predictions -- what do you think?
      #forecast |> 
      #  ggplot(aes(x = datetime, y = prediction, group = parameter)) +
      #  geom_line(alpha=0.3) +
      #  facet_wrap(~variable, scales = "free")
      
      # Run all sites -- may be slow!
      if(all_sites == F) {
        forecast <- map_dfr(vars,
                            run_all_vars,
                            sites = sites,
                            forecast_model = forecast_model,
                            noaa_past_mean = noaa_past_mean,
                            noaa_future_daily = noaa_future_daily,
                            target = target,
                            horiz = horiz,
                            step = step,
                            theme = theme,
                            forecast_date = forecast_date)
      } else {
        forecast <- map_dfr(vars,
                            forecast_model,
                            sites = sites,
                            noaa_past_mean = noaa_past_mean,
                            noaa_future_daily = noaa_future_daily,
                            target = target,
                            horiz = horiz,
                            step = step,
                            theme = theme,
                            forecast_date = forecast_date)
      }
      
      if(theme %in% c("beetles","ticks")){
        forecast = forecast%>% 
          filter(wday(datetime, label=TRUE)=="Mon") %>% #The beetles and ticks challenges only want weekly forecasts
          mutate(duration = "P1W")
      } else {
        forecast <- forecast %>%
          mutate(duration = "P1D")
      }
      
      #Forecast output file name in standards requires for Challenge.
      # csv.gz means that it will be compressed
      file_date <- forecast_date
      forecast_file <- paste0(theme,"-",file_date,"-",model_id,".csv.gz")
      
      #Write csv to disk
      write_csv(forecast %>%
                  mutate(project_id = "neon4cast"), 
                forecast_file)
      
      # Step 5: Submit forecast!
      neon4cast::submit(forecast_file = forecast_file, metadata = NULL, ask = FALSE)
    }
  }
}
