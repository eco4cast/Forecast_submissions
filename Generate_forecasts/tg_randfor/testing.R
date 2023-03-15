site = "KING"
target_variable <- "oxygen"
target <- download_target("aquatics")


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
    site_target<-site_target|>
      drop_na()
    #Recipe for training models
    rec_base <- recipe(site_target)|>
      step_rm(c("datetime", "site_id"))|>
      update_role(everything(), new_role = "predictor")|>
      update_role({{target_variable}}, new_role = "outcome")|>
      step_normalize(all_numeric(), -all_outcomes())
    
    ## Set up tuning and fitting engine
    
    tune_randfor <- rand_forest(
      mtry = tune(),
      trees = 20, #SET LOW FOR TESTING - trees 500/
      min_n = tune()) |>
      set_mode("regression") %>%
      set_engine("ranger", importance = "impurity") 
    
    #k-fold cross-validation
    randfor_resamp <- vfold_cv(site_target, v = 5, repeats = 1) #SET LOW FOR TESTING repeats = 5/ define k-fold cross validation procedure 
    ## Assemble workflow and tune
    wf <- workflow() %>%
      add_recipe(rec_base)
    
    #Tune models
    #If running in parallel  
    library(doParallel)
    cl <- makePSOCKcluster(3) #SET LOW FOR TESTING
    registerDoParallel(cl) 
    randfor_grid <- tune_grid(
      wf %>% add_model(tune_randfor),
      resamples = randfor_resamp,
      grid = 20
    )
    
    ## Select best model via RMSE
    best_mod<-randfor_grid|>
      select_best("rmse")
    
    #select model with best tuning parameter by RMSE, cross-validation approach
    final_mod <- finalize_workflow(
      wf %>% add_model(tune_randfor),
      best_mod
    )
    
    final_fit <- fit(final_mod, site_target)
    
    vip <- final_fit|>extract_fit_parsnip()|>vip()|>pluck("data")|>
      pivot_wider(names_from = "Variable", values_from = "Importance", names_prefix = "importance_")
    
    
    final_preds <- predict(final_fit, site_target)|>
      bind_cols(site_target)
    
    final_rmse<-rmse(final_preds, estimate = .pred, truth = {{target_variable}})
    #try to extract fit and write to tibble variable importance as columns bind_cols
    
    #save model fit in minimal form
    res_bundle <-
      final_fit %>%            
      butcher() %>% 
      bundle()
    
    
    saveRDS(res_bundle, here(paste0("EFI_Theory/Generate_forecasts/tg_randfor/trained_models/", paste(theme, site, target_variable,"trained",Sys.Date(), sep = "-"), ".Rds")))
    test<-tibble(theme = theme, site = site, target_variable = target_variable, rmse = final_rmse$.estimate, mtry = best_mod$mtry, min_n = best_mod$min_n)|>
      bind_cols(vip)
    
  }
}

ggplot(final_preds)+geom_point(aes(x= datetime, y = .pred))+geom_point(aes(x= datetime, y = oxygen), color = "red")
