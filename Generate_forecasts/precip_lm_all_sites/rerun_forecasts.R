source("./Generate_forecasts/precip_lm_all_sites/forecast_model.R")
source("./Generate_forecasts/R/rerun_forecasts.R")

END <- as_date('2023-10-10') #Re-run if forecasts have not been re-run after Oct 10, fixing meteorology issue

rerun_forecasts(model_id, forecast_model, model_themes, END, noaa = T, all_sites = T)