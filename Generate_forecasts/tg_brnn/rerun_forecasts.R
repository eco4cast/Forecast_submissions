source("./Generate_forecasts/tg_brnn/forecast_model.R")
source("./Generate_forecasts/R/rerun_forecasts.R")

END <- as_date('2023-10-10') #Re-run if forecasts have not been re-run after Oct 10, fixing meteorology issue

rerun_forecasts(model_id, forecast_model, model_themes, END, noaa = T)