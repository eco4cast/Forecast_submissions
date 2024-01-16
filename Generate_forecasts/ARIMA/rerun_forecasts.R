source("./Generate_forecasts/ARIMA/forecast_model.R")
source("./Generate_forecasts/R/rerun_forecasts.R")

END <- as_date('2023-11-13') #Re-run if forecasts have not been re-run after Nov 13, fixing calibration issue

rerun_forecasts(model_id, forecast_model, model_themes, END, noaa = F)
