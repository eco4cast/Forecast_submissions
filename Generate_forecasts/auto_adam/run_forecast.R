source("./Generate_forecasts/auto_adam/forecast_model.R")

tryCatch({
  generate_tg_forecast(forecast_date = Sys.Date(),
                       forecast_model = forecast_model,
                       model_themes = model_themes,
                       model_id = model_id,
                       noaa = F)
}, error=function(e){cat("ERROR with forecast generation:\n",conditionMessage(e), "\n")})
