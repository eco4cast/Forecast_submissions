#Quick function to repeat for all variables
run_all_vars = function(var,
                        sites,
                        forecast_site,
                        noaa_past_mean,
                        noaa_future_daily,
                        target,
                        horiz,
                        step) {
  
  message(paste0("Running variable: ", var))
  forecast <- map_dfr(sites,
                      forecast_site,
                      noaa_past_mean,
                      noaa_future_daily,
                      var,
                      target,
                      horiz,
                      step)
  
}