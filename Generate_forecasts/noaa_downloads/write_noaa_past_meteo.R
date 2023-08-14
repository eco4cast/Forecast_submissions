#download past noaa meteorological data for each site and write to file - will only exist in checkout repo on docker container
library(tidyverse)
library(lubridate)
library(arrow)
library(neon4cast)
library(here)
#source(here("Forecast_submissions/ignore_sigpipe.R"))

here::i_am("Forecast_submissions/Generate_forecasts/noaa_downloads/write_noaa_past_meteo.R")

# Set up
model_types = c("terrestrial","aquatics","phenology","beetles","ticks")

noaa_date <- Sys.Date() - lubridate::days(1)

variables <- c('air_temperature',
               "surface_downwelling_longwave_flux_in_air",
               "surface_downwelling_shortwave_flux_in_air",
               "precipitation_flux",
               "air_pressure",
               "relative_humidity",
               "air_temperature",
               "northward_wind",
               "eastward_wind")

endpoint = "data.ecoforecast.org"

site_data <- readr::read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-targets/main/NEON_Field_Site_Metadata_20220412.csv") %>%
  filter(if_any(matches(model_types),~.==1))
all_sites = site_data$field_site_id


# Define function to download and tidy noaa data
load_stage3 <- function(site,endpoint,variables){
  message('run ', site)
  use_bucket <- paste0("neon4cast-drivers/noaa/gefs-v12/stage3/parquet/", site)
  use_s3 <- arrow::s3_bucket(use_bucket, endpoint_override = endpoint, anonymous = TRUE)
  parquet_file <- arrow::open_dataset(use_s3) |>
    dplyr::collect() |>
    dplyr::filter(datetime >= lubridate::ymd('2017-01-01'),
                  variable %in% variables)|> #It would be more efficient to filter before collecting, but this is not running on my M1 mac
    na.omit() |> 
    mutate(datetime = lubridate::as_date(datetime)) |> 
    group_by(datetime, site_id, variable) |> 
    summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
    pivot_wider(names_from = variable, values_from = prediction) |> 
    # convert air temp to C
    mutate(air_temperature = air_temperature - 273.15)
}

noaa_past_mean <- map_dfr(all_sites, load_stage3,endpoint,variables)

write_csv(noaa_past_mean, here("Forecast_submissions/Generate_forecasts/noaa_downloads/updated_noaa_past_meteo.csv"))
