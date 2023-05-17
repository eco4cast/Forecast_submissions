# EFI Theory

A repository for forecast development and analysis as part of the Ecological Forecasting Initiative (EFI) Theory Working Group.

We are currently working on developing forecast models that can be used across multiple NEON data streams in the Ecological Forecasting Initiative's forecast challenge. Code to run forecasts is in ./Generate_forecasts, with subfolders for each forecast model. Models are run automatically every day through github actions.

If you are interested in joining the project, we would love for you to help! See contact information for Abby Lewis below to get in touch. Here are some possible places to start:
- ./Generate_forecasts/temp_lm/forecast_model.R provides a good starting point with a very basic linear model
- ./Generate_forecasts/ETS/forecast_model.R provides a good start point for a time series model
- ./Generate_forecasts/tg_lasso/train_model.R and ./Generate_forecasts/tg_lasso/forecast_model.R provide a good starting point for a separated train-forecast workflow for machine learning, 
where model hyperparameter tuning is too intensive to perform every forecast, using tidymodels. tg_lasso and tg_randfor models do not currently train on new observations since training. 

Models with "_all_sites" suffix include 'site' in the predictor set. Occasionally, model objects trained on all sites (e.g., randfor_all_sites) can be excessively large (>100 MB) even after butchering. In such cases, Github will disallow pushing these files to the repo.
Automation is done using .yml files, which you can find and modify in .github/workflows

Please note, we are adding the prefix "tg_" to the start of all theory group models for consistency. 

## Contact

- Abigail Lewis, Virginia Tech, aslewis@vt.edu
