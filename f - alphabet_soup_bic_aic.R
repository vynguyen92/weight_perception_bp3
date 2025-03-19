alphabet_soup_bic_aic <- function(regression_stratified
                                  , regression_all
                                  , name_of_folder
                                  , current_directory
                                  , df_nhanes
                                  , covariates
                                  , chemical
                                  , intercept_model_string
                                  , is_adult = TRUE)
{
  library(tidyverse)
  library(xlsx)
  
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Make a new folder if the folder doesn't exist
  if(name_of_folder %in% all_files_in_current_directory)
  {
    
  } else {
    dir.create(name_of_folder)
  }
  
  # Define a string for the working directory for the new folder
  new_working_directory <- paste(current_directory
                                 , name_of_folder
                                 , sep = "/")
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  relevant_covariates <- regression_stratified[["glance"]] %>%
    pull(covariates) %>%
    unique(.)
  # print(relevant_covariates)
  
  df_aic_bic <- regression_stratified[["glance"]] %>%
    full_join(.
              , regression_all[["glance"]] %>%
                mutate(race = "All NHANES Women")
              , by = NULL) %>%
    filter(regression_formula != "log10(URXBP3) ~ race + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI") %>%
    filter(covariates %in% relevant_covariates) %>%
    filter(type_sample_size == "same across models") %>%
    #   filter(grepl("race \\+ "
    #                , regression_formula) == FALSE) %>%
    mutate(sunscreen_included = ifelse(grepl("sunscreen_usage_ordinal", regression_formula) == TRUE
                                       , "with sunscreen usage"
                                       , "without sunscreen usage")) %>%
    mutate(baseline_covariates = gsub(" \\+ sunscreen_usage_ordinal"
                                      , ""
                                      , covariates)) %>%
    mutate(race_weight_perception_together = ifelse(grepl("race_weight_perception", regression_formula) == TRUE
                                                    , "together"
                                                    , "separate"))
  # View(df_aic_bic)
  
  df_stats <- df_aic_bic %>%
    group_by(race
             , race_weight_perception_together
             , baseline_covariates
             , account_sampling_design
             , type_sample_size) %>%
    summarise(bic_sunscreen_contribution = diff(BIC)
              , aic_sunscreen_contribution = diff(AIC)) %>%
    ungroup(.) %>%
    pivot_longer(cols = c("bic_sunscreen_contribution"
                          , "aic_sunscreen_contribution")
                 , names_to = "prediction_performance_type") %>%
    mutate(prediction_performance_type = gsub("_sunscreen_contribution"
                           , ""
                           , prediction_performance_type))
  # View(df_stats)
  
  df_combination <- df_stats  %>%
    select(account_sampling_design
           , baseline_covariates
           , type_sample_size
           , prediction_performance_type) %>%
    unique(.) %>%
    mutate(combination = paste(account_sampling_design
                               , type_sample_size
                               , prediction_performance_type
                               , baseline_covariates
                               , sep = " - "))
  View(df_combination)
  
  num_combinations <- nrow(df_combination)
  
  list_wide_regressions <- list()
  
  for(i in seq(num_combinations)[1])
  {
    subset_combination <- df_combination[i,]
    # print(subset_combination)
    
    combination_i <- subset_combination %>%
      pull(combination)
    print(combination_i)
  
    type_sampling_design_i <- subset_combination %>%
      pull(account_sampling_design)
    
    type_sample_size_i <- subset_combination %>%
      pull(type_sample_size)
    
    prediction_performance_type_i <- subset_combination %>%
      pull(prediction_performance_type)
    
    baseline_covariates_i <- subset_combination %>%
      pull(baseline_covariates)
    
    subset_stats_contribution <- df_stats %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i) %>%
      filter(prediction_performance_type == prediction_performance_type_i) %>%
      filter(baseline_covariates == baseline_covariates_i)
    # print(subset_stats_contribution)
    # print(colnames(subset_stats_contribution))
    
    subset_stats_pred_perf <- df_aic_bic %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i) %>%
      filter(baseline_covariates == baseline_covariates_i)
    View(subset_stats_pred_perf)
    # print(colnames(subset_stats_pred_perf))
    
    prediction_performance_pattern <- toupper(prediction_performance_type_i)
    # print(prediction_performance_pattern)
    
    subset_sunscreen_wide <- subset_stats_pred_perf %>%
      select(race
             , sunscreen_included
             , race_weight_perception_together
             , all_of(prediction_performance_pattern)) %>%
      mutate(sunscreen_included = gsub(" "
                                       , "_"
                                       , sunscreen_included)) %>%
      pivot_wider(names_from = "sunscreen_included"
                  , values_from = prediction_performance_pattern) %>%
      full_join(.
                , subset_stats_contribution
                , by = c("race"
                         , "race_weight_perception_together")) %>%
      rename(value_explained_by_sunscreen = "value")
    View(subset_sunscreen_wide)
    
    intercept_model <- run_intercept_model(df_nhanes = df_nhanes
                                           , covariates = covariates
                                           , chemical = chemical
                                           , intercept_model_string = intercept_model_string
                                           , type_sampling_design = type_sampling_design_i
                                           , prediction_performance_stat = prediction_performance_pattern)
    
  }
  
  
  
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  
  # write.xlsx(x = df_wide_regression
  #            , file = "r2_without_vs_with_sunscreen_adjustment.xlsx"
  #            , sheetName = "tidy")
  
  # alphabet_soup_plot
}