run_svy_glm <- function(df_merged_nhanes
                        , formula_regression)
{
  subset_x <- df_merged_nhanes
  
  race_i <- subset_x %>%
    pull(race) %>%
    unique(.) %>%
    as.character(.)
  # print(race_i)
  
  weight_codename <- "WT_URXBP3"
  
  # Extract the unadjusted survey weights from the subset
  unadjusted_weights <- subset_x %>%
    pull(all_of(weight_codename))
  
  # Determine the cycles that the metal biomarker was measured
  unique_cycles_x <- subset_x %>%
    pull(SDDSRVYR) %>%
    unique(.)
  # print(unique_cycles_x)
  
  # Determine the total number of cycles
  num_cycles <- length(unique_cycles_x)
  # print(num_cycles) 
  
  # Determine if the chemical biomarker had measurements in both cycle 1 and cycle 2
  indicator_cycles <- ifelse(1 %in% unique_cycles_x & 2 %in% unique_cycles_x,
                             "yes", 
                             "no")
  
  # Calculate the adjusted weights
  if(indicator_cycles == "yes")
  {
    adjusted_weights <- ifelse(subset_x$SDDSRVYR %in% c(1,2), 
                               (2/num_cycles)*unadjusted_weights,
                               (1/num_cycles)*unadjusted_weights)
  } else {
    adjusted_weights <- (1/num_cycles)*unadjusted_weights
  }
  
  # Include the adjusted weights in the subset and exclude any weights that are 0 or else the code to calculate the statistics may not run
  subset_x <- subset_x %>%
    mutate(adjusted_weights = adjusted_weights) %>%
    filter(adjusted_weights != 0)
  
  # Define the survey object to account for the sampling design
  dsn_x <- svydesign(ids = ~SDMVPSU,
                     strata = ~SDMVSTRA,
                     weights = ~adjusted_weights,
                     nest = TRUE,
                     data = subset_x)
  
  # Run the regression model
  model_x <- svyglm(as.formula(formula_regression),
                    design = dsn_x)
  # print(model_x)
  
  loglik <- logLik(model_x)
  dev <- -2*as.numeric(loglik)
  k <- length(model_x$coefficients)
  n <- subset_x %>%
    pull(adjusted_weights) %>%
    sum(.)

  df_prediction_performances <- data.frame(race = race_i
                                           , dev = dev
                                           , k = k
                                           , n = n) %>%
    mutate(BIC = dev +  k*log(n))
  
  return(df_prediction_performances)
  
}