run_univariate_lm_models <- function(df_nhanes
                                  , outcome
                                  , predictor)
{
  regression_model <- paste(outcome
                            , " ~ "
                            , predictor
                            , sep = "")
  print(regression_model)
  
  races <- df_nhanes$race %>% unique(.)
  
  num_groups <- length(races)
  
  list_regression_tidy <- list()
  
  list_regression_glance <- list()
  
  for(i in seq(num_groups))
  {
    race_i <- races[i]
    
    subset_race_i <- df_nhanes %>%
      filter(race == race_i) 
    
    model_i <- lm(formula = as.formula(regression_model)
                  , data = subset_race_i)
    # print(model_i)
    
    list_regression_tidy[[race_i]] <- model_i %>%
      tidy(.) %>%
      mutate(race = race_i)
    
    list_regression_glance[[race_i]] <-model_i %>%
      glance(.) %>%
      mutate(race = race_i)
  }
  
  df_tidy <- list_regression_tidy %>%
    reduce(.
           , full_join)
  
  df_glance <- list_regression_glance %>%
    reduce(.
           , full_join)
  
  list_regressions <- list()
  
  list_regressions[["tidy"]] <- df_tidy
  
  list_regressions[["glance"]] <- df_glance
  
  return(list_regressions)
}