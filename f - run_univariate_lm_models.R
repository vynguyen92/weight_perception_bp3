run_univariate_lm_models <- function(df_nhanes
                                     , outcome
                                     , predictor
                                     , by_group)
{
  num_predictors <- length(predictor)
  
  if(num_predictors == 1)
  {
    regression_model <- paste(outcome
                              , " ~ "
                              , predictor
                              , sep = "")
  } else {
    regression_model <- paste(outcome
                              , " ~ "
                              , predictor %>%
                                paste(.
                                      , collapse = " + ")
                              , sep = "")
  }
  
  
  print(regression_model)
  
  if(by_group == TRUE)
  {
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
    
    
    
  } else {
    
    subset_nhanes <- df_nhanes %>%
      filter(race != "All NHANES Women") %>%
      mutate(race = factor(race) %>% 
               relevel(., ref = "Non-Hispanic Black")) %>%
      mutate(race_weight_perception = ifelse(race == "Non-Hispanic Black"
                                             , "Non-Hispanic Black"
                                             , race_weight_perception) %>%
               factor(.) %>%
               relevel(.
                       , ref = "Non-Hispanic Black"))
    
    model <- lm(formula = as.formula(regression_model)
                , data = subset_nhanes)
    
    df_tidy <- model %>%
      tidy(.)
    
    df_glance <- model %>%
      glance(.)
    
  }
  
  list_regressions <- list()
  
  list_regressions[["tidy"]] <- df_tidy
  
  list_regressions[["glance"]] <- df_glance
  
  return(list_regressions)
}