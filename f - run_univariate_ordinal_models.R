run_univariate_ordinal_models <- function(df_nhanes
                                          , outcome
                                          , predictor
                                          , by_group)
{
  library(MASS)
  
  levels_sunscreen_usage_cat <- rev(levels(df_nhanes$sunscreen_usage_cat)) 
  # print(levels_sunscreen_usage_cat)
  
  df_nhanes <- df_nhanes %>%
    mutate(factor(sunscreen_usage_cat
                  , levels = levels_sunscreen_usage_cat))
  
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
  # print(regression_model)
  
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
      
      model_i <- polr(formula = as.formula(regression_model)
                      , data = subset_race_i
                      , Hess = TRUE)
      # print(model_i)
      
      # print(tidy(model_i))
      
  
      
      coeff_table <- coef(summary(model_i))
      
      p_values <- pnorm(abs(coeff_table[, "t value"])
                        , lower.tail = FALSE) * 2
      
      list_regression_tidy[[race_i]] <- model_i %>%
        tidy(.) %>%
        mutate(odds_ratio = exp(-estimate)) %>%
        mutate(p.value = p_values) %>%
        mutate(race = race_i) 
      # print(list_regression_tidy[[race_i]])
      
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
    
    model <- polr(formula = as.formula(regression_model)
                  , data = subset_nhanes
                  , Hess = TRUE)
    
    coeff_table <- coef(summary(model))
    
    p_values <- pnorm(abs(coeff_table[, "t value"])
                      , lower.tail = FALSE) * 2
    
    # Reason to negate estimate before performing the exponentiation
    df_tidy <- model %>%
      tidy(.) %>%
      mutate(odds_ratio = exp(-estimate)) %>%
      mutate(p.value = p_values)
    
    df_glance <- model %>%
      glance(.)
    
  }
  
  
  list_regressions <- list()
  
  list_regressions[["tidy"]] <- df_tidy
  
  list_regressions[["glance"]] <- df_glance
  
  return(list_regressions)
  
}