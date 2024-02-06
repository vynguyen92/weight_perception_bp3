run_univariate_ordinal_models <- function(df_nhanes
                                          , outcome
                                          , predictor
                                          , by_group)
{
  library(MASS)
  library(tidyverse)
  library(broom)
  library(xlsx)
  
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
  
  list_regressions[["tidy"]] <- df_tidy %>%
    mutate(estimate = -estimate) %>%
    mutate(readable_term = gsub("race_weight_perception_|race"
                                , ""
                                , term) %>%
             gsub("_about"
                  , " perceived about"
                  , .) %>%
             gsub("_overweight"
                  , " perceived overweight"
                  , .) %>%
             gsub("INDFMPIR"
                  , "Poverty income ratio"
                  , .) %>%
             gsub("RIDAGEYR"
                  , "Age"
                  , .) %>%
             gsub("weight_perception perceived overweight"
                  , "Perceived overweight vs. perceived at right weight"
                  , .) %>%
             gsub("_always\\|_most of the time"
                  , "always vs. (most of the time, sometimes, rarely, and never)"
                  , .) %>%
             gsub("_most of the time\\|_sometimes"
                  , "most of the time vs. (sometimes, rarely, and never)"
                  , .) %>%
             gsub("_sometimes\\|_rarely"
                  , "sometimes vs. (rarely and never)"
                  , .) %>%
             gsub("_rarely\\|_never"
                  , "rarely vs. (never)"
                  , .) 
             ) %>%
    mutate(readable_term = ifelse(grepl("race", term) == TRUE
                                  , paste(readable_term
                                          , " vs. non-Hispanic Black"
                                          , sep = "")
                                  , readable_term)) %>%
    relocate(readable_term
             , .after = "term")
  # View(list_regressions[["tidy"]])
  
  list_regressions[["glance"]] <- df_glance
  
  file_name <- paste("ordinal_logistic_sunscreen_"
                     , ifelse(by_group == TRUE
                              , "stratified_by_race_"
                              , "")
                     , paste(predictor
                             , collapse = "_")
                     , ".xlsx"
                     , sep = "")
  # print(file_name)
  
  write.xlsx(x = list_regressions[["tidy"]]
             , file = file_name
             , sheetName = "tidy")

  write.xlsx(x =  list_regressions[["glance"]]
             , file = file_name
             , sheetName = "glance"
             , append = TRUE)
  
  return(list_regressions)
  
}