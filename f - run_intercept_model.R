run_intercept_model <- function(df_nhanes 
                                , covariates 
                                , chemical
                                , intercept_model_string
                                , type_sampling_design
                                , prediction_performance_stat)
{
  library(broom)
  library(survey)
  library(tidyverse)
  library(xlsx)
  
  df_nhanes_same_size <- df_nhanes %>%
    select(race
           , chemical
           , covariates) %>%
    na.omit(.) %>%
    filter(race != "All NHANES Women") %>%
    mutate(race = factor(race) %>%
             relevel(.
                     , ref = "Non-Hispanic Black")) %>%
    mutate(race_weight_perception = ifelse(race == "Non-Hispanic Black"
                                           , "Non-Hispanic Black"
                                           , race_weight_perception)) %>%
    mutate(race_weight_perception = factor(race_weight_perception) %>%
             relevel(.
                     , ref = "Non-Hispanic Black"))
  
  if(type_sampling_design == "unweighted") 
  {
    
    lm_model_same_sample_size <- lm(as.formula(intercept_model_string)
                                    , data = df_nhanes_same_size)
    
    df_glance_same_sample_size_i <- lm_model_same_sample_size %>%
      glance(.) %>%
      mutate(regression_formula = intercept_model_string) %>%
      mutate(account_sampling_design = "unweighted") %>%
      mutate(type_sample_size = "same across models") %>%
      mutate(race = "All NHANES Women")
    # print(df_glance_same_sample_size_i)
    
    df_race_glance <- df_nhanes_same_size %>%
      group_by(race) %>%
      do(lm(as.formula(intercept_model_string)
                , data = .) %>%
           glance(.)) %>%
      ungroup(.) %>%
      mutate(regression_formula = intercept_model_string) %>%
      mutate(account_sampling_design = "unweighted") %>%
      mutate(type_sample_size = "same across models")
    # View(df_race_glance)
    
    df_glance_intercept_model <- df_race_glance %>%
      full_join(.
                , df_glance_same_sample_size_i) %>%
      select(race
             , all_of(prediction_performance_stat)) %>%
      rename(BIC_max = BIC)
    # View(df_glance_intercept_model)
    
  } else if(type_sampling_design == "weighted") {
    
    df_nhanes_same_size_svy <- df_nhanes %>%
      select(race
             , chemical
             , covariates
             , "SDMVPSU"
             , "SDMVSTRA"
             , "WT_URXBP3") %>%
      na.omit(.) %>%
      filter(race != "All NHANES Women") %>%
      mutate(race = factor(race) %>%
               relevel(.
                       , ref = "Non-Hispanic Black")) %>%
      mutate(race_weight_perception = ifelse(race == "Non-Hispanic Black"
                                             , "Non-Hispanic Black"
                                             , race_weight_perception)) %>%
      mutate(race_weight_perception = factor(race_weight_perception) %>%
               relevel(.
                       , ref = "Non-Hispanic Black"))
    # print(dim(df_nhanes_same_size_svy))
    
    num_cycles <- df_nhanes_same_size_svy$SDDSRVYR %>%
      unique(.) %>%
      length(.)
    
    df_nhanes_same_size_svy <- df_nhanes_same_size_svy %>%
      mutate(adjusted_weights = WT_URXBP3/num_cycles)
    # print(colnames(df_nhanes_same_size_svy))
    
    options(survey.lonely.psu="remove")
    
    nhanes_design_same_size <- svydesign(ids = ~SDMVPSU
                                         , strata = ~SDMVSTRA
                                         , weights = ~adjusted_weights
                                         , nest = TRUE
                                         , data = df_nhanes_same_size_svy)
    
    svy_model_same_sample_size <- svyglm(as.formula(intercept_model_string)
                                         , design = nhanes_design_same_size)
    
    df_observations_race <- df_nhanes_same_size_svy %>%
      group_by(race) %>%
      summarize(n = sum(adjusted_weights)) %>%
      ungroup(.)
    # View(df_observations_race)
    
    loglik <- logLik(svy_model_same_sample_size)
    dev <- -2*as.numeric(loglik)
    k <- length(svy_model_same_sample_size$coefficients)
    n <- df_nhanes_same_size_svy %>%
      pull(adjusted_weights) %>%
      sum(.)  #nrow(df_nhanes_same_size_svy)
    
    prediction_performance_stat_value_US_women <- dev +  k*log(n)
    
    df_race_glance <- df_nhanes_same_size_svy %>%
      group_by(race) %>%
      do(run_svy_glm(df_merged_nhanes = .
                     , formula_regression = intercept_model_string)) %>%
      ungroup(.)
    # View(df_race_glance)
    

    df_glance_intercept_model <- df_race_glance %>%
      select(-n) %>%
      full_join(.
                , df_observations_race) %>%
      full_join(.
                , data.frame(race = "All NHANES Women"
                             , k = k
                             , n = n
                             , BIC = prediction_performance_stat_value_US_women))  %>%
      select(race
             , BIC
             , n) %>%
      rename(BIC_max = BIC)
    # View(df_glance_intercept_model)
    
    # glance() doesn't work with the svy model
    # df_glance_svy_same_sample_size_i <- svy_model_same_sample_size %>%
    #   glance(.) %>%
    #   mutate(regression_formula = intercept_model_string)  %>%
    #   mutate(account_sampling_design = "weighted") %>%
    #   mutate(type_sample_size = "same across models") %>%
    #   mutate(r.squared = calculate_rsq_svy(model_object = svy_model_same_sample_size
    #                                        , df_nhanes = df_nhanes_same_size_svy
    #                                        , svy_design = nhanes_design_same_size
    #                                        , stats = "rsq")
    #          , adj.r.squared = calculate_rsq_svy(model_object = svy_model_same_sample_size
    #                                              , df_nhanes = df_nhanes_same_size_svy
    #                                              , svy_design = nhanes_design_same_size
    #                                              , stats = "adjusted_rsq")
    #   )
    
  } else {
    
    print("Invalid name for type of sampling design")
    
  }
  
  # print(df_glance_intercept_model)
  return(df_glance_intercept_model)
}