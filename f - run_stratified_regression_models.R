run_stratified_regression_models <- function(df_nhanes 
                                             , covariates 
                                             , chemical
                                             , regression_formulas)
{
  library(broom)
  library(survey)
  library(tidyverse)
  
  df_nhanes_same_size <- df_nhanes %>%
    select(race
           , chemical
           , covariates) %>%
    na.omit(.) %>%
    filter(race != "All NHANES Women") 
  # print(dim(df_nhanes_same_size))
  
  num_regression_models <- length(regression_formulas)
  
  list_tidy <- list()
  list_glance <- list()
  
  for(i in seq(num_regression_models))
  {
    regression_model_i <- regression_formulas[i]
    print(regression_model_i)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Unweighted Models  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    df_tidy_lm <- df_nhanes_same_size %>%
      group_by(race) %>%
      do(lm(as.formula(regression_model_i)
                      , data = .) %>%
               tidy(.)) %>%
      ungroup(.) %>%
      mutate(percent_diff = (10^estimate-1)*100) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "unweighted") %>%
      mutate(type_sample_size = "same across models")
    
    df_glance_lm <- df_nhanes_same_size %>%
      group_by(race) %>%
      do(lm(as.formula(regression_model_i)
            , data = .) %>%
           glance(.)) %>%
      ungroup(.)  %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "unweighted") %>%
      mutate(type_sample_size = "same across models")
    # View(df_glance_lm)
    
    pattern_same_sample_size_i <- paste("unweighted_same_sample_size"
                                        , i
                                        , sep = "_")
    
    list_tidy[[pattern_same_sample_size_i]] <- df_tidy_lm
    
    list_glance[[pattern_same_sample_size_i]] <- df_glance_lm
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Weighted Models  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    df_nhanes_same_size_svy <- df_nhanes %>%
      select(race
             , chemical
             , covariates
             , "SDMVPSU"
             , "SDMVSTRA"
             , "WT_URXBP3") %>%
      na.omit(.) %>%
      filter(race != "All NHANES Women")
    
    df_tidy_svy <- df_nhanes_same_size_svy %>%
      group_by(race) %>%
      do(svyglm_stratified_model(.
                                 , regression_formula = regression_model_i
                                 , stats = "tidy")) %>%
      ungroup(.) %>%
      mutate(percent_diff = (10^estimate-1)*100) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "weighted") %>%
      mutate(type_sample_size = "same across models")
    # print(df_tidy_svy)
    
    df_glance_svy <- df_nhanes_same_size_svy %>%
      # filter(race == "Non-Hispanic White") %>%
      group_by(race) %>%
      do(svyglm_stratified_model(.
                                 , regression_formula = regression_model_i
                                 , stats = "glance")) %>%
      ungroup(.) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "weighted") %>%
      mutate(type_sample_size = "same across models")
    
    pattern_svy_same_sample_size_i <- paste("weighted_same_sample_size"
                                            , i
                                            , sep = "_")
    
    list_tidy[[pattern_svy_same_sample_size_i]] <- df_tidy_svy
    
    list_glance[[pattern_svy_same_sample_size_i]] <- df_glance_svy
    
    
  }
  
  df_tidy <- reduce(list_tidy
                    , full_join
                    , by = NULL) %>%
    mutate(perc_diff_ci_low = percent_diff - 1.96*((10^std.error-1)*100)) %>%
    mutate(perc_diff_ci_high = percent_diff + 1.96*((10^std.error-1)*100)) %>%
    mutate(asterisks = case_when(p.value > 0.05 ~ ""
                                 , p.value > 0.01 & p.value <= 0.05 ~ "*"
                                 , p.value > 0.001 & p.value <= 0.01 ~ "**"
                                 , p.value <= 0.001 ~ "***")) %>%
    mutate(covariates = gsub("log10\\(URXBP3\\) \\~ weight_perception \\+ |log10\\(URXBP3\\) \\~ race (\\+ |\\+ weight_perception \\+ )"
                             , ""
                             , regression_formula))
  # View(df_tidy)

  df_glance <- reduce(list_glance
                      , full_join
                      , by = NULL) %>%
    mutate(covariates = gsub("log10\\(URXBP3\\) \\~ weight_perception \\+ |log10\\(URXBP3\\) \\~ race (\\+ |\\+ weight_perception \\+ )"
                             , ""
                             , regression_formula))
  # View(df_glance)


  list_regression <- list()

  list_regression[["tidy"]] <- df_tidy 

  list_regression[["glance"]] <- df_glance

  return(list_regression)
  
}