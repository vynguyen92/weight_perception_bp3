run_regression_models <- function(df_nhanes 
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
  # print(dim(df_nhanes_same_size))
  
  num_regression_models <- length(regression_formulas)
  
  list_tidy <- list()
  list_glance <- list()
  
  for(i in seq(num_regression_models))
  {
    regression_model_i <- regression_formulas[i]
    print(regression_model_i)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Unweighted Models with Same Sample Size  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    lm_model_same_sample_size <- lm(as.formula(regression_model_i)
                                    , data = df_nhanes_same_size)

    df_tidy_same_sample_size_i <- lm_model_same_sample_size %>%
      tidy(.) %>%
      mutate(percent_diff = (10^estimate-1)*100) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "unweighted") %>%
      mutate(type_sample_size = "same across models")

    df_glance_same_sample_size_i <- lm_model_same_sample_size %>%
      glance(.) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "unweighted") %>%
      mutate(type_sample_size = "same across models")

    pattern_same_sample_size_i <- paste("unweighted_same_sample_size"
                                        , i
                                        , sep = "_")

    list_tidy[[pattern_same_sample_size_i]] <- df_tidy_same_sample_size_i

    list_glance[[pattern_same_sample_size_i]] <- df_glance_same_sample_size_i
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Weighted Models with Same Sample Size  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
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

    nhanes_design_same_size <- svydesign(ids = ~SDMVPSU
                                         , strata = ~SDMVSTRA
                                         , weights = ~adjusted_weights
                                         , nest = TRUE
                                         , data = df_nhanes_same_size_svy)

    svy_model_same_sample_size <- svyglm(as.formula(regression_model_i)
                                         , design = nhanes_design_same_size)

    df_tidy_svy_same_sample_size_i <- svy_model_same_sample_size %>%
      tidy(.) %>%
      mutate(percent_diff = (10^estimate-1)*100) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "weighted") %>%
      mutate(type_sample_size = "same across models") 
    # print(df_tidy_svy_same_sample_size_i)

    df_glance_svy_same_sample_size_i <- svy_model_same_sample_size %>%
      glance(.) %>%
      mutate(regression_formula = regression_model_i)  %>%
      mutate(account_sampling_design = "weighted") %>%
      mutate(type_sample_size = "same across models")
    # print(df_glance_svy_same_sample_size_i)

    pattern_svy_same_sample_size_i <- paste("weighted_same_sample_size"
                                        , i
                                        , sep = "_")

    list_tidy[[pattern_svy_same_sample_size_i]] <- df_tidy_svy_same_sample_size_i

    list_glance[[pattern_svy_same_sample_size_i]] <- df_glance_svy_same_sample_size_i
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Unweighted Models with Max Sample Size  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    specific_covariates <- regression_model_i %>%
      gsub("log10\\(URXBP3\\) ~ "
           , ""
           , .) %>%
      strsplit(.
               , split = "\\s\\+\\s") %>%
      unlist(.)
    # print(specific_covariates)

    specific_covariates <- c(specific_covariates
                             , "race_weight_perception"
                             , "race") %>%
      unique(.)

    df_nhanes_dif_size <- df_nhanes %>%
      filter(race != "All NHANES Women") %>%
      select("URXBP3"
             , all_of(specific_covariates)) %>%
      na.omit(.) %>%
      mutate(race = factor(race) %>%
               relevel(.
                       , ref = "Non-Hispanic Black")) %>%
      mutate(race_weight_perception = ifelse(race == "Non-Hispanic Black"
                                             , "Non-Hispanic Black"
                                             , race_weight_perception)) %>%
      mutate(race_weight_perception = factor(race_weight_perception) %>%
               relevel(.
                       , ref = "Non-Hispanic Black"))
    # print(dim(df_nhanes_dif_size))

    lm_model_max_sample_size <- lm(as.formula(regression_model_i)
                                   , data = df_nhanes_dif_size)

    df_tidy_max_sample_size_i <- lm_model_max_sample_size %>%
      tidy(.)  %>%
      mutate(percent_diff = (10^estimate-1)*100) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "unweighted") %>%
      mutate(type_sample_size = "max sample size")

    df_glance_max_sample_size_i <- lm_model_max_sample_size %>%
      glance(.) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "unweighted") %>%
      mutate(type_sample_size = "max sample size")

    pattern_max_sample_size_i <- paste("unweighted_max_sample_size"
                                       , i
                                       , sep = "_")

    list_tidy[[pattern_max_sample_size_i]] <- df_tidy_max_sample_size_i

    list_glance[[pattern_max_sample_size_i]] <- df_glance_max_sample_size_i
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Weighted Models with Max Sample Size  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    df_svy_nhanes_dif_size <- df_nhanes %>%
      filter(race != "All NHANES Women") %>%
      select("URXBP3"
             , all_of(specific_covariates)
             , "SDMVPSU"                
             , "SDMVSTRA"
             , "WT_URXBP3") %>%
      na.omit(.) %>%
      mutate(race = factor(race) %>%
               relevel(.
                       , ref = "Non-Hispanic Black")) %>%
      mutate(race_weight_perception = ifelse(race == "Non-Hispanic Black"
                                             , "Non-Hispanic Black"
                                             , race_weight_perception)) %>%
      mutate(race_weight_perception = factor(race_weight_perception) %>%
               relevel(.
                       , ref = "Non-Hispanic Black"))
    # print(dim(df_svy_nhanes_dif_size))
    
    num_cycles <- df_svy_nhanes_dif_size$SDDSRVYR %>%
      unique(.) %>%
      length(.)
    # print(num_cycles)

    df_svy_nhanes_dif_size <- df_svy_nhanes_dif_size %>%
      mutate(adjusted_weights = WT_URXBP3/num_cycles)

    nhanes_design_dif_size <- svydesign(ids = ~SDMVPSU
                                         , strata = ~SDMVSTRA
                                         , weights = ~adjusted_weights
                                         , nest = TRUE
                                         , data = df_svy_nhanes_dif_size)

    svy_model_dif_sample_size <- svyglm(as.formula(regression_model_i)
                                         , design = nhanes_design_dif_size)

    df_tidy_svy_dif_sample_size_i <- svy_model_dif_sample_size %>%
      tidy(.) %>%
      mutate(percent_diff = (10^estimate-1)*100) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "weighted") %>%
      mutate(type_sample_size = "max sample size")
    # print(df_tidy_svy_same_sample_size_i)

    df_glance_svy_dif_sample_size_i <- svy_model_dif_sample_size %>%
      glance(.) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "weighted") %>%
      mutate(type_sample_size = "max sample size")
    # print(df_glance_svy_same_sample_size_i)

    pattern_svy_dif_sample_size_i <- paste("weighted_dif_sample_size"
                                        , i
                                        , sep = "_")

    list_tidy[[pattern_svy_dif_sample_size_i]] <- df_tidy_svy_dif_sample_size_i

    list_glance[[pattern_svy_dif_sample_size_i]] <- df_glance_svy_dif_sample_size_i
  }
  
  df_tidy <- reduce(list_tidy
                    , full_join
                    , by = NULL) %>%
    mutate(perc_diff_ci_low = percent_diff - 1.96*((10^std.error-1)*100)) %>%
    mutate(perc_diff_ci_high = percent_diff + 1.96*((10^std.error-1)*100)) %>%
    mutate(asterisks = case_when(p.value > 0.05 ~ ""
                                 , p.value > 0.01 & p.value <= 0.05 ~ "*"
                                 , p.value > 0.001 & p.value <= 0.01 ~ "**"
                                 , p.value <= 0.001 ~ "***"))
  # View(df_tidy)

  df_glance <- reduce(list_glance
                      , full_join
                      , by = NULL)
  # View(df_glance)

  df_regression <- full_join(df_tidy
                             , df_glance
                             , by = c("regression_formula"
                                      , "account_sampling_design"
                                      , "type_sample_size")) %>%
    mutate(covariates = gsub("log10\\(URXBP3\\) \\~ race_weight_perception \\+ |log10\\(URXBP3\\) \\~ race (\\+ |\\+ weight_perception \\+ )"
                             , ""
                             , regression_formula))
  View(df_regression)

  return(df_regression)
}
