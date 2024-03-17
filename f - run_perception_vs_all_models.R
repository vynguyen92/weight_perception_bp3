run_perception_vs_all_models <- function(df_nhanes 
                                         , covariates 
                                         , chemical
                                         , regression_formulas
                                         , race_group)
{
  library(broom)
  library(survey)
  library(tidyverse)
  
  df_race_ridreth1 <- df_nhanes[,c("RIDRETH1", "race")] %>%
    unique(.) %>%
    filter(race != "All NHANES Women") 
  # View(df_race_ridreth1)
  
  race_encoding <- df_race_ridreth1 %>%
    filter(race == race_group) %>%
    pull(RIDRETH1)
  print(race_encoding)
  
  subset_nhanes_same_size <- df_nhanes %>%
    filter(RIDRETH1 == race_encoding) %>%
    .[,c("RIDRETH1"
         , chemical
         , covariates)] %>%
    na.omit(.) %>%
    mutate(weight_perception = as.character(weight_perception)) %>%
    mutate(weight_perception = ifelse(race == "All NHANES Women"
                                      , "all"
                                      , weight_perception)) %>%
    mutate(weight_perception = factor(weight_perception) %>%
             relevel(.
                     , ref = "all"))
  # print(str(subset_nhanes_same_size$weight_perception))
  # View(subset_nhanes_same_size)
  
  num_regression_models <- length(regression_formulas)
  
  list_tidy <- list()
  list_glance <- list()
  
  for(i in seq(num_regression_models))
  {
    regression_model_i <- regression_formulas[i]
    print(regression_model_i)
  
    lm_model_same_sample_size <- lm(as.formula(regression_model_i)
                                    , data = subset_nhanes_same_size)
    # print(summary(lm_model_same_sample_size))
    
    df_tidy_same_sample_size_i <- lm_model_same_sample_size %>%
      tidy(.) %>%
      mutate(fold_diff = 10^estimate) %>%
      mutate(percent_diff = (fold_diff-1)*100) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "unweighted") %>%
      mutate(type_sample_size = "same across models")
    # print(df_tidy_same_sample_size_i)
    
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
      filter(RIDRETH1 == race_encoding) %>%
      .[,c("RIDRETH1"
           , chemical
           , covariates
           , "SDMVPSU"
           , "SDMVSTRA"
           , "WT_URXBP3")] %>%
      na.omit(.) %>%
      mutate(weight_perception = as.character(weight_perception)) %>%
      mutate(weight_perception = ifelse(race == "All NHANES Women"
                                        , "all"
                                        , weight_perception)) %>%
      mutate(weight_perception = factor(weight_perception) %>%
               relevel(.
                       , ref = "all"))
    # View(df_nhanes_same_size_svy)
    
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
      mutate(fold_diff = 10^estimate) %>%
      mutate(percent_diff = (fold_diff-1)*100) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "weighted") %>%
      mutate(type_sample_size = "same across models") 
    # print(df_tidy_svy_same_sample_size_i)
    
    df_glance_svy_same_sample_size_i <- svy_model_same_sample_size %>%
      glance(.) %>%
      mutate(regression_formula = regression_model_i)  %>%
      mutate(account_sampling_design = "weighted") %>%
      mutate(type_sample_size = "same across models") %>%
      mutate(r.squared = calculate_rsq_svy(model_object = svy_model_same_sample_size
                                           , df_nhanes = df_nhanes_same_size_svy
                                           , svy_design = nhanes_design_same_size
                                           , stats = "rsq")
             , adj.r.squared = calculate_rsq_svy(model_object = svy_model_same_sample_size
                                                 , df_nhanes = df_nhanes_same_size_svy
                                                 , svy_design = nhanes_design_same_size
                                                 , stats = "adjusted_rsq"))
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
      filter(RIDRETH1 == race_encoding) %>%
      .[,c("URXBP3"
             , "RIDRETH1"
             , specific_covariates)] %>%
      na.omit(.)  %>%
      mutate(weight_perception = as.character(weight_perception)) %>%
      mutate(weight_perception = ifelse(race == "All NHANES Women"
                                        , "all"
                                        , weight_perception)) %>%
      mutate(weight_perception = factor(weight_perception) %>%
               relevel(.
                       , ref = "all"))
    # View(df_nhanes_dif_size)
     
    lm_model_max_sample_size <- lm(as.formula(regression_model_i)
                                   , data = df_nhanes_dif_size)
    
    df_tidy_max_sample_size_i <- lm_model_max_sample_size %>%
      tidy(.)  %>%
      mutate(fold_diff = 10^estimate) %>%
      mutate(percent_diff = (fold_diff-1)*100) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "unweighted") %>%
      mutate(type_sample_size = "max sample size")
    # print(df_tidy_max_sample_size_i)
    
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
      filter(RIDRETH1 == race_encoding) %>%
      .[,c("URXBP3"
           , "RIDRETH1"
           , "SDMVPSU"                
           , "SDMVSTRA"
           , "WT_URXBP3"
           , specific_covariates)] %>%
      na.omit(.)  %>%
      mutate(weight_perception = as.character(weight_perception)) %>%
      mutate(weight_perception = ifelse(race == "All NHANES Women"
                                        , "all"
                                        , weight_perception)) %>%
      mutate(weight_perception = factor(weight_perception) %>%
               relevel(.
                       , ref = "all"))
    # View(df_svy_nhanes_dif_size)
    
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
      mutate(fold_diff = 10^estimate) %>%
      mutate(percent_diff = (fold_diff-1)*100) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "weighted") %>%
      mutate(type_sample_size = "max sample size")
    # print(df_tidy_svy_same_sample_size_i)
    
    df_glance_svy_dif_sample_size_i <- svy_model_dif_sample_size %>%
      glance(.) %>%
      mutate(regression_formula = regression_model_i) %>%
      mutate(account_sampling_design = "weighted") %>%
      mutate(type_sample_size = "max sample size") %>%
      mutate(r.squared = calculate_rsq_svy(model_object = svy_model_dif_sample_size
                                           , df_nhanes = df_svy_nhanes_dif_size
                                           , svy_design = nhanes_design_dif_size
                                           , stats = "rsq")
             , adj.r.squared = calculate_rsq_svy(model_object = svy_model_dif_sample_size
                                                 , df_nhanes = df_svy_nhanes_dif_size
                                                 , svy_design = nhanes_design_dif_size
                                                 , stats = "adjusted_rsq"))
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
    mutate(fold_diff_ci_low = 10^(estimate - 1.96*std.error)) %>%
    mutate(fold_diff_ci_high = 10^(estimate + 1.96*std.error)) %>%
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
  
  list_regression <- list()
  
  list_regression[["tidy"]] <- df_tidy %>%
    mutate(covariates = gsub("log10\\(URXBP3\\) \\~ weight_perception \\+ |log10\\(URXBP3\\) \\~ race (\\+ |\\+ weight_perception \\+ )"
                             , ""
                             , regression_formula))
  
  write.xlsx(x = list_regression[["tidy"]]
             , file = "regressions_non_hispanic_black_women.xlsx"
             , sheetName = "tidy")
  
  list_regression[["glance"]] <- df_glance %>%
    mutate(covariates = gsub("log10\\(URXBP3\\) \\~ weight_perception \\+ |log10\\(URXBP3\\) \\~ race (\\+ |\\+ weight_perception \\+ )"
                             , ""
                             , regression_formula))
  
  write.xlsx(x = list_regression[["glance"]]
             , file = "regressions_non_hispanic_black_women.xlsx"
             , sheetName = "glance"
             , append = TRUE)
  
  return(list_regression)
  
}