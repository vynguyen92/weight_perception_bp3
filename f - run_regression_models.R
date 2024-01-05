run_regression_models <- function(df_nhanes 
                                  , covariates 
                                  , chemical
                                  , regression_formulas)
{
  library(broom)
  
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
      mutate(regression_formula = regression_model_i)
    
    pattern_same_sample_size_i <- paste("unweighted_same_sample_size"
                                        , i
                                        , sep = "_")
    
    list_tidy[[pattern_same_sample_size_i]] <- df_tidy_same_sample_size_i 
    
    list_glance[[pattern_same_sample_size_i]] <- df_glance_same_sample_size_i
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Weighted Models with Same Sample Size  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
    
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
      mutate(regression_formula = regression_model_i)
    
    pattern_max_sample_size_i <- paste("unweighted_max_sample_size"
                                       , i
                                       , sep = "_")
    
    list_tidy[[pattern_max_sample_size_i]] <- df_tidy_max_sample_size_i 
    
    list_glance[[pattern_max_sample_size_i]] <- df_glance_max_sample_size_i
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Weighted Models with Max Sample Size  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    
  }
  
  df_tidy <- reduce(list_tidy
                    , full_join
                    , by = NULL)
  # View(df_tidy)

  df_glance <- reduce(list_glance
                      , full_join
                      , by = NULL)
  # View(df_glance)

  df_regression <- full_join(df_tidy
                             , df_glance
                             , by = "regression_formula")
  # View(df_regression)
  
  return(df_regression)
}
