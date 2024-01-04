run_regression_models <- function(df_nhanes 
                                  , covariates 
                                  , chemical
                                  , regression_formulas)
{
  library(broom)
  
  df_nhanes <- df_nhanes %>%
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
  # print(dim(df_nhanes))
  
  num_regression_models <- length(regression_formulas)
  
  list_tidy <- list()
  list_glance <- list()
  
  for(i in seq(num_regression_models))
  {
    regression_model_i <- regression_formulas[i]

    glm_model <- glm(as.formula(regression_model_i)
                     , data = df_nhanes)

    df_tidy_i <- glm_model %>%
      tidy(.) %>%
      mutate(percent_diff = (10^estimate-1)*100) %>%
      mutate(regression_formula = regression_model_i)
    
    df_glance_i <- glm_model %>%
      glance(.) %>%
      mutate(regression_formula = regression_model_i)
    
    list_tidy[[i]] <- df_tidy_i
    
    list_glance[[i]] <- df_glance_i
  }
  
  df_tidy <- reduce(list_tidy
                    , full_join
                    , by = NULL)
  View(df_tidy)
  
}
