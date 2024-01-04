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
    filter(race != "All NHANES Women")
  # print(dim(df_nhanes))
  
  num_regression_models <- length(regression_formulas)
  
  for(i in seq(num_regression_models))
  {
    regression_model_i <- regression_formulas[i]
    
    glm_model <- glm(as.formula(regression_model_i)
                     , data = df_nhanes)
    
    print(glm_model %>%
            tidy(.))
  }
}
