run_univariate_ordinal_models <- function(df_nhanes
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
}