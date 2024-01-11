calculate_rsq_svy <- function(model_object
                              , df_nhanes
                              , svy_design
                              , stats)
{
  total_var <- svyvar(log10(df_nhanes$URXBP3)
                      , svy_design) %>%
    unlist(.) %>%
    as.vector()
  # print(total_var)
  
  resid_var <- summary(model_object)$dispersion %>%
    unlist(.)  %>%
    as.vector()
  # print(resid_var)
  
  rsq <- 1 - resid_var/total_var
  # print(rsq)
  
  num_observations <- nobs(model_object)
  # print(num_observations)
  
  num_predictors <- length(model_object$coefficients) - 1
  # print(num_predictors)
  
  adjusted_rsq <- 1 - ((1-rsq)*(num_observations - 1)/(num_observations - num_predictors - 1))
  # print(adjusted_rsq)
  
  rsq_object <- ifelse(stats == "rsq"
                      , rsq
                      , adjusted_rsq)
    
  return(rsq_object)
  
}