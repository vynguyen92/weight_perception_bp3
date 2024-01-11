svyglm_stratified_model <- function(x
                                    , regression_formula
                                    , stats)
{
  
  df_race_i <- x
  
  race_i <- df_race_i$race %>% 
    unique(.)
  
  print(race_i)
  
  num_cycles <- df_race_i$SDDSRVYR %>%
    unique(.) %>%
    length(.)
  # print(num_cycles)
  
  df_race_i <- df_race_i %>%
    mutate(adjusted_weights = WT_URXBP3/num_cycles)
  # print(colnames(df_race_i))
  
  nhanes_design <- svydesign(ids = ~SDMVPSU
                             , strata = ~SDMVSTRA
                             , weights = ~adjusted_weights
                             , nest = TRUE
                             , data = df_race_i)
  
  # Remove strata with only one PSU or else error in calculating the representation
  options(survey.lonely.psu = "remove")
  
  svy_model_race_i <- svyglm(as.formula(regression_formula)
                             , design = nhanes_design)
  # print(svy_model_race_i)
  # print(summary(svy_model_race_i))
  
  
  
  
  
  if(stats == "tidy")
  {
    df_stats <- svy_model_race_i %>%
      tidy(.)
    
  } else if (stats == "glance") {
    
    df_stats <- svy_model_race_i %>%
      glance(.) %>%
      mutate(r.squared = calculate_rsq_svy(model_object = svy_model_race_i
                                           , df_nhanes = df_race_i
                                           , svy_design = nhanes_design
                                           , stats = "rsq")
             , adj.r.squared = calculate_rsq_svy(model_object = svy_model_race_i
                                                 , df_nhanes = df_race_i
                                                 , svy_design = nhanes_design
                                                 , stats = "adjusted_rsq"))
  }
  
  return(df_stats)
}