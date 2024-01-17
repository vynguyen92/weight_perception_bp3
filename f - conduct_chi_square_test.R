conduct_chi_square_test <- function(df_nhanes
                                    , variable_1
                                    , variable_2)
{
  races <- df_nhanes$race %>% unique(.)
  
  num_groups <- length(races)
  
  list_contingency_tables <- list()
  
  list_chi_square_test <- list()
  
  for(i in seq(num_groups))
  {
    race_i <- races[i]
    
    subset_race_i <- df_nhanes %>%
      filter(race == race_i)
    
    list_contingency_tables[[race_i]] <- table(subset_race_i[,variable_1]
                                               , subset_race_i[,variable_2])

    list_chi_square_test[[race_i]] <- chisq.test(subset_race_i[,variable_1]
                                                 , subset_race_i[,variable_2]) %>%
      tidy(.) %>%
      mutate(race = race_i) %>%
      relocate(race)
    
    
  }
  
  
  list_results <- list()

  list_results[["contingency_table"]] <- list_contingency_tables

  list_results[["chi_square_test"]] <- list_chi_square_test %>%
    reduce(.
           , full_join)

  return(list_results)
}