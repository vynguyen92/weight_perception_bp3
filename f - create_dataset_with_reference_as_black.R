create_dataset_with_reference_as_black <- function(df_nhanes)
{
  df_nhanes_black_women <- df_nhanes %>%
    filter(race == "Non-Hispanic Black")
  
  races <- df_nhanes %>%
    filter(race != "Non-Hispanic Black") %>%
    pull(race) %>%
    unique() %>%
    as.character(.)
  # print(races)
  
  num_races <- length(races)
  
  list_nhanes_race <- list()
  
  for(i in seq(num_races))
  {
    race_i <- races[i]
    # print(race_i)
    
    df_nhanes_race_i <- df_nhanes_black_women %>%
      mutate(weight_perception = "Reference") %>%
      mutate(race = race_i)
    
    list_nhanes_race[[race_i]] <- df_nhanes_race_i
  }
  
  df_nhanes_references <- list_nhanes_race %>%
    reduce(.
           , full_join
           , by = NULL)
  
  # print(df_nhanes_references %>% 
  #         select(weight_perception, race) %>% 
  #         unique(.))
  
  df_nhanes_updated <- full_join(df_nhanes
                                 , df_nhanes_references
                                 , by = NULL)
    
  return(df_nhanes_updated)
}