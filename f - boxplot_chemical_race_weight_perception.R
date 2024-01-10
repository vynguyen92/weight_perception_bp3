boxplot_chemical_race_weight_perception <- function(df_nhanes
                                                    , covariates 
                                                    , chemical 
                                                    , name_of_folder
                                                    , current_directory)
{
  library(tidyverse)
  
  # Determine all file names in the current working directory
  all_files_in_current_directory <- list.files()
  
  # Make a new folder if the folder doesn't exist
  if(name_of_folder %in% all_files_in_current_directory)
  {
    
  } else {
    dir.create(name_of_folder)
  }
  
  # Define a string for the working directory for the new folder
  new_working_directory <- paste(current_directory
                                 , name_of_folder
                                 , sep = "/")
  
  # Set the working directory to this new folder
  setwd(new_working_directory)
  
  df_nhanes_same_size <- df_nhanes %>%
    select(race
           , chemical
           , "SDMVPSU"
           , "SDMVSTRA"
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
  
  print(dim(df_nhanes_same_size))
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
}