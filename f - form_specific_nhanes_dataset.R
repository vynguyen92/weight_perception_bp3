form_specific_nhanes_dataset <- function(pattern)
{
  library(nhanesA)
  library(tidyverse)
  
  pattern_updated <- paste("^"
                   , pattern
                   , "_"
                   , sep = "") 
  
  vector_file_names <- nhanesSearchTableNames(pattern_updated) 
  
  if(pattern %in% vector_file_names)
  {
    vector_file_names <- vector_file_names %>%
      append(.
             , pattern) %>%
      sort(.)
    
  } else {
    vector_file_names <- vector_file_names
  }
  print(vector_file_names)
  
  list_datasets <- lapply(vector_file_names
                          , nhanes)

  df_nhanes_specific <- reduce(list_datasets
                               , full_join
                               , by = NULL)

  return(df_nhanes_specific)
}  
