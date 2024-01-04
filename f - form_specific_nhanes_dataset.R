form_specific_nhanes_dataset <- function(pattern)
{
  library(nhanesA)
  library(tidyverse)
  
  pattern_updated <- paste("^"
                   , pattern
                   , "_"
                   , sep = "") 
  
  vector_file_names <- nhanesSearchTableNames(pattern_updated) %>%
    append(.
           , pattern) %>%
    sort(.)
  print(vector_file_names)
  
  list_datasets <- lapply(vector_file_names
                          , nhanes)

  df_nhanes_specific <- reduce(list_datasets
                               , full_join
                               , by = NULL)

  return(df_nhanes_specific)
}  
