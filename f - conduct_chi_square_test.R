conduct_chi_square_test <- function(df_nhanes
                                    , variable_1
                                    , variable_2)
{
  
  contingency_table <- table(df_nhanes[,variable_1]
                             , df_nhanes[,variable_2])
  
  chi_square_test <- chisq.test(df_nhanes[,variable_1]
                                , df_nhanes[,variable_2])
  
  list_results <- list()
  
  list_results[["contingency_table"]] <- contingency_table
  
  list_results[["chi_square_test"]] <- chi_square_test
  
  return(list_results)
}