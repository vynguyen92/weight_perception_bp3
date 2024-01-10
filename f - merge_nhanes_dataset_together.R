merge_nhanes_dataset_together <- function(list_dataset
                                          , vector_chemical_codenames)
{
  library(tidyverse)
  
  # print(names(list_dataset))
  
  df_merged <- list_dataset[["self_reported_weight"]] %>%
    select(SEQN
           , WHD010
           , WHD020
           , WHQ030
           , WHQ040) %>%
    mutate(weight_perception = case_when(WHQ030 == 1 ~ "_overweight"
                                         , WHQ030 == 2 ~ "_underweight"
                                         , WHQ030 == 3 ~ "_about the right weight") ) %>%
    full_join(.
              , list_dataset[["chemicals"]] %>%
                filter(SDDSRVYR != -1) %>%
                select(SEQN
                       , all_of(vector_chemical_codenames)) 
              , by = "SEQN") %>%
    full_join(.
              , list_dataset[["demographics"]] %>%
                filter(SDDSRVYR != -1) %>%
                select(SEQN
                       , RIAGENDR
                       , RIDRETH1
                       , RIDRETH3
                       , RIDAGEYR
                       , INDFMPIR
                       , SDDSRVYR
                       , SDMVPSU
                       , SDMVSTRA)
              , by = "SEQN") %>%
    full_join(.
              , list_dataset[["response"]] %>%
                filter(SDDSRVYR != -1) %>%
                select(SEQN
                       , URXUCR
                       , BMXBMI)
              , by = "SEQN") %>%
    full_join(.
              , list_dataset[["dermatology"]] %>%
                select("DEQ034D"
                       , "DED031"
                       , "SEQN") 
              , by = "SEQN") %>%
    full_join(.
              , list_dataset[["weights"]] %>%
                select("SEQN"
                       , "WT_URXBP3"
                       , "WTINT2YR"
                       , "WTMEC2YR")
              , by = "SEQN") %>%
    mutate(skin_tone_cat = case_when(DED031 == 1 ~ "severe sunburn with blisters"
                                     , DED031 == 2 ~ "severe sunburn for a few days with peeling"
                                     , DED031 == 3 ~ "mildly burned with some tanning"
                                     , DED031 == 4 ~ "turned darker without a sunburn"
                                     , DED031 == 5 ~ "nothing would happen in half an hour")) %>%
    mutate(sunscreen_usage_cat = case_when(DEQ034D == 1 ~ "_always"
                                           , DEQ034D == 2 ~ "_most of the time"
                                           , DEQ034D == 3 ~ "_sometimes"
                                           , DEQ034D == 4 ~ "_rarely"
                                           , DEQ034D == 5 ~ "_never") %>%
             factor(.
                    , levels = c("_always"
                                 , "_most of the time"
                                 , "_sometimes"
                                 , "_rarely"
                                 , "_never"))) %>%
    mutate(sunscreen_usage_ordinal = case_when(sunscreen_usage_cat == "_always" ~ 5
                                               , sunscreen_usage_cat == "_most of the time" ~ 4
                                               , sunscreen_usage_cat == "_sometimes" ~ 3
                                               , sunscreen_usage_cat == "_rarely" ~ 2
                                               , sunscreen_usage_cat == "_never" ~ 1)) %>%
    filter(WHQ030 %in% seq(3)) %>%
    filter(RIAGENDR == 2) %>%
    mutate(race = case_when(RIDRETH1 == 1 ~ "Mexican American"
                            , RIDRETH1 == 2 ~ "Other Hispanic"
                            , RIDRETH1 == 3 ~ "Non-Hispanic White"
                            , RIDRETH1 == 4 ~ "Non-Hispanic Black"
                            , RIDRETH1 == 5 & is.na(RIDRETH3) == TRUE ~ "Other Race - Including Multi-Racial"
                            , RIDRETH1 == 5 & RIDRETH3 == 7 ~ "Other Race - Including Multi-Racial"
                            , RIDRETH1 == 5 & RIDRETH3 == 6 ~ "Non-Hispanic Asian")) %>%
    mutate(race_weight_perception = paste(race
                                          , weight_perception
                                          , sep = "")) %>%
    filter(weight_perception %in% c("_overweight"
                                    , "_about the right weight")) %>%
    mutate(weight_perception = weight_perception %>%
             factor(.) %>%
             relevel(.
                     , ref = "_about the right weight")) 
  
  
  df_merged <- df_merged%>%
    full_join(.
              , df_merged %>%
                mutate(race = "All NHANES Women")
              , by = NULL) %>%
    mutate(race_weight_perception = paste("_"
                                          , race
                                          , weight_perception
                                          , sep = "")) 
  
  print(dim(df_merged))
  print(colnames(df_merged))
  
  return(df_merged)
}