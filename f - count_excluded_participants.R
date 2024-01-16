count_excluded_participants <- function(list_dataset
                                        , vector_chemical_codenames)
{
  library(tidyverse)
  
  # print(names(list_dataset))
  
  df_merged1 <- list_dataset[["self_reported_weight"]] %>%
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
                                               , sunscreen_usage_cat == "_never" ~ 1))
  print("Number of people with survey information. This is where we start")
  number_with_survey = nrow(df_merged1)
  print(number_with_survey)
  
  df_merged2 <- df_merged1 %>% filter(WHQ030 %in% seq(3))
  print("Number of people with who did not answer how they feel about weight")
  num_no_weight_feeling = nrow(df_merged1)-nrow(df_merged2)
  print(num_no_weight_feeling)
  print("Number of people with survey and weight info")
  print(nrow(df_merged2))
  df_merged3 <- df_merged2 %>%
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
  
  print("The number of people who feel underweight")
  number_underweight = nrow(df_merged2) - nrow(df_merged3)
  print(number_underweight)
  print("Total number of people after removing those who feel underweight")
  print(nrow(df_merged3))
  
  # exclude male participants
  df_merged4 <- df_merged3 %>% filter(RIAGENDR == 2)
  print("The number of male participants we remove")
  number_male = nrow(df_merged3) - nrow(df_merged4)
  print(number_male)
  print("Total number of subjects after removing males")
  print(nrow(df_merged4))
  
  df_merge5 <- df_merged4 %>% filter(!is.na(URXBP3))
  
  print("Total number of subjects who do not have BP3 measurements")
  num_no_BP3 = nrow(df_merged4) - nrow(df_merge5)
  print(num_no_BP3)
  print("Total number of subjects after removeing those that do not have BP3 measurements")
  print(nrow(df_merge5))
  
  df_merge6 <- df_merge5 %>% filter(!is.na(sunscreen_usage_cat))
  print("Total number of subjects who do not have sunscreen measurements")
  num_no_sunscreen = nrow(df_merge5) - nrow(df_merge6)
  print(num_no_sunscreen)
  print("Total number of subjects after removeing those that do not have sunscreen measurements")
  print(nrow(df_merge6))
  
  # remove people that do not have poverty income ratio, BMI, creatine measurements
  
  df_merge7 <- df_merge6 %>% filter(!is.na(INDFMPIR)) 
  print("The number of subjects that do not have PIR.")
  num_no_PIR = nrow(df_merge6) - nrow(df_merge7)
  print(num_no_PIR)
  print("Total number of subjects after removeing those that do not have PIR measurements")
  print(nrow(df_merge7))
  
  df_merge8 = df_merge7 %>% filter(!is.na(BMXBMI)) 
  print("The number of subjects that do not have BMI measurements")
  num_no_BMI = nrow(df_merge7) - nrow(df_merge8)
  print(num_no_BMI)
  print("Total number of subjects after removeing those that do not have BMI measurements")
  print(nrow(df_merge8))
  
  df_merge9 <- df_merge8 %>% filter(!is.na(URXUCR))
  print("Total number of subjects who don't have  urine creatine measurementws")
  num_no_creatine = nrow(df_merge8) - nrow(df_merge9)
  print(num_no_creatine)
  print("Total Number of people in final dataset")
  print(nrow(df_merge9))
  
  df_merged10 <- df_merge9 %>%
    full_join(.
              , df_merge9 %>%
                mutate(race = "All NHANES Women")
              , by = NULL) %>%
    mutate(race_weight_perception = paste("_"
                                          , race
                                          , weight_perception
                                          , sep = "")) 
  print("Final Number after adding ALL NHANES Women category")
  print(nrow(df_merged10))
  
  all_row_counts = c(number_with_survey,num_no_weight_feeling,number_underweight,number_male,num_no_BP3,num_no_sunscreen,num_no_PIR,num_no_BMI,num_no_creatine)
  
  names(all_row_counts) = c("number_with_survey","no_weigth_feeling","number_underweight","number_male","number_no_BP3","number_no_sunscreen","number_no_PIR","number_no_BMI","number_no_urine_creatine")
  return(all_row_counts)
}
