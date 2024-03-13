create_table_population_statistics <- function(df_nhanes
                                               , continuous_variables 
                                               , categorical_variables 
                                               , boolean_adult = TRUE)
{
  # install.packages("gtsummary")
  library(tidyverse)
  library(gtsummary)
  library(gt)
  
  df_nhanes <- df_nhanes %>%
    filter(SDDSRVYR != -1) %>%
    dplyr::select(race
           , continuous_variables
           , SDDSRVYR
           , SDMVPSU
           , SDMVSTRA
           , categorical_variables) %>%
    na.omit(.)
  print(dim(df_nhanes))
  
  print(range(df_nhanes$RIDAGEYR))
  # subset_stratum_psu <- df_nhanes %>%
  #   filter(race != "All NHANES Women") %>%
  #   select(SDMVPSU
  #          , SDMVSTRA) %>%
  #   unique(.) %>%
  #   group_by(SDMVSTRA) %>%
  #   summarise(num_psu = n()) %>%
  #   ungroup(.)
  # View(subset_stratum_psu)
  
  print(df_nhanes$SDDSRVYR %>% unique(.))

  continuous_variables <- continuous_variables[which(!(continuous_variables %in% c("WTINT2YR"
                                                                                   , "WT_URXBP3"
                                                                                   )))]
  
  df_nhanes <- df_nhanes %>%
    select(race
           , all_of(continuous_variables)
           , all_of(categorical_variables)) 

  if(boolean_adult == TRUE)
  {
    df_stats <- df_nhanes %>%
      mutate(race = factor(race
                           , levels = c("All NHANES Women"
                                        , "Non-Hispanic White"
                                        , "Mexican American"
                                        , "Other Hispanic"
                                        , "Non-Hispanic Black"
                                        , "Non-Hispanic Asian"
                                        , "Other Race - Including Multi-Racial"))) %>%
      mutate(weight_perception = gsub("_"
                                      , "perceived "
                                      , weight_perception)) %>%
      mutate(sunscreen_usage_cat = gsub("_"
                                        , ""
                                        , sunscreen_usage_cat) %>%
               factor(.
                      , levels = c("always"
                                   , "most of the time"
                                   , "sometimes"
                                   , "rarely"
                                   , "never"))) %>%
      tbl_summary(by = race
                  , label = list(RIDAGEYR ~ "Age (years)"
                                 , URXUCR ~ "Urinary creatinine (mg/dL)"
                                 , BMXBMI ~ "Body Mass Index (kg/m**2)"
                                 , INDFMPIR ~ "Poverty income ratio (-)"
                                 , URXBP3 ~ "Urinary Benzophenone-3 (ng/mL)"
                                 , weight_perception ~ "Weight perception"
                                 , sunscreen_usage_cat ~ "Sunscreen usage")
                  , statistic = list(all_continuous() ~ "{mean} ({sd})")
                  , digits = list(RIDAGEYR ~ c(1, 1)
                                  , URXUCR ~ c(1, 1)
                                  , BMXBMI ~ c(1, 1)
                                  , INDFMPIR ~ c(2, 2)
                                  , URXBP3 ~ c(1, 1)))

    name_table <- "table_1_adults.png"

  } else {

    df_stats <- df_nhanes %>%
      mutate(race = ifelse(race == "All NHANES Women"
                           , "All NHANES Children"
                           , race)) %>%
      mutate(race = factor(race
                           , levels = c("All NHANES Children"
                                        , "Non-Hispanic White"
                                        , "Mexican American"
                                        , "Other Hispanic"
                                        , "Non-Hispanic Black"
                                        , "Non-Hispanic Asian"
                                        , "Other Race - Including Multi-Racial"))) %>%
      mutate(weight_perception = gsub("_"
                                      , "perceived "
                                      , weight_perception)) %>%
      tbl_summary(by = race
                  , label = list(RIDAGEYR ~ "Age (years)"
                                 , URXUCR ~ "Urinary creatinine (mg/dL)"
                                 , BMXBMI ~ "Body Mass Index (kg/m**2)"
                                 , INDFMPIR ~ "Poverty income ratio (-)"
                                 , URXBP3 ~ "Urinary Benzophenone-3 (ng/mL)"
                                 , weight_perception ~ "Weight perception")
                  , statistic = list(all_continuous() ~ "{mean} ({sd})")
                  , type = list(c(RIDAGEYR) ~ "continuous")
                  , digits = list(RIDAGEYR ~ c(1, 1)
                                  , URXUCR ~ c(1, 1)
                                  , BMXBMI ~ c(1, 1)
                                  , INDFMPIR ~ c(2, 2)
                                  , URXBP3 ~ c(1, 1))

                  )

    name_table <- "table_1_youth.png"
  }
  print(df_stats)

  gtsave(data = as_gt(df_stats)
         , filename = name_table
         , expand = 10)

  return(df_stats)
}