create_table_population_statistics <- function(df_nhanes
                                               , continuous_variables 
                                               , categorical_variables 
                                               , boolean_adult = TRUE)
{
  # install.packages("gtsummary")
  library(gtsummary)
  library(gt)
  
  df_nhanes <- df_nhanes %>%
    select(race
           , continuous_variables
           , "SDDSRVYR"
           , categorical_variables) %>%
    na.omit(.)
  
  print(df_nhanes$SDDSRVYR %>% unique(.))
  
  df_stats <- df_nhanes %>%
    select(race
           , all_of(continuous_variables)
           , all_of(categorical_variables)) %>%
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
                                    , weight_perception))
  
  if(boolean_adult == TRUE)
  {
    df_stats <- df_stats %>%
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
    df_stats <- df_stats  %>%
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
  
  # continuous_summary <- df_nhanes %>%
  #   group_by(race) %>%
  #   select(all_of(continuous_variables)) %>%
  #   summarise_all(list(mean = ~mean(., na.rm = TRUE)
  #                      , sd = ~sd(., na.rm = TRUE)
  #                      )) %>%
  #   ungroup(.) %>%
  #   pivot_longer(!race
  #                , names_to = "variable_stat"
  #                , values_to = "values") %>%
  #   separate(col = "variable_stat"
  #            , into = c("variable"
  #                       , "stat")
  #            , sep = "_") %>%
  #   pivot_wider(names_from = "stat"
  #               , values_from = "values") %>%
  #   mutate(mean_sd = paste(mean %>%
  #                            round(.
  #                                  , digits = 1)
  #                          , " ("
  #                          , sd %>%
  #                            round(.
  #                                  , digits = 1)
  #                          , ")"
  #                          , sep = "")) %>%
  #   select(-mean
  #          , -sd) %>%
  #   pivot_wider(names_from = "race"
  #               , values_from = "mean_sd")
  # # View(continuous_summary)
  # 
  # total_num_women <- df_nhanes %>%
  #   filter(race == "All NHANES Women") %>%
  #   nrow(.)
  # 
  # # Summarize categorical variables by race
  # categorical_summary_weight_perception <- df_nhanes %>%
  #   group_by(race, weight_perception) %>%
  #   summarise(count = n())  %>%
  #   group_by(race) %>%
  #   mutate(total = sum(count)) %>%
  #   mutate(percent = count / total * 100) %>%
  #   ungroup(.) %>%
  #   mutate(percent_count = paste(count
  #                                , " ("
  #                                , percent %>%
  #                                  round(.
  #                                        , digits = 1)
  #                                , "%)"
  #                                , sep = "")) %>%
  #   select(-percent
  #          , -count
  #          , -total) %>%
  #   pivot_wider(names_from = race
  #               , values_from = percent_count) %>%
  #   rename(variable = "weight_perception") %>%
  #   mutate(variable = paste("weight_perception"
  #                           , variable
  #                           , sep = ""))
  # # View(categorical_summary_weight_perception)
  # 
  # categorical_summary_sunscreen_usage_cat <- df_nhanes %>%
  #   mutate(sunscreen_usage_cat = factor(sunscreen_usage_cat
  #                                       , levels = c("_always"
  #                                                    , "_most of the time"
  #                                                    , "_sometimes"
  #                                                    , "_rarely"
  #                                                    , "_never"))) %>%
  #   group_by(race, sunscreen_usage_cat) %>%
  #   summarise(count = n())  %>%
  #   group_by(race) %>%
  #   mutate(total = sum(count)) %>%
  #   mutate(percent = count / total * 100) %>%
  #   ungroup(.) %>%
  #   mutate(percent_count = paste(count
  #                                , " ("
  #                                , percent %>%
  #                                  round(.
  #                                        , digits = 1)
  #                                , "%)"
  #                                , sep = "")) %>%
  #   select(-percent
  #          , -count
  #          , -total) %>%
  #   pivot_wider(names_from = race
  #               , values_from = percent_count) %>%
  #   rename(variable = "sunscreen_usage_cat") %>%
  #   mutate(variable = paste("sunscreen_usage"
  #                           , variable
  #                           , sep = ""))
  # # View(categorical_summary_sunscreen_usage_cat)
  # 
  # df_stats_hard <- reduce(list(categorical_summary_weight_perception
  #                         , categorical_summary_sunscreen_usage_cat
  #                         , continuous_summary)
  #                    , full_join
  #                    , by = NULL)
  # View(df_stats_hard)
  
  return(df_stats)
}