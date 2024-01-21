merge_adult_youth_regression_df <- function(list_adults
                                            , list_youth)
{
  df_regression_adults <- list_adults[["all"]][["tidy"]] %>%
    filter(regression_formula != "log10(URXBP3) ~ race + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI") %>%
    filter(grepl("race", term) == TRUE) %>%
    full_join(.
              , list_adults[["ref"]][["tidy"]] %>%
                filter(grepl("weight_perception_", term) == TRUE)
              , by = NULL) %>%
    mutate(race_weight_perception = gsub("race_weight_perception_|weight_perception_"
                                         , ""
                                         , term)) %>%
    mutate(race_weight_perception = gsub("^race"
                                         , ""
                                         , race_weight_perception)) %>%
    mutate(race_weight_perception = case_when(race_weight_perception == "about the right weight" ~ "Non-Hispanic Black_about the right weight"
                                              , race_weight_perception == "overweight" ~ "Non-Hispanic Black_overweight"
                                              , .default = as.character(race_weight_perception))) %>%
    relocate(race_weight_perception
             , .after = term) %>%
    separate(race_weight_perception
             , into = c("race"
                        , "weight_perception")
             , sep = "_") %>%
    mutate(weight_perception = ifelse(is.na(weight_perception) == TRUE
                                      , "all"
                                      , weight_perception)) %>%
    mutate(age_group = "Adults")
  # View(df_regression_adults)
  
  df_regression_youth <-  list_youth[["all"]][["tidy"]] %>%
    filter(regression_formula != "log10(URXBP3) ~ race + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI") %>%
    filter(grepl("race", term) == TRUE) %>%
    full_join(.
              , list_youth[["ref"]][["tidy"]] %>%
                filter(grepl("weight_perception_", term) == TRUE)
              , by = NULL) %>%
    mutate(race_weight_perception = gsub("race_weight_perception_|weight_perception_"
                                         , ""
                                         , term)) %>%
    mutate(race_weight_perception = gsub("^race"
                                         , ""
                                         , race_weight_perception)) %>%
    mutate(race_weight_perception = case_when(race_weight_perception == "about the right weight" ~ "Non-Hispanic Black_about the right weight"
                                              , race_weight_perception == "overweight" ~ "Non-Hispanic Black_overweight"
                                              , .default = as.character(race_weight_perception))) %>%
    relocate(race_weight_perception
             , .after = term) %>%
    separate(race_weight_perception
             , into = c("race"
                        , "weight_perception")
             , sep = "_") %>%
    mutate(weight_perception = ifelse(is.na(weight_perception) == TRUE
                                      , "all"
                                      , weight_perception)) %>%
    mutate(age_group = "Youths")
  # View(df_regression_youth)
  
  df_regression_all <- full_join(df_regression_adults
                                 , df_regression_youth
                                 , by = NULL) %>%
    filter(grepl("sunscreen_usage_ordinal", covariates) == FALSE)
  
  # print(df_regression_all %>%
  #         select(age_group
  #                , covariates) %>%
  #         unique(.))
  
  return(df_regression_all)
}