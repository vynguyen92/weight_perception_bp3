form_table_with_and_without_weight_perception <- function(list_all
                                                          , list_stratified)
{
  library(readxl)
  
  df_regressions_equation <- read_excel(path = "/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3/mapping_regression_formulas_and_equation_numbers.xlsx")
  # View(df_regressions_equation)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~ Contribution of Body Dissatisfaction on BP3 Levels for all NHANES Women ~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  df_glance_all <- list_all[["glance"]] %>%
    filter(type_sample_size != "max sample size") %>%
    # mutate(covariates = gsub(" \\+ weight_perception| \\+ race_weight_perception"
    #                          , ""
    #                          , covariates) %>%
    #          gsub("log10\\(URXBP3\\) \\~ "
    #               , ""
    #               , .)) %>%
    mutate(covariates =  gsub("log10\\(URXBP3\\) \\~ "
                              , ""
                              , covariates)) %>%
    mutate(covariates = ifelse(grepl("log10\\(URXBP3\\) \\~ race \\+ ", regression_formula) == TRUE
                               , paste0("race + "
                                       , covariates)
                               , covariates)) %>%
    left_join(.
              , df_regressions_equation %>%
                filter(group == "All NHANES Women")
              , by = "regression_formula")
  # View(df_glance_all)
  # print(colnames(df_glance_all))
  
  subset_glance_wide <- df_glance_all %>%
    select(equation_number
           , account_sampling_design
           , regression_formula
           , r.squared) %>%
    mutate(include_weight_perception = ifelse(grepl("weight_perception", regression_formula) == TRUE
                                              , "with_weight_perception"
                                              , "without_weight_perception")) %>%
    select(-regression_formula) %>%
    pivot_wider(names_from = include_weight_perception
                , values_from = r.squared)
  # View(subset_glance_wide)
  
  df_stats_all <- df_glance_all %>%
    group_by(equation_number
             , account_sampling_design) %>%
    summarise(contribution = diff(r.squared)) %>%
    ungroup(.) %>%
    mutate(group = "All NHANES women") %>%
    left_join(.
              , subset_glance_wide
              , by = c("equation_number"
                       , "account_sampling_design")) %>%
    relocate(contribution
             , .after = with_weight_perception) %>%
    relocate(group)
  # View(df_stats_all)
  
  df_mapping_model_nums_to_equation_numbers <- data.frame(model_number = c(1:8)
                                                          , equation_number = c(1,1,4,4,2,2,5,5))
  # View(df_mapping_model_nums_to_equation_numbers)
  
  df_all_combinations <- df_stats_all %>%
    select(equation_number
           , account_sampling_design) %>%
    unique(.)
  # View(df_all_combinations)
  
  # print(names(list_all$model_objects))
  
  num_combinations_all <- nrow(df_all_combinations)
  
  for(i in seq(num_combinations_all))
  {
    combination_i <- df_all_combinations[i,]
    # print(combination_i)
    
    equation_number_i <- combination_i %>%
      pull(equation_number)
    # print(equation_number_i)
    
    account_sampling_design_i <- combination_i %>%
      pull(account_sampling_design)
    # print(account_sampling_design_i)
    
    model_numbers_i <- df_mapping_model_nums_to_equation_numbers %>%
      filter(equation_number == equation_number_i) %>%
      pull(model_number)
    # print(model_numbers_i)
    
    names_model_objects_i <- paste0(account_sampling_design_i
                                    , "_"
                                    , "same_sample_size"
                                    , "_"
                                    , model_numbers_i)
    # print(names_model_objects_i)
    
    f_test_i <- anova(list_all$model_objects[[names_model_objects_i[1]]]
                      , list_all$model_objects[[names_model_objects_i[2]]])
    print(f_test_i)
    # print(str(f_test_i))
    
    p_value_i <- ifelse(account_sampling_design_i == "unweighted"
                        , f_test_i$`Pr(>F)` %>%
                          .[!is.na(.)]
                        , f_test_i$p)
    
    df_f_test_stats_i <- data.frame("equation_number" = equation_number_i
                                    , "account_sampling_design" = account_sampling_design_i
                                    , "p_value" = p_value_i)
    # print(df_f_test_stats_i)
    
    if(i == 1)
    {
      df_f_test_stats_all <- df_f_test_stats_i
      
    } else {
      
      df_f_test_stats_all <- df_f_test_stats_all %>%
        full_join(.
                  , df_f_test_stats_i
                  , by = colnames(.))
    }
  }
  # View(df_f_test_stats_all)
  
  df_stats_all <- df_stats_all %>%
    full_join(.
              , df_f_test_stats_all
              , by = c("equation_number"
                       , "account_sampling_design"))
  # print("problem")
  # View(df_stats_all)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~ Contribution of Body Dissatisfaction on BP3 Levels by Race/ethnicity ~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  df_glance_race <- list_stratified[["glance"]] %>%
    left_join(.
              , df_regressions_equation %>%
                filter(group == "Race/ethnicity") %>%
                select(-group)
              , by = "regression_formula")
  # View(df_glance_race)
  
  subset_glance_wide_race <- df_glance_race %>%
    select(race
           , equation_number
           , account_sampling_design
           , regression_formula
           , r.squared) %>%
    mutate(include_weight_perception = ifelse(grepl("weight_perception", regression_formula) == TRUE
                                              , "with_weight_perception"
                                              , "without_weight_perception")) %>%
    select(-regression_formula) %>%
    pivot_wider(names_from = include_weight_perception
                , values_from = r.squared) %>%
    rename(group = race)
  # View(subset_glance_wide_race)
  
  df_stats_race <- df_glance_race %>%
    group_by(race
             , equation_number
             , account_sampling_design) %>%
    summarise(contribution = diff(r.squared)) %>%
    ungroup(.) %>%
    rename(group = race)  %>%
    left_join(.
              , subset_glance_wide_race
              , by = c("group"
                       , "equation_number"
                       , "account_sampling_design")) %>%
    relocate(contribution
             , .after = with_weight_perception) %>%
    relocate(group)
  # View(df_stats_race)
  
  df_mapping_model_nums_to_equation_numbers_race <- data.frame(model_number = c(1:4)
                                                          , equation_number = c(7,7,8,8))
  
  df_all_combinations_race <- df_stats_race %>%
    select(group
           , equation_number
           , account_sampling_design) %>%
    unique(.)
  # View(df_all_combinations_race)
  
  num_combinations_race <- nrow(df_all_combinations_race)
  
  for(i in seq(num_combinations_race))
  {
    combination_i <- df_all_combinations_race[i,]
    # print(combination_i)
    
    race_i <- combination_i %>%
      pull(group)
    
    equation_number_i <- combination_i %>%
      pull(equation_number)
    # print(equation_number_i)
    
    account_sampling_design_i <- combination_i %>%
      pull(account_sampling_design)
    # print(account_sampling_design_i)
    
    model_numbers_i <- df_mapping_model_nums_to_equation_numbers_race %>%
      filter(equation_number == equation_number_i) %>%
      pull(model_number)
    
    names_model_objects_i <- paste0(account_sampling_design_i
                                    , "_"
                                    , "same_sample_size"
                                    , "_"
                                    , model_numbers_i)
    # print(names_model_objects_i)
    
    f_test_i <- anova(list_stratified$model_objects[[names_model_objects_i[1]]][[race_i]]
                      , list_stratified$model_objects[[names_model_objects_i[2]]][[race_i]])
    # print(f_test_i)
    
    p_value_i <- ifelse(account_sampling_design_i == "unweighted"
                        , f_test_i$`Pr(>F)` %>%
                          .[!is.na(.)]
                        , f_test_i$p)
    # print(p_value_i)
    
    df_f_test_stats_race_i <- data.frame("group" = race_i
                                    , "equation_number" = equation_number_i
                                    , "account_sampling_design" = account_sampling_design_i
                                    , "p_value" = p_value_i)
    # print(df_f_test_stats_race_i)
    
    if(i == 1)
    {
      df_f_test_stats_race <- df_f_test_stats_race_i
    } else {
      df_f_test_stats_race <- df_f_test_stats_race %>%
        full_join(.
                  , df_f_test_stats_race_i
                  , by = colnames(.))
    }
  }
  # View(df_f_test_stats_race)
  
  df_stats_race <- df_stats_race %>%
    full_join(.
              , df_f_test_stats_race
              , by = c("group"
                       , "equation_number"
                       , "account_sampling_design"))
  # View(df_stats_race)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~ Contribution of Body Dissatisfaction on BP3 Levels All Together ~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # print(colnames(df_stats_all))
  # print(colnames(df_stats_race))
  
  df_stats_all_groups <- df_stats_all %>%
    full_join(.
              , df_stats_race
              , by = colnames(.))
  # View(df_stats_all_groups)
  
  write.xlsx(x = df_stats_all_groups
             , file = "contribution_weight_perception_on_bp3.xlsx")
  
  return(df_stats_all_groups)
}