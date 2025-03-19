alphabet_soup_bic <- function(regression_stratified
                              , regression_all
                              , name_of_folder
                              , current_directory
                              , df_nhanes
                              , covariates
                              , chemical
                              , intercept_model_string
                              , is_adult = TRUE)
{
  library(tidyverse)
  library(xlsx)
  
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
  
  relevant_covariates <- regression_stratified[["glance"]] %>%
    pull(covariates) %>%
    unique(.)
  # print(relevant_covariates)
  
  df_coefficients <- regression_stratified[["tidy"]] %>%
    full_join(.
              , regression_all[["tidy"]] %>%
                mutate(race = "All NHANES Women")
              , by = NULL)
  # View(df_coefficients)
  
  df_degrees_of_freedom <- df_coefficients %>%
    group_by(race
             , account_sampling_design
             , regression_formula
             , type_sample_size
             , covariates) %>%
    summarize(num_coefficients = n()) %>%
    ungroup(.) %>%
    mutate(degrees_of_freedom = num_coefficients + 1) %>%
    unique(.) 
  # View(df_degrees_of_freedom)
  
  df_bic <- regression_stratified[["glance"]] %>%
    full_join(.
              , regression_all[["glance"]] %>%
                mutate(race = "All NHANES Women")
              , by = NULL) %>%
    full_join(.
              , df_degrees_of_freedom) %>%
    filter(regression_formula != "log10(URXBP3) ~ race + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI") %>%
    filter(covariates %in% relevant_covariates) %>%
    filter(type_sample_size == "same across models") %>%
    #   filter(grepl("race \\+ "
    #                , regression_formula) == FALSE) %>%
    mutate(sunscreen_included = ifelse(grepl("sunscreen_usage_ordinal", regression_formula) == TRUE
                                       , "with sunscreen usage"
                                       , "without sunscreen usage")) %>%
    mutate(baseline_covariates = gsub(" \\+ sunscreen_usage_ordinal"
                                      , ""
                                      , covariates)) %>%
    mutate(race_weight_perception_together = ifelse(grepl("race_weight_perception", regression_formula) == TRUE
                                                    , "together"
                                                    , "separate")) %>%
    mutate(BIC_min = degrees_of_freedom*log(nobs))
  # View(df_bic)

  df_stats <- df_bic %>%
    group_by(race
             , race_weight_perception_together
             , baseline_covariates
             , account_sampling_design
             , type_sample_size) %>%
    summarise(bic_sunscreen_contribution = diff(BIC)) %>%
    ungroup(.) %>%
    pivot_longer(cols = c("bic_sunscreen_contribution")
                 , names_to = "prediction_performance_type") %>%
    mutate(prediction_performance_type = gsub("_sunscreen_contribution"
                           , ""
                           , prediction_performance_type))
  # View(df_stats)

  df_combination <- df_stats  %>%
    select(account_sampling_design
           , baseline_covariates
           , type_sample_size
           , prediction_performance_type) %>%
    unique(.) %>%
    mutate(combination = paste(account_sampling_design
                               , type_sample_size
                               , prediction_performance_type
                               , baseline_covariates
                               , sep = " - "))
  # View(df_combination)

  num_combinations <- nrow(df_combination)

  list_wide_regressions <- list()

  for(i in seq(num_combinations))
  {
    subset_combination <- df_combination[i,]
    # print(subset_combination)

    combination_i <- subset_combination %>%
      pull(combination)
    print(combination_i)

    type_sampling_design_i <- subset_combination %>%
      pull(account_sampling_design)

    type_sample_size_i <- subset_combination %>%
      pull(type_sample_size)

    prediction_performance_type_i <- subset_combination %>%
      pull(prediction_performance_type)

    baseline_covariates_i <- subset_combination %>%
      pull(baseline_covariates)

    subset_stats_contribution <- df_stats %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i) %>%
      filter(prediction_performance_type == prediction_performance_type_i) %>%
      filter(baseline_covariates == baseline_covariates_i)
    # print(subset_stats_contribution)
    # print(colnames(subset_stats_contribution))

    
    prediction_performance_pattern <- toupper(prediction_performance_type_i)
    # print(prediction_performance_pattern)
    
    df_glance_intercept_model <- run_intercept_model(df_nhanes = df_nhanes
                                                     , covariates = covariates
                                                     , chemical = chemical
                                                     , intercept_model_string = intercept_model_string
                                                     , type_sampling_design = type_sampling_design_i
                                                     , prediction_performance_stat = prediction_performance_pattern)
    # View(df_glance_intercept_model)
    
    subset_stats_pred_perf <- df_bic %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i) %>%
      filter(baseline_covariates == baseline_covariates_i)

    if(type_sampling_design_i == "weighted")
    {
      subset_stats_pred_perf <- subset_stats_pred_perf  %>%
        left_join(.
                  , df_glance_intercept_model %>%
                    select(race, n)) %>%
        mutate(BIC_min = degrees_of_freedom*log(n))
    }


    # View(subset_stats_pred_perf)
    # print(colnames(subset_stats_pred_perf))

   

    df_degrees_of_freedom <- subset_stats_pred_perf %>%
      select(race
             , regression_formula
             , account_sampling_design
             , type_sample_size
             , covariates
             , race_weight_perception_together
             , nobs
             , num_coefficients
             , degrees_of_freedom
             , BIC_min) %>%
      # Choose the minimal BIC from the model with most parameters, so the fully adjusted models
      # correcting for sunscreen
      filter(grepl("sunscreen_usage_ordinal", regression_formula) == TRUE)
    # View(df_degrees_of_freedom)

    subset_sunscreen_wide <- subset_stats_pred_perf %>%
      select(race
             , sunscreen_included
             , race_weight_perception_together
             , all_of(prediction_performance_pattern)) %>%
      mutate(sunscreen_included = gsub(" "
                                       , "_"
                                       , sunscreen_included)) %>%
      pivot_wider(names_from = "sunscreen_included"
                  , values_from = prediction_performance_pattern) %>%
      full_join(.
                , subset_stats_contribution
                , by = c("race"
                         , "race_weight_perception_together")) %>%
      rename(value_explained_by_sunscreen = "value")
    # View(subset_sunscreen_wide)



    subset_sunscreen_wide <- subset_sunscreen_wide %>%
      full_join(.
                , df_glance_intercept_model) %>%
      left_join(.
                , df_degrees_of_freedom)

    if(type_sampling_design_i == "weighted")
    {
      subset_sunscreen_wide <- subset_sunscreen_wide %>%
        mutate(BIC_min = degrees_of_freedom*log(n))
    }

    subset_sunscreen_wide <- subset_sunscreen_wide %>%
      mutate(total_variation = BIC_max - BIC_min) %>%
      mutate(value_explained_by_sunscreen = abs(value_explained_by_sunscreen)) %>%
      mutate(label_explained_by_sunscreen = round(value_explained_by_sunscreen/total_variation*100, digits = 1) %>%
               paste(.
                     , "%"
                     , sep = "")) %>%
      mutate(mean_position_sunscreen = (without_sunscreen_usage + with_sunscreen_usage)/2) %>%
      mutate(value_others = abs(BIC_max - without_sunscreen_usage)) %>%
      mutate(label_others = round(abs(value_others)/total_variation*100, digits = 1) %>%
               paste(.
                     , "%"
                     , sep = "")) %>%
      mutate(mean_position_others = (BIC_max + without_sunscreen_usage)/2) %>%
      mutate(value_unexplained = abs(with_sunscreen_usage - BIC_min)) %>%
      mutate(label_unexplained = round(value_unexplained/total_variation*100, digits = 1) %>%
               paste(.
                     , "%"
                     , sep = "")) %>%
      mutate(mean_position_unexplained = (BIC_min + with_sunscreen_usage)/2) %>%
      mutate(total_percentage = (value_unexplained + value_others + value_explained_by_sunscreen)/total_variation*100) %>%
      arrange(label_unexplained)
    # View(subset_sunscreen_wide)

    ordered_race <- subset_sunscreen_wide %>%
      pull(race) %>%
      unique(.)
    print(ordered_race)

    df_positions_of_segments <- define_positions_segments(ordered_race = ordered_race)

    subset_sunscreen_wide <- subset_sunscreen_wide %>%
      mutate(race = factor(race
                           , levels = ordered_race))
    # View(subset_sunscreen_wide)

    subset_stats_pred_perf <- subset_stats_pred_perf %>%
      mutate(race = factor(race
                           , levels = ordered_race)) %>%
      mutate(covariates_included = ifelse(sunscreen_included == "without sunscreen usage"
                                          , "adjusted for age, NHANES cycle, urinary creatinine, BMI, and PIR"
                                          , "adjusted for age, NHANES cycle, urinary creatinine, BMI, PIR, and sunscreen usage")) %>%
      mutate(regression_formula_label = gsub("race"
                                             , "race/ethinicity"
                                             , regression_formula) %>%
               gsub("URXBP3"
                    , "BP3"
                    , .) %>%
               gsub("weight_perception"
                    , "body dissatisfaction"
                    , .) %>%
               gsub("RIDAGEYR"
                    , "age"
                    , .) %>%
               gsub("SDDSRVYR"
                    , "NHANES cycles"
                    , .) %>%
               gsub("URXUCR"
                    , "urinary creatinine"
                    , .) %>%
               gsub("BMXBMI"
                    , "BMI"
                    , .) %>%
               gsub("sunscreen_usage_ordinal"
                    , "sunscreen usage"
                    , .) %>%
               gsub("INDFMPIR"
                    , "PIR"
                    , .) %>%
               gsub("race/ethinicity_body dissatisfaction"
                    , "combination(race/ethinicity, body dissatisfaction)"
                    , .)) %>%
      left_join(.
                , subset_sunscreen_wide %>%
                  select(race
                         , race_weight_perception_together
                         , BIC_max
                         ))

    current_levels_regression_models <- subset_stats_pred_perf %>%
      pull(regression_formula_label) %>%
      unique(.) %>%
      sort(.) %>%
      rev(.) %>%
      .[c(2,1,4,3,6,5)]

    # View(subset_stats_pred_perf)

    subset_stats_pred_perf <- subset_stats_pred_perf %>%
      mutate(regression_formula_label = factor(regression_formula_label
                                               , levels = current_levels_regression_models)) %>%
      mutate(race_weight_perception_together = as.character(race_weight_perception_together))

    alphabet_soup_plot <- ggplot(data = subset_sunscreen_wide
                                 , mapping = aes(x = !!sym(prediction_performance_pattern)
                                                 , y = race_weight_perception_together #rep(1, nrow(subset_stats_pred_perf))
                                                 , group = race_weight_perception_together
                                 )) +
      facet_wrap(vars(race)
                 , ncol = 1
                 , scales = "free"
                 , strip.position = "right") +
      geom_point(data = subset_stats_pred_perf
                 , mapping = aes(shape = regression_formula_label
                 )
                 , position = position_dodge(0.9)
                 , size = 5) +
      geom_point(data = subset_stats_pred_perf %>%
                   filter(grepl("sunscreen_usage_ordinal", regression_formula) == TRUE)
                 , mapping = aes(x = BIC_min
                                 # , y =  race
                                 , group = race_weight_perception_together)
                 , shape = 124
                 , color = "red"
                 , position = position_dodge(0.9)
                 , size = 5) +
      geom_point(data = subset_stats_pred_perf
                 , mapping = aes(x = BIC_max
                                 # , y =  race
                                 , group = race_weight_perception_together)
                 , shape = 124
                 , color = "purple"
                 , position = position_dodge(0.9)
                 , size = 5) +
      geom_segment(data = subset_sunscreen_wide
                   , mapping = aes(x = without_sunscreen_usage
                                   , xend = with_sunscreen_usage
                                   , y = race_weight_perception_together #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                   , yend = race_weight_perception_together #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                   , group = race_weight_perception_together
                                   , color = "sunscreen"
                   )
                   , inherit.aes = FALSE) +
      geom_text(data = subset_sunscreen_wide
                , mapping = aes(y = race_weight_perception_together #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                , x = mean_position_sunscreen
                                , group = race_weight_perception_together
                                , label = label_explained_by_sunscreen)
                , size = 5
                , nudge_y = 0.2
                , inherit.aes = FALSE) +
      geom_segment(data = subset_sunscreen_wide
                   , mapping = aes(x = with_sunscreen_usage
                                   , xend = BIC_min
                                   , y = race_weight_perception_together #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                   , yend = race_weight_perception_together #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                   , group = race_weight_perception_together
                                   , color = "unknown"
                   )
                   , inherit.aes = FALSE) +
      geom_text(data = subset_sunscreen_wide
                , mapping = aes(y = race_weight_perception_together #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                , x = mean_position_unexplained
                                , group = race_weight_perception_together
                                , label = label_unexplained)
                , size = 5
                , nudge_y = 0.2
                , inherit.aes = FALSE
                , color = "red") +
      geom_segment(data = subset_sunscreen_wide
                   , mapping = aes(x = BIC_max
                                   , xend = without_sunscreen_usage
                                   , y = race_weight_perception_together #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                   , yend = race_weight_perception_together #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                   , group = race_weight_perception_together
                                   , color = "demographics and\nbody dissatisfaction"
                   )
                   , inherit.aes = FALSE) +
      geom_text(data = subset_sunscreen_wide
                , mapping = aes(y = race_weight_perception_together #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                , x = mean_position_others
                                , group = race_weight_perception_together
                                , label = label_others)
                , size = 5
                , nudge_y = 0.2
                , inherit.aes = FALSE
                , color = "purple") +
      scale_shape_manual(values = c(49
                                    , 52
                                    , 50
                                    , 53
                                    , 55
                                    , 56)) +
      scale_color_manual(values = c("purple"
                                    , "black"
                                    , "red")) +
      guides(shape = guide_legend(title = "Regression Models")
             , color = guide_legend(title = "BP3 levels explained by")) +
      theme(legend.position = "top"
            , legend.direction = "vertical"
            , panel.background = element_rect(fill = "#FAF9F6")
            , panel.grid.major = element_line(size = 0.25
                                              , linetype = 'solid'
                                              ,colour = "lightgray")
            , panel.grid.minor = element_line(size = 0.25
                                              , linetype = 'solid'
                                              , colour = "lightgray")
            ,  panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5)
            , axis.title.y = element_blank()
            , axis.text.y = element_blank()
            , axis.text.x = element_text(size = 12)
            , axis.title.x = element_text(size = 12)
            , legend.text = element_text(size = 10)
            , legend.title = element_text(size = 12))

    plot_name.png <- paste("alphabet_soup_plot_"
                           , combination_i %>%
                             gsub(" - | "
                                  , "_"
                                  , .) %>%
                             gsub("_\\+_"
                                  , "_"
                                  , .)
                           , ".png"
                           , sep = "")

    plot_name.pdf <- paste("alphabet_soup_plot_"
                           , combination_i %>%
                             gsub(" - | "
                                  , "_"
                                  , .) %>%
                             gsub("_\\+_"
                                  , "_"
                                  , .)
                           , ".pdf"
                           , sep = "")

    # Save the panel of plots as a png and pdf
    print(plot_name.png)
    ggsave(filename = plot_name.png
           , plot = alphabet_soup_plot
           , width = 14
           , height = 18
           , units = "in")
    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = alphabet_soup_plot
           , width = 14
           , height = 18
           , units = "in")

    list_wide_regressions[[combination_i]] <- subset_sunscreen_wide

  }
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)

  df_wide_regression <- list_wide_regressions %>%
    reduce(.
           , full_join)
  # View(df_wide_regression)
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  
  write.xlsx(x = df_wide_regression
             , file = "bic_without_vs_with_sunscreen_adjustment.xlsx"
             , sheetName = "tidy")

  alphabet_soup_plot
}