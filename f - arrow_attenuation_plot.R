arrow_attenuation_plot <- function(list_all
                                   , list_ref
                                   , name_of_folder 
                                   , current_directory
                                   , is_adult = TRUE)
{
  library(tidyverse)
  library(RColorBrewer)
  library(seriation)
  library(ggrepel)
  library(cowplot)
  library(ggpubr)
  library(dplyr)
  library(ggh4x)
  
  # install.packages('reticulate')
  # reticulate::install_miniconda()
  # reticulate::conda_install('r-reticulate', 'python-kaleido')
  # reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
  # reticulate::use_miniconda('r-reticulate')
  
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
  
  df_regression_all <- list_all[["tidy"]] %>%
    filter(regression_formula != "log10(URXBP3) ~ race + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI") %>%
    mutate(group = "All NHANES Women") %>%
    filter(type_sample_size == "same across models") %>%
    filter(grepl("race", term) == TRUE)
  
  df_regression_ref <- list_ref[["tidy"]] %>%
    filter(grepl("weight_perception_", term) == TRUE) %>%
    filter(type_sample_size == "same across models") %>%
    mutate(group = "Non-Hispanic Blacks") 
  
  df_regression <- full_join(df_regression_all
                             , df_regression_ref
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
    mutate(baseline_covariates = gsub(" \\+ sunscreen_usage_ordinal"
                                      , ""
                                      , covariates)) %>%
    mutate(with_sunscreen = ifelse(grepl("sunscreen_usage_ordinal", covariates)
                                   , "yes"
                                   , "no")) %>%
    mutate(race_weight_perception = paste(race
                                          , weight_perception
                                          , sep = " - ") %>%
             gsub(" - all"
                  , ""
                  , .) %>%
             gsub(" - "
                  , "\n"
                  , .)) 
  # View(df_regression)
  
  df_combination <- df_regression %>%
    filter(baseline_covariates != "RIDAGEYR + SDDSRVYR + URXUCR + INDFMPIR") %>%
    select(account_sampling_design
           , type_sample_size
           , baseline_covariates) %>%
    unique(.) %>%
    mutate(combination = paste(account_sampling_design
                               , type_sample_size
                               , baseline_covariates
                               , sep = " - "))
  # View(df_combination)
  
  list_wide_regressions <- list()
  
  num_combinations <- nrow(df_combination)
  
  for(i in seq(num_combinations))
  {
    subset_combination <- df_combination[i,]
    
    combination_i <- subset_combination %>%
      pull(combination)
    print(combination_i)
    
    type_sampling_design_i <- subset_combination %>%
      pull(account_sampling_design)
    
    type_sample_size_i <- subset_combination %>%
      pull(type_sample_size)
    
    baseline_covariates_i <- subset_combination %>%
      pull(baseline_covariates)
    
    subset_fold_diffs <- df_regression %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i) %>%
      filter(baseline_covariates == baseline_covariates_i) %>%
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
      arrange(race
              , weight_perception) %>%
      mutate(group_combination = ifelse(weight_perception == "all"
                                        , "Race/ethnicity"
                                        , "Race/ethnicity &\nBody Dissatisfaction")) %>%
      mutate(weight_perception = factor(weight_perception
                                        , levels = c("all"
                                                     , "overweight"
                                                     , "about the right weight")))
    # View(subset_fold_diffs)
    
    subset_fold_diffs_wide <- subset_fold_diffs %>%
      select(race_weight_perception
             , weight_perception
             , group_combination
             , with_sunscreen
             , fold_diff) %>%
      pivot_wider(names_from = with_sunscreen
                   , values_from = fold_diff) %>%
      mutate(percent_change = abs(no - yes)/abs(1 - no)*100) %>%
      mutate(position_label = (no + yes)/2) %>%
      mutate(percent_change_label = paste(percent_change %>%
                                            round(., digits = 1)
                                          , "%"
                                          , sep = "")) %>%
      mutate(yes_corrected = case_when(yes > 1 & no > 1 ~ yes + 0.015
                             , yes < 1 & no < 1 ~ yes - 0.015
                             , no > 1 & yes < 1 ~ yes + 0.015)) %>%
      mutate(no_corrected = case_when(yes > 1 & no > 1 ~ no - 0.015
                            , yes < 1 & no < 1 ~ no + 0.015
                            , no > 1 & yes < 1 ~ no - 0.015))
    # View(subset_fold_diffs_wide)
    
    current_levels_regression_models <- subset_fold_diffs %>% 
      pull(regression_formula_label) %>%
      unique(.) %>%
      sort(.) %>%
      rev(.) %>%
      .[c(2,1,4,3,6,5)]
    
    ordered_race_weight_perception <- subset_fold_diffs_wide %>%
      arrange(percent_change) %>%
      pull(race_weight_perception) %>%
      unique(.)
    
    subset_fold_diffs <- subset_fold_diffs %>%
      mutate(regression_formula_label = factor(regression_formula_label
                                               , levels = current_levels_regression_models)) %>%
      mutate(race_weight_perception = factor(race_weight_perception
                                             , levels = ordered_race_weight_perception)) 
    
    attenuation_plot <- ggplot(data = subset_fold_diffs
                               , mapping = aes(x = fold_diff
                                               , y = race_weight_perception
                                               , color = weight_perception
                                               , shape = regression_formula_label
                                               , group = weight_perception
                               )) +
      facet_wrap(vars(group_combination)
                 , ncol = 1
                 , scales = "free_y"
                 , strip.position = "right") +
      force_panelsizes(rows = c(0.4, 1)) +
      geom_point(size = 5) +
      geom_segment(data = subset_fold_diffs_wide 
                   , mapping = aes(x = no_corrected
                                   , xend = yes_corrected
                                   , y = race_weight_perception
                                   , yend = race_weight_perception
                                   , color = weight_perception
                                   , group = race_weight_perception)
                   , arrow = arrow(length = unit(0.1, "inches"))
                   , lineend = "butt"
                   , linejoin = "round"
                   , size = 1.0
                   , inherit.aes = FALSE) +
      geom_text(data = subset_fold_diffs_wide  
                , mapping = aes(y = race_weight_perception
                                , x = position_label
                                , color = weight_perception
                                , group = race_weight_perception
                                , label = percent_change_label)
                , size = 5
                , nudge_y = 0.3
                , inherit.aes = FALSE) +
      geom_vline(xintercept = 1) +
      guides(color = guide_legend(title = "Weight Perception")
             , shape = guide_legend(title = "Regression Models")) +
      scale_color_manual(values = c("black"
                                    , "red"
                                    , "blue")
                         , labels = c("All"
                                      , "Perceived overweight"
                                      , "Perceived at the right weight"
                         )) +
      scale_shape_manual(values = c(49
                                    , 52
                                    , 50
                                    , 53
                                    , 51
                                    , 54)) +
      xlab("Fold Differences of BP3 relative to Non-Hispanic Black Women") +
      ylab("") +
      theme(legend.position = "top"
            , legend.direction = "vertical"
            , legend.text = element_text(size = 12)
            , legend.title = element_text(size = 14)
            , strip.text = element_text(size = 16))
    
    plot_name.png <- paste("arrow_attenuation_plot_"
                           , combination_i %>%
                             gsub(" \\+ "
                                  , "_"
                                  , .) %>%
                             gsub(" - "
                                  , "_"
                                  , .) %>%
                             gsub("\\s"
                                  , "_"
                                  , .)
                           , ".png"
                           , sep = "")
    
    plot_name.pdf <- paste("arrow_attenuation_plot_"
                           , combination_i %>%
                             gsub(" \\+ "
                                  , "_"
                                  , .) %>%
                             gsub(" - "
                                  , "_"
                                  , .) %>%
                             gsub("\\s"
                                  , "_"
                                  , .)
                           , ".pdf"
                           , sep = "")
    
    # Save the panel of plots as a png and pdf
    print(plot_name.png)
    ggsave(filename = plot_name.png
           , plot = attenuation_plot
           , width = 19
           , height = 13
           , units = "in")
    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = attenuation_plot
           , width = 19
           , height = 13
           , units = "in")
    
    list_wide_regressions[[combination_i]] <- subset_fold_diffs_wide %>%
      mutate(account_sampling_design = type_sampling_design_i) %>%
      mutate(type_sample_size = type_sample_size_i) %>%
      mutate(baseline_covariates = baseline_covariates_i)
    
  }
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  
  df_wide_regression <- list_wide_regressions %>%
    reduce(.
           , full_join)
  # View(df_wide_regression)
  
  write.xlsx(x = df_wide_regression
             , file = "fold_differences_without_vs_with_sunscreen_adjustment.xlsx"
             , sheetName = "tidy")
  
  attenuation_plot
}