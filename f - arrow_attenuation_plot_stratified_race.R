arrow_attenuation_plot_stratified_race <- function(list_all
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
  
  df_regression <- list_all[["tidy"]] %>%
    filter(term == "weight_perception_overweight") %>%
    mutate(term = "Perceived overweight vs. perceived at the right weight") %>%
    mutate(with_sunscreen = ifelse(grepl("sunscreen_usage_ordinal", covariates)
                                   , "yes"
                                   , "no")) %>%
    mutate(baseline_covariates = gsub(" \\+ sunscreen_usage_ordinal"
                                      , ""
                                      , covariates))
  # View(df_regression_all)
  # print(colnames(df_regression))
  
  df_combination <- df_regression %>%
    .[c("account_sampling_design"
        , "type_sample_size"
        , "baseline_covariates")] %>%
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
                    , .))
    # View(subset_fold_diffs)
    # print(colnames(subset_fold_diffs))
    
    subset_fold_diffs_wide <- subset_fold_diffs %>%
      select(race
             , term
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
      mutate(yes_corrected = case_when(yes > 1 & no > 1 & yes > no ~ yes - 0.005
                                       , yes > 1 & no > 1 & yes < no ~ yes + 0.005
                                       , yes < 1 & no < 1 ~ yes - 0.005
                                       , yes < 1 & no > 1  ~ yes + 0.005)) %>%
      mutate(no_corrected = case_when(yes > 1 & no > 1 & yes > no ~ no + 0.005
                                      , yes > 1 & no > 1 & yes < no ~ no - 0.005
                                      , yes < 1 & no < 1 ~ no + 0.005
                                      , yes < 1 & no > 1 ~ no - 0.005
                                      )) %>%
      arrange(percent_change)
    # View(subset_fold_diffs_wide)
    
    race_ordered <- subset_fold_diffs_wide %>%
      pull(race) %>%
      unique(.)
    # print(race_ordered)
    
    subset_fold_diffs <- subset_fold_diffs %>%
      mutate(race = factor(race
                           , levels = race_ordered))
    
    subset_fold_diffs_wide <- subset_fold_diffs_wide %>%
      mutate(race = factor(race
                           , levels = race_ordered))
    
    attenuation_plot <- ggplot(data = subset_fold_diffs
                               , mapping = aes(x = fold_diff
                                               , y = race
                                               , shape = regression_formula_label
                               )) +
      geom_point(size = 5) +
      geom_segment(data = subset_fold_diffs_wide 
                   , mapping = aes(x = no_corrected
                                   , xend = yes_corrected
                                   , y = race
                                   , yend = race)
                   , arrow = arrow(length = unit(0.1, "inches"))
                   , lineend = "butt"
                   , linejoin = "round"
                   , size = 1.0
                   , inherit.aes = FALSE) +
      geom_text(data = subset_fold_diffs_wide  
                , mapping = aes(y = race
                                , x = position_label
                                , label = percent_change_label)
                , size = 5
                , nudge_y = 0.2
                , inherit.aes = FALSE) +
      geom_vline(xintercept = 1) +
      xlab("Fold Differences of BP3 between perceived as overweight vs. at the right weight") +
      ylab("") +
      guides(shape = guide_legend(title = "Regression Models")) +
      scale_shape_manual(values = c(55
                                    , 56)) +
      theme(legend.position = "top"
            , legend.direction = "vertical"
            , axis.title = element_text(size = 14)
            , axis.text = element_text(size = 12)
            , legend.text = element_text(size = 12)
            , legend.title = element_text(size = 14))
    
    plot_name.png <- paste("arrow_attenuation_plot_race_stratified_"
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
    
    plot_name.pdf <- paste("arrow_attenuation_plot_race_stratified_"
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
           , width = 15
           , height = 9
           , units = "in")
    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = attenuation_plot
           , width = 15
           , height = 9
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
             , file = "fold_differences_race_stratified_without_vs_with_sunscreen_adjustment.xlsx"
             , sheetName = "tidy")
  
  attenuation_plot
  
}