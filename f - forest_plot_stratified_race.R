forest_plot_stratified_race <- function(list_all
                                        , name_of_folder 
                                        , current_directory
                                        , is_adult)
{
  library(tidyverse)
  library(RColorBrewer)
  library(seriation)
  library(ggrepel)
  library(cowplot)
  library(ggpubr)
  library(dplyr)
  
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
    mutate(term = "Perceived overweight vs. perceived at the right weight")
  # View(df_regression_all)
  print(colnames(df_regression))
  
  df_combination <- df_regression %>%
    .[c("account_sampling_design"
        , "type_sample_size"
        , "covariates")] %>%
    unique(.) %>%
    mutate(combination = paste(account_sampling_design
                               , type_sample_size
                               , covariates
                               , sep = " - "))
  # print(df_combination)
  
  num_combinations <- nrow(df_combination)
  
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
    
    covariates_i <- subset_combination %>%
      pull(covariates)
    
    subset_regression <- df_regression %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i) %>%
      filter(covariates == covariates_i) %>%
      arrange(fold_diff)
    # print(subset_regression)
    
    ordered_race_by_fold_diff <- subset_regression %>%
      pull(race)
    
    subset_regression$race <- factor(subset_regression$race
                                     , levels = ordered_race_by_fold_diff)
    
    forest_plot_race <- ggplot(data = subset_regression
                               , mapping = aes(x = race
                                               , y = fold_diff)) +
      geom_point(size = 4) +
      geom_errorbar(mapping = aes(ymin = fold_diff_ci_low
                                  , ymax = fold_diff_ci_high)
                    , width = .2) +
      geom_hline(yintercept = 1) +
      labs(y = "Fold Difference of BP3 between perceived as overweight vs. at the right weight") +
      theme(axis.title.x = element_blank()
            , axis.text = element_text(size = 12)
            , axis.title.y = element_text(size = 16))
    
    if(is_adult == TRUE)
    {
      population_tag <- "adult"
    } else {
      population_tag <- "youth"
    }
    
    plot_race_name.png <- paste("forest_plot_stratified_"
                                , population_tag
                                , "_"
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
    
    plot_race_name.pdf <- paste("forest_plot_stratified_"
                                , population_tag
                                , "_"
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
    print(plot_race_name.png)
    ggsave(filename = plot_race_name.png
           , plot = forest_plot_race
           , width = 14
           , height = 9
           , units = "in")
    print(plot_race_name.pdf)
    ggsave(filename = plot_race_name.pdf
           , plot = forest_plot_race
           , width = 14
           , height = 9
           , units = "in")
  }
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  # forest_plot_race
}