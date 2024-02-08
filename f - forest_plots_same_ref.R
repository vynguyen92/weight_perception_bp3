forest_plots_same_ref <- function(list_all
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
    filter(grepl("race", term) == TRUE)
  
  df_regression_ref <- list_ref[["tidy"]] %>%
    filter(grepl("weight_perception_", term) == TRUE) %>%
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
                                      , weight_perception))
  # View(df_regression)
  
  df_combination <- df_regression %>%
    .[c("account_sampling_design"
        , "type_sample_size"
        , "covariates")] %>%
    unique(.) %>%
    mutate(combination = paste(account_sampling_design
                               , type_sample_size
                               , covariates
                               , sep = " - "))
  # View(df_combination)
  
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
      mutate(label_ci = paste(fold_diff %>%
                                round(.
                                      , digits = 2)
                              , " ["
                              , fold_diff_ci_low %>%
                                round(.
                                      , digits = 2)
                              , ", "
                              , fold_diff_ci_high%>%
                                round(.
                                      , digits = 2)
                              , "]"
                              , sep = "")) %>%
      add_row(term = "raceNon-Hispanic Black"
              , race = "Non-Hispanic Black"
              , weight_perception = "all"
              , estimate = NA
              , std.error = NA
              , statistic = NA
              , p.value = NA
              , fold_diff = 1
              , percent_diff = 0
              , regression_formula = NA
              , account_sampling_design = type_sampling_design_i
              , type_sample_size = type_sample_size_i
              , perc_diff_ci_low = NA
              , perc_diff_ci_high = NA
              , asterisks = ""
              , covariates = covariates_i
              , group = "All NHANES Women"
              , label_ci = "Reference") %>%
      mutate(weight_perception = factor(weight_perception
                                        , levels = c("all"
                                                     , "overweight"
                                                     , "about the right weight")))
    # View(subset_regression)
    # print(colnames(subset_regression))
    
    subset_regression_race <- subset_regression %>%
      filter(weight_perception == "all") %>%
      arrange(fold_diff)
    
    subset_regression_race$race <- factor(subset_regression_race$race
                                          , levels = subset_regression_race$race)
    
    forest_plot_race <- ggplot(data = subset_regression_race
                               , mapping = aes(x = race
                                               , y = fold_diff)) +
      geom_point(size = 4) +
      geom_errorbar(mapping = aes(ymin = fold_diff_ci_low
                                  , ymax = fold_diff_ci_high)
                    , width = .2) +
      geom_hline(yintercept = 1) +
      labs(y = "Fold Difference of BP3 relative to Non-Hispanic Black Women") +
      theme(axis.title.x = element_blank()
            , axis.text = element_text(size = 12)
            , axis.title.y = element_text(size = 16))
      
    if(is_adult == TRUE)
    {
      population_tag <- "adult"
    } else {
      population_tag <- "youth"
    }
    
    plot_race_name.png <- paste("forest_plot_race_"
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
    
    plot_race_name.pdf <- paste("forest_plot_race_"
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
    
    forest_plot_race_weight_perception <- ggplot(data = subset_regression
                                                 , mapping = aes(x = race
                                                                 , y = fold_diff
                                                                 , color = weight_perception)) +
      geom_point(size = 4
                 , position = position_dodge(.3)) +
      geom_errorbar(mapping = aes(ymin = fold_diff_ci_low
                                  , ymax = fold_diff_ci_high)
                    , width = .2
                    , position = position_dodge(.3)) +
      geom_hline(yintercept = 1) +
      scale_color_manual(values = c("black"
                                    , "red"
                                    , "blue")
                         , labels = c("All"
                                      , "Perceived overweight"
                                      , "Perceived at the right weight"
                         )) +
      guides(color = guide_legend(title = "Weight Perception")) +
      labs(y = "Fold Difference of BP3 relative to Non-Hispanic Black Women") +
      theme(axis.title.x = element_blank()
            , legend.position = "top"
            , legend.text = element_text(size = 16)
            , legend.title = element_text(size = 16)
            , axis.text = element_text(size = 12)
            , axis.title.y = element_text(size = 16))
    
    plot_race_name.png <- paste("forest_plot_race_weight_perception_"
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
    
    plot_race_name.pdf <- paste("forest_plot_race_weight_perception_"
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
           , plot = forest_plot_race_weight_perception
           , width = 14
           , height = 9
           , units = "in")
    print(plot_race_name.pdf)
    ggsave(filename = plot_race_name.pdf
           , plot = forest_plot_race_weight_perception
           , width = 14
           , height = 9
           , units = "in")
    
  }
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
}