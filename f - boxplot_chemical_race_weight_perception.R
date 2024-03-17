boxplot_chemical_race_weight_perception <- function(df_nhanes
                                                    , covariates 
                                                    , chemical 
                                                    , name_of_folder
                                                    , current_directory
                                                    ,is_adult)
{
  library(tidyverse)
  library(ggsignif)
  
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
  
  df_nhanes_same_size <- df_nhanes %>%
    select(race
           , chemical
           , "SDMVPSU"
           , "SDMVSTRA"
           , "WT_URXBP3"
           , covariates) %>%
    na.omit(.) %>%
    # filter(race != "All NHANES Women") %>%
    mutate(race = factor(race
                         , levels = c("All NHANES Women"
                                      , "Non-Hispanic White"
                                      , "Non-Hispanic Black"
                                      , "Mexican American"                   
                                      , "Other Hispanic"
                                      , "Non-Hispanic Asian"
                                      , "Other Race - Including Multi-Racial"))) %>%
    mutate(race_weight_perception = ifelse(race == "Non-Hispanic Black"
                                           , "Non-Hispanic Black"
                                           , race_weight_perception)) %>%
    mutate(race_weight_perception = factor(race_weight_perception)) %>%
    mutate(weight_perception = factor(weight_perception
                                      , levels = c("_overweight"
                                                   , "_about the right weight")))
  
  # print(dim(df_nhanes_same_size))
  
  
  chemical_name <- df_dictionary %>%
    filter(variable_codename_use == chemical) %>%
    pull(variable_description_use)
  
  boxplot_race_same_size <- ggplot(data = df_nhanes_same_size
                                   , mapping = aes(x = race
                                                   , y = df_nhanes_same_size[,chemical]
                                                   , fill = race)) +
    geom_boxplot() +
    # facet_grid(cols = vars(race)) +
    stat_summary(fun.y = mean
                 , geom = "point"
                 , shape = 24
                 , size = 4
                 , fill = "gray") +
    scale_y_log10() +
    labs(y = chemical_name) +
    theme(legend.position = "top"
          , axis.text.x = element_blank()
          , axis.title.x = element_blank())
  
  df_stats_sample_size <- df_nhanes_same_size %>%
    group_by(race, weight_perception) %>%
    summarise(n = n()) %>%
    ungroup(.)
  View(df_stats_sample_size)
  
  df_sample_size_label <- df_stats_sample_size %>%
    mutate(weight_perception = gsub(" "
                                    , "_"
                                    , weight_perception) ) %>%
    mutate(weight_perception = gsub("^_"
                                    , ""
                                    , weight_perception)) %>%
    pivot_wider(names_from = "weight_perception"
                , values_from = "n") %>%
    mutate(label = paste(overweight
                         , about_the_right_weight
                         , sep = "       "))
  # print(df_sample_size_label)
  
  df_nhanes_same_size <- full_join(df_nhanes_same_size
                                  , df_sample_size_label %>%
                                    select(race
                                           , label)
                                  , by = "race")
  
  boxplot_race_weight_perception_same_size <- ggplot(data = df_nhanes_same_size
                                                     , mapping = aes(x = weight_perception
                                                                     , y = df_nhanes_same_size[,chemical]
                                                                     , fill = weight_perception)) +
    geom_violin() +
    geom_boxplot(data = df_nhanes_same_size
                 , mapping = aes(x = weight_perception
                                 , y = df_nhanes_same_size[,chemical]
                                 )
                 , inherit.aes = FALSE
                 , width = 0.3) +
    stat_summary(fun.y = mean
                 , geom = "point"
                 , shape = 24
                 , size = 4
                 , fill = "gray"
    ) +
    facet_grid(cols = vars(race)) +
    geom_text(data = df_nhanes_same_size
              , mapping = aes(x = 1.5
                              , y = max(df_nhanes_same_size[,chemical]) + 10000
                              , label = label)) +
    geom_signif(comparisons = list(c("_overweight"
                                     , "_about the right weight"))
               , map_signif_level = TRUE
               , textsize = 5) +
    scale_y_log10() +
    scale_fill_manual(values = c(#"black"
                                   "red"
                                  , "blue")
                       , labels = c(#"All"
                                     "Perceived overweight"
                                    , "Perceived at the right weight"
                       )) +
    labs(y = chemical_name) +
    guides(fill = guide_legend(title = "Weight Perception")) +
    theme(legend.position = "top"
          , axis.text.x = element_blank()
          , axis.title.x = element_blank()
          , axis.text.y = element_text(size = 12)
          , axis.title.y = element_text(size = 12)
          , legend.title = element_text(size = 12)
          , legend.text = element_text(size = 12))
  
  if(is_adult == TRUE) {
    plot_name.png <- paste("box_plot_"
                         , "same_sample_size"
                         , ".png"
                         , sep = "")
  
    plot_name.pdf <- paste("box_plot_"
                         , "same_sample_size"
                         , ".pdf"
                         , sep = "")
  } else {
    plot_name.png <- paste("box_plot_youth_"
                           , "same_sample_size"
                           , ".png"
                           , sep = "")
    
    plot_name.pdf <- paste("box_plot_youth_"
                           , "same_sample_size"
                           , ".pdf"
                           , sep = "")
  }
  # Save the panel of stairway plots as a png and pdf
  print(plot_name.png)
  ggsave(filename = plot_name.png
         , plot = boxplot_race_weight_perception_same_size
         , width = 15
         , height = 9
         , units = "in")
  print(plot_name.pdf)
  ggsave(filename = plot_name.pdf
         , plot = boxplot_race_weight_perception_same_size
         , width = 15
         , height = 9
         , units = "in")
  
  df_nhanes_ref_black <- create_dataset_with_reference_as_black(df_nhanes_same_size)
  
  # View(df_nhanes_ref_black %>% select(race, weight_perception) %>% unique())
  
  boxplot_race_weight_perception_same_size_ref_black <- ggplot(data = df_nhanes_ref_black
                                                               , mapping = aes(x = weight_perception
                                                                               , y = df_nhanes_ref_black[,chemical]
                                                                               , fill = weight_perception)) +
    geom_boxplot() +
    geom_jitter() +
    facet_grid(cols = vars(race)) +
    stat_summary(fun.y = mean
                 , geom = "point"
                 , shape = 24
                 , size = 4
                 , fill = "gray") +
    # geom_signif(comparisons = list(c("_overweight"
    #                                  , "_about the right weight")), 
    #             map_signif_level = TRUE) +
    scale_y_log10() +
    # scale_fill_manual(values = c(#"black"
    #   "red"
    #   , "blue")
    #   , labels = c(#"All"
    #     "Perceived overweight"
    #     , "Perceived at the right weight"
    #   )) +
    labs(y = chemical_name) +
    theme(legend.position = "top"
          , axis.text.x = element_blank()
          , axis.title.x = element_blank())
  
  df_nhanes_max_size <- df_nhanes %>%
    select(weight_perception
           , chemical
           , race) %>%
    na.omit(.) %>%
    mutate(weight_perception = factor(weight_perception
                                      , levels = c("_overweight"
                                                   , "_about the right weight")))
  print(dim(df_nhanes_max_size))
  
  df_stats_sample_size <- df_nhanes_max_size %>%
    group_by(race, weight_perception) %>%
    summarise(n = n()) %>%
    ungroup(.)
  # View(df_stats_sample_size)
  
  df_sample_size_label <- df_stats_sample_size %>%
    mutate(weight_perception = gsub(" "
                                    , "_"
                                    , weight_perception) ) %>%
    mutate(weight_perception = gsub("^_"
                                    , ""
                                    , weight_perception)) %>%
    pivot_wider(names_from = "weight_perception"
                , values_from = "n") %>%
    mutate(label = paste(overweight
                         , about_the_right_weight
                         , sep = "    "))
  # print(df_sample_size_label)
  
  df_nhanes_max_size <- full_join(df_nhanes_max_size
                                  , df_sample_size_label %>%
                                    select(race
                                           , label)
                                  , by = "race")
  
  boxplot_max_size <- ggplot(data = df_nhanes_max_size 
                             , mapping = aes(x = weight_perception
                                             , y = df_nhanes_max_size[,chemical]
                                             , fill = weight_perception)) +
    geom_boxplot() +
    geom_jitter() +
    facet_grid(cols = vars(race)) +
    geom_signif(comparisons = list(c("_overweight"
                                     , "_about the right weight")),
                map_signif_level = TRUE) +
    stat_summary(fun.y = mean
                 , geom = "point"
                 , shape = 24
                 , size = 4
                 , fill = "gray") +
    geom_text(data = df_nhanes_max_size
              , mapping = aes(x = 1.5
                              , y = max(df_nhanes_max_size[,chemical]) + 10000
                              , label = label)) +
    scale_y_log10() +
    scale_fill_manual(values = c("red"
                                 , "blue")
                      , labels = c("Perceived overweight"
                                   , "Perceived at the right weight")) +
    labs(y = chemical_name) +
    theme(legend.position = "top"
          , axis.text.x = element_blank()
          , axis.title.x = element_blank())
  
  if(is_adult==TRUE) {
    plot_name.png <- paste("box_plot_"
                         , "max_sample_size"
                         , ".png"
                         , sep = "")
  
    plot_name.pdf <- paste("box_plot_"
                         , "max_sample_size"
                         , ".pdf"
                         , sep = "")
  } else {
    plot_name.png <- paste("box_plot_youth_"
                           , "max_sample_size"
                           , ".png"
                           , sep = "")
    
    plot_name.pdf <- paste("box_plot_youth_"
                           , "max_sample_size"
                           , ".pdf"
                           , sep = "")
    
  }
  # Save the panel of stairway plots as a png and pdf
  print(plot_name.png)
  ggsave(filename = plot_name.png
         , plot = boxplot_max_size
         , width = 14
         , height = 9
         , units = "in")
  print(plot_name.pdf)
  ggsave(filename = plot_name.pdf
         , plot = boxplot_max_size
         , width = 14
         , height = 9
         , units = "in")
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  boxplot_race_weight_perception_same_size
}