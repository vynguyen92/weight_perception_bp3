panel_boxplot_chemical_race_weight_perception <- function(df_adults 
                                                          , df_youth  
                                                          , covariates_adults 
                                                          , covariates_youth 
                                                          , chemical 
                                                          , name_of_folder 
                                                          , current_directory)
{
  library(tidyverse)
  library(ggsignif)
  library(RColorBrewer)
  library(seriation)
  library(ggrepel)
  library(cowplot)
  library(ggpubr)
  library(dplyr)
  library(scales)
  
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
  
  subset_adults <- df_adults %>%
    select(chemical
           , "SDMVPSU"
           , "SDMVSTRA"
           , covariates_adults) %>%
    na.omit(.) %>%
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
  # print((subset_adults$race %>% unique(.)))
  
  subset_youth <- df_merge_youth %>%
    select(chemical
           , "SDMVPSU"
           , "SDMVSTRA"
           , covariates_youth) %>%
    na.omit(.) %>%
    mutate(race = as.character(race)) %>%
    mutate(race = ifelse(race == "All NHANES Women"
                         , "All NHANES Female Children"
                         , race)) %>%
    mutate(race = factor(race
                         , levels = c("All NHANES Female Children"
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
  # print((subset_youth$race %>% unique(.)))
  # print(dim(subset_youth))
  
  df_stats_sample_size <- subset_youth %>%
    group_by(race, weight_perception) %>%
    summarise(n = n()) %>%
    ungroup(.)
  # print(df_stats_sample_size)
  
  chemical_name <- df_dictionary %>%
    filter(variable_codename_use == chemical) %>%
    pull(variable_description_use)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Extract Legend from Dummy Plot  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  dummy_plot <- ggplot(data = subset_adults
                       , mapping = aes(x = weight_perception
                                       , y = subset_adults[,chemical]
                                       , fill = weight_perception)) +
    geom_violin() +
    geom_boxplot(data = subset_adults
                 , mapping = aes(x = weight_perception
                                 , y = subset_adults[,chemical])
                 , inherit.aes = FALSE
                 , width = 0.3) +
    scale_fill_manual(values = c("red", "blue")
                      , labels = c("Perceived overweight"
                                   , "Perceived at the right weight")) +
    guides(fill = guide_legend(title = "Weight Perception")) +
    theme(legend.position = "top"
          , axis.text.x = element_blank()
          , axis.title.x = element_blank()
          , axis.text.y = element_text(size = 12)
          , axis.title.y = element_text(size = 12)
          , legend.title = element_text(size = 18)
          , legend.text = element_text(size = 18))
  
  legend_colors <- get_legend(dummy_plot)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Box Plot for Adults  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  df_stats_t_test_adults <- subset_adults %>%
    group_by(race) %>%
    do(t.test(URXBP3 ~ weight_perception
              , data = .) %>% tidy(.)) %>%
    ungroup(.) %>%
    mutate(asterisks = case_when(p.value > 0.05 ~ ""
                                 , p.value > 0.01 & p.value <= 0.05 ~ "*"
                                 , p.value > 0.001 & p.value <= 0.01 ~ "**"
                                 , p.value <= 0.001 ~ "***")) %>%
    mutate(label = paste(asterisks
                         , "\n"
                         , "p-value: "
                         , p.value %>%
                           round(.
                                 , digits = 2)
                         , sep = ""))
  # View(df_stats_t_test_adults)
  # print(colnames(df_stats_t_test_adults))
  
  max_bp3 <- max(subset_adults$URXBP3) + 50000
  
  boxplot_adults <- ggplot(data = subset_adults
                           , mapping = aes(x = weight_perception
                                           , y = subset_adults[,chemical]
                                           , fill = weight_perception)
                           ) +
    geom_violin() +
    geom_boxplot(data = subset_adults
                 , mapping = aes(x = weight_perception
                                 , y = subset_adults[,chemical])
                 , inherit.aes = FALSE
                 , width = 0.3) +
    stat_summary(fun = mean
                 , geom = "point"
                 , shape = 24
                 , size = 4
                 , fill = "gray") +
    facet_grid(cols = vars(race)) +
    geom_text(data = df_stats_t_test_adults
              , mapping = aes(x = 1.5
                              , y = max_bp3
                              , label = label)
              , size = 6
              , inherit.aes = FALSE) +
    scale_y_log10(breaks = 10^c(0, seq(5))
                  , labels = label_comma()) +
    scale_fill_manual(values = c("red", "blue")
      , labels = c("Perceived overweight"
                   , "Perceived at the right weight")) +
    labs(y = chemical_name
         , tag = "A") +
    guides(fill = "none") +
    theme(legend.position = "none"
          , axis.text.x = element_blank()
          , axis.title.x = element_blank()
          , axis.text.y = element_text(size = 12)
          , axis.title.y = element_text(size = 16)
          , plot.tag = element_text(size = 22))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Box Plot for Youth  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  df_stats_t_test_youth <- subset_youth %>%
    group_by(race) %>%
    do(t.test(URXBP3 ~ weight_perception
              , data = .) %>% tidy(.)) %>%
    ungroup(.) %>%
    mutate(asterisks = case_when(p.value > 0.05 ~ ""
                                 , p.value > 0.01 & p.value <= 0.05 ~ "*"
                                 , p.value > 0.001 & p.value <= 0.01 ~ "**"
                                 , p.value <= 0.001 ~ "***")) %>%
    mutate(label = paste(asterisks
                         , "\n"
                         , "p-value = "
                         , p.value %>%
                           round(.
                                 , digits = 2)
                         , sep = ""))
  # View(df_stats_t_test_youth)
  
  max_bp3 <- max(subset_youth$URXBP3) + 50000
  
  boxplot_youth <- ggplot(data = subset_youth
                           , mapping = aes(x = weight_perception
                                           , y = subset_youth[,chemical]
                                           , fill = weight_perception)) +
    geom_violin() +
    geom_boxplot(data = subset_youth
                 , mapping = aes(x = weight_perception
                                 , y = subset_youth[,chemical])
                 , inherit.aes = FALSE
                 , width = 0.3) +
    stat_summary(fun = mean
                 , geom = "point"
                 , shape = 24
                 , size = 4
                 , fill = "gray") +
    facet_grid(cols = vars(race)) +
    geom_text(data = df_stats_t_test_youth
              , mapping = aes(x = 1.5
                              , y = max_bp3
                              , label = label)
              , size = 6
              , inherit.aes = FALSE) +
    scale_y_log10(breaks = 10^c(0, seq(5)) 
                  , labels = label_comma()) +
    scale_fill_manual(values = c("red", "blue")
                      , labels = c("Perceived overweight"
                                   , "Perceived at the right weight")) +
    labs(y = chemical_name
         , tag = "B") +
    guides(fill = "none") +
    theme(legend.position = "none"
          , axis.text.x = element_blank()
          , axis.title.x = element_blank()
          , axis.text.y = element_text(size = 12)
          , axis.title.y = element_text(size = 16)
          , plot.tag = element_text(size = 22))
  
  final_plot <- ggarrange(legend_colors
                          , boxplot_adults
                          , ncol = 1
                          , nrow = 2
                          , heights = c(0.8,10))
  
  final_plot <- ggarrange(final_plot
                          , boxplot_youth
                          , ncol = 1
                          , nrow = 2
                          , heights = c(5,4))
  
  plot_name.png <- paste("panel_boxplot_race_weight_perception_bp3"
                         , ".png"
                         , sep = "")
  
  plot_name.pdf <- paste("panel_boxplot_race_weight_perception_bp3"
                         , ".pdf"
                         , sep = "")
  
  # Save the panel of plots as a png and pdf
  print(plot_name.png)
  ggsave(filename = plot_name.png
         , plot = final_plot
         , width = 15
         , height = 17
         , units = "in")
  print(plot_name.pdf)
  ggsave(filename = plot_name.pdf
         , plot = final_plot
         , width = 15
         , height = 17
         , units = "in")
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  boxplot_adults
  boxplot_youth
  final_plot
}