forest_plot_gap_widening <- function(list_all
                                     , list_stratified
                                     , name_of_folder 
                                     , current_directory
                                     ,is_adult=TRUE)
{
  library(tidyverse)
  library(RColorBrewer)
  library(seriation)
  library(ggrepel)
  library(cowplot)
  library(ggpubr)
  
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
    filter(regression_formula != "log10(URXBP3) ~ race + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI")
  
  df_combination <- df_regression %>%
    select(account_sampling_design
           , type_sample_size
           , covariates) %>%
    unique(.) %>%
    mutate(combination = paste(account_sampling_design
                               , type_sample_size
                               , covariates
                               , sep = " - "))
  print(df_combination)
  
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
      filter(grepl("race", term) == TRUE) %>%
      filter(covariates == covariates_i) %>%
      mutate(adj_pvalues = p.adjust(p.value, method = "BH")) %>%
      mutate(asterisk_adj = case_when(adj_pvalues > 0.05 ~ ""
                                      , adj_pvalues > 0.01 & adj_pvalues <= 0.05 ~ "*"
                                      , adj_pvalues > 0.001 & adj_pvalues <= 0.01 ~ "**"
                                      , adj_pvalues <= 0.001 ~ "***")) %>%
      mutate(term = gsub("race|race_weight_perception_"
                         , ""
                         , term)) %>%
      mutate(race_weight_perception = term) %>%
      relocate(race_weight_perception
               , .after = term) %>%
      separate(race_weight_perception
               , into = c("race"
                         , "weight_perception")
               , sep = "_") %>%
      mutate(weight_perception = ifelse(is.na(weight_perception) == TRUE
                                        , "all"
                                        , weight_perception)) %>%
      mutate(label_ci = paste(percent_diff %>%
                                round(.)
                              , "% ["
                              , perc_diff_ci_low %>%
                                round(.)
                              , "%,"
                              , perc_diff_ci_high %>%
                                round(.)
                              , "%]"
                              , sep = ""))
    # View(subset_regression)
    regression_model_i <- subset_regression %>%
      pull(regression_formula) %>%
      unique(.)
    # print(regression_model_i)
    
    # View(list_all[["glance"]])
    num_observations <- list_all[["glance"]] %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i) %>%
      filter(covariates == covariates_i) %>%
      pull(nobs) %>%
      unique(.)
    # print(num_observations)
    
    df_widening_gaps <- subset_regression %>%
      filter(weight_perception != "all") 
    # View(df_widening_gaps)
    
    df_stats_widening_gap <- df_widening_gaps %>%
      group_by(race) %>%
      summarise(dif = diff(percent_diff)
                , average = mean(percent_diff)) %>%
      ungroup(.) %>%
      arrange(dif, average)
    # View(df_stats_widening_gap)
    
    race_ordered_by_stats <- df_stats_widening_gap %>%
      pull(race)
    
    subset_regression$race <- factor(subset_regression$race
                                     , levels = race_ordered_by_stats)
    
    max_perc_diff <- subset_regression %>%
      pull(percent_diff) %>%
      max(.)
    # print(max_perc_diff)
    
    min_perc_diff <- subset_regression %>%
      pull(percent_diff) %>%
      min(.)
    # print(min_perc_diff)
    
    # print(colnames(subset_regression))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Extract Legend from Dummy Plot  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    dummy_plot <- ggplot(data = subset_regression %>%
                           mutate(weight_perception = factor(weight_perception
                                                             , levels = c("all"
                                                                          , "overweight"
                                                                          , "about the right weight"
                                                                          )))) +
      geom_point(mapping = aes(x = race
                               , y = percent_diff
                               , color = weight_perception)
                 , size = 4) +
      scale_color_manual(values = c("black"
                                   , "red"
                                   , "blue")
                         , labels = c("All"
                                      , "Perceived overweight"
                                      , "Perceived at the right weight"
                                      )) +
      guides(color = guide_legend(title = "Weight Perception")) +
      theme(legend.position = "top"
            , legend.text = element_text(size = 14))
    
    
    legend_colors <- get_legend(dummy_plot)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~  Create Forest Plot for non-Hispanic Black Women  ~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    df_tidy_regression_black <- list_stratified[["tidy"]] %>%
      filter(race == "Non-Hispanic Black") %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      # filter(type_sample_size == type_sample_size_i) %>%
      filter(covariates == covariates_i) %>%
      filter(grepl("weight_perception", term) == TRUE) %>%
      mutate(term = "Perceived overweight") %>%
      mutate(label_ci = paste(percent_diff %>%
                             round(.)
                           , "% ["
                           , perc_diff_ci_low %>%
                             round(.)
                           , "%,"
                           , perc_diff_ci_high %>%
                             round(.)
                           , "%]"
                           , sep = ""))
    # print(df_tidy_regression_black)
    # print(colnames(df_tidy_regression_black))
    
    forest_plot_black <- ggplot(data = df_tidy_regression_black
                                , mapping = aes(x = term
                                                , y = percent_diff)
                                ) +
      geom_point(color = "red"
                 , size = 3.5) +
      geom_errorbar(mapping = aes(ymin = perc_diff_ci_low
                                  , ymax = perc_diff_ci_high)
                    , width = 0.2
                    , linewidth = 1.5
                    , color = "red") +
      facet_wrap(~race) +
      ylim(ifelse(min_perc_diff < 0
                  , min_perc_diff
                  , 0)
           , max_perc_diff) +
      geom_text(mapping = aes(x = term
                              , y = percent_diff
                              , label = asterisks)
                , size = 5
                , nudge_x = 0.15
                , nudge_y = -0.35) +
      geom_text_repel(mapping = aes(x = term
                                      , y = percent_diff
                                      , label = label_ci)
                      , size = 3
                      , nudge_x = -0.3
                      , segment.color = 'transparent'
                      ) +
      labs(#y = "Percent Difference of BP3" #relative to women who perceived at the right weight"
           tag = "B") +
      theme(axis.text.x = element_text(size = 12)
            , axis.text.y = element_text(size = 14)
            , axis.title.x = element_blank()
            , axis.title.y = element_blank()
            , strip.text = element_text(size = 15))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Create Arrow Forest Plots  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    
    subset_regression_all <- subset_regression %>%
      filter(weight_perception == "all") 
    # View(subset_regression_all)
    
    subset_regression_right_weight <- subset_regression %>%
      filter(weight_perception == "about the right weight")
    
    subset_regression_overweight <- subset_regression %>%
      filter(weight_perception == "overweight")
    
    subset_regression_all <- subset_regression_all %>%
      mutate(group = "All NHANES Women")
    
    forest_plot <- ggplot(subset_regression_all) +
      geom_segment(data = subset_regression_right_weight
                   , mapping = aes(x = race
                                   , xend = race
                                   , y = subset_regression_all$percent_diff
                                   , yend = percent_diff)
                   , arrow = arrow(length = unit(0.2, "inches"))
                   , size = 2
                   , color = "blue"
                   , inherit.aes = FALSE) +
      geom_segment(data = subset_regression_overweight
                   , mapping = aes(x = race
                                   , xend = race
                                   , y = subset_regression_all$percent_diff
                                   , yend = percent_diff)
                   , arrow = arrow(length = unit(0.2, "inches"))
                   , size = 2
                   , color = "red"
                   , inherit.aes = FALSE) +
      geom_point(data = subset_regression_all
                 , mapping = aes(x = race
                                 , y = percent_diff)
                 , size = 3.5) +
      geom_text(data = subset_regression
                , mapping = aes(x = race
                                , y = percent_diff
                                , label = asterisks)
                , size = 5
                , nudge_x = 0.15
                , nudge_y = -0.35
                , inherit.aes = FALSE) +
      geom_text_repel(data = subset_regression
                , mapping = aes(x = race
                                , y = percent_diff
                                , label = label_ci)
                , size = 3
                , nudge_x = -0.3
                , segment.color = 'transparent'
                , inherit.aes = FALSE) +
      facet_wrap(~group) +
      ylim(ifelse(min_perc_diff < 0
                  , min_perc_diff
                  , 0)
           , max_perc_diff) +
      # ggtitle(label = paste(combination_i
      #                       , " - (N = "
      #                       , num_observations
      #                       , ")"
      #                       , sep = "")) +
      labs(y = "Percent Difference of BP3" #relative to Non-Hispanic Black Women"
           , tag = "A") +
      theme(axis.title.x = element_blank()
            , plot.title = element_text(size = 14)
            , axis.text = element_text(size = 12)
            , axis.title.y = element_text(size = 14)
            , strip.text = element_text(size = 15))
    
    final_plot <- ggarrange(forest_plot
                            , forest_plot_black
                            , ncol = 2
                            , nrow = 1
                            , widths = c(8,2))
    
    final_plot <- ggarrange(legend_colors
                            , final_plot
                            , ncol = 1
                            , nrow = 2
                            , heights = c(0.8,10))
    if(is_adult == TRUE) {
      plot_name.png <- paste("forest_plot_"
                           , combination_i %>%
                             gsub(" \\+ "
                                  , "_"
                                  , .)
                           , "_"
                           , "glm"
                           , ".png"
                           , sep = "")

      plot_name.pdf <- paste("forest_plot_"
                           , combination_i %>%
                             gsub(" \\+ "
                                  , "_"
                                  , .)
                           , "_"
                           , "glm"
                           , ".pdf"
                           , sep = "")
    } else {
      plot_name.png <- paste("forest_plot_youth_"
                             , combination_i %>%
                               gsub(" \\+ "
                                    , "_"
                                    , .)
                             , "_"
                             , "glm"
                             , ".png"
                             , sep = "")
      
      plot_name.pdf <- paste("forest_plot_youth_"
                             , combination_i %>%
                               gsub(" \\+ "
                                    , "_"
                                    , .)
                             , "_"
                             , "glm"
                             , ".pdf"
                             , sep = "")
      
    }
    # Save the panel of plots as a png and pdf
    print(plot_name.png)
    ggsave(filename = plot_name.png
           , plot = final_plot
           , width = 15
           , height = 9
           , units = "in")
    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = final_plot
           , width = 15
           , height = 9
           , units = "in")
    
  }
  
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  final_plot
  # forest_plot_black
}