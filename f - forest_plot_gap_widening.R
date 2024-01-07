forest_plot_gap_widening <- function(df_regression
                                     , name_of_folder 
                                     , current_directory)
{
  library(tidyverse)
  library(RColorBrewer)
  library(seriation)
  
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
  
  df_regression <- df_regression %>%
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
  
  for(i in seq(num_combinations)[16])
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
      mutate(adj_pvalues = p.adjust(p.value.x, method = "BH")) %>%
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
                                        , weight_perception))
    View(subset_regression)
    
    max_perc_diff <- subset_regression %>%
      pull(percent_diff) %>%
      max(.)
    print(max_perc_diff)
    
    min_perc_diff <- subset_regression %>%
      pull(percent_diff) %>%
      min(.)
    print(min_perc_diff)
    
    print(colnames(subset_regression))
    
    subset_regression_all <- subset_regression %>%
      filter(weight_perception == "all") 
    # View(subset_regression_all)
    
    
    
    subset_regression_right_weight <- subset_regression %>%
      filter(weight_perception == "about the right weight")
    
    subset_regression_overweight <- subset_regression %>%
      filter(weight_perception == "overweight")
    
    forest_plot <- ggplot() +
      geom_segment(data = subset_regression_right_weight
                   , mapping = aes(x = race
                                   , xend = race
                                   , y = subset_regression_all$percent_diff
                                   , yend = subset_regression_right_weight$percent_diff)
                   , arrow = arrow(length = unit(0.2, "inches"))
                   , size = 2
                   , color = "blue") +
      geom_segment(data = subset_regression_overweight
                   , mapping = aes(x = race
                                   , xend = race
                                   , y = subset_regression_all$percent_diff
                                   , yend = subset_regression_overweight$percent_diff)
                   , arrow = arrow(length = unit(0.2, "inches"))
                   , size = 2
                   , color = "red") +
      geom_point(data = subset_regression_all
                 , mapping = aes(x = race
                                 , y = percent_diff)
                 , size = 3) +
      ylim(ifelse(min_perc_diff < 0
                  , min_perc_diff
                  , 0)
           , max_perc_diff)
    
  }
  
  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  forest_plot
}