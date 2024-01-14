alphabet_soup_rsq <- function(regression_stratified 
                              , regression_all 
                              , name_of_folder
                              , current_directory)
{
  library(tidyverse)
  
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
  
  df_rsq <- regression_stratified[["glance"]] %>%
    full_join(.
              , regression_all[["glance"]] %>%
                mutate(race = "All NHANES Women")
              , by = NULL) %>%
    filter(covariates %in% relevant_covariates) %>%
    filter(type_sample_size == "same across models") %>%
    filter(grepl("race \\+ "
                 , regression_formula) == FALSE) %>%
    mutate(sunscreen_included = ifelse(grepl("sunscreen_usage_ordinal", regression_formula) == TRUE
                                       , "with sunscreen usage"
                                       , "without sunscreen usage"))
           
  # View(df_rsq)
  
  print(colnames(df_rsq))
  
  df_stats <- df_rsq %>%
    group_by(race
             , account_sampling_design
             , type_sample_size) %>%
    summarise(rsq_sunscreen_contribution = diff(r.squared)
              , adj_rsq_sunscreen_contribution = diff(adj.r.squared)) %>%
    ungroup(.) %>%
    pivot_longer(cols = c("rsq_sunscreen_contribution"
                          , "adj_rsq_sunscreen_contribution")
                 , names_to = "rsq_type") %>%
    mutate(rsq_type = gsub("_sunscreen_contribution"
                           , ""
                           , rsq_type))
  # View(df_stats)
  
  df_combination <- df_stats  %>%
    select(account_sampling_design
           , type_sample_size
           , rsq_type) %>%
    unique(.) %>%
    mutate(combination = paste(account_sampling_design
                               , type_sample_size
                               , rsq_type
                               , sep = " - "))
  print(df_combination)
  
  num_combinations <- nrow(df_combination)
  
  for(i in seq(num_combinations)[1])
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
    
    type_rsq_i <- subset_combination %>%
      pull(rsq_type)
    
    subset_stats_contribution <- df_stats %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i) %>%
      filter(rsq_type == type_rsq_i)
    # print(subset_stats_contribution)
    # print(colnames(subset_stats_contribution))
    
    subset_stats_rsq <- df_rsq %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i)
    print(subset_stats_rsq)
    # print(colnames(subset_stats_rsq))
    
    rsq_pattern <- ifelse(type_rsq_i == "rsq"
                          , "r.squared"
                          , "adj.r.squared")
    
    # print(subset_stats_rsq[,rsq_pattern])
    # print(subset_stats_rsq$race)
    
    subset_sunscreen_wide <- subset_stats_rsq %>%
      select(race
             , sunscreen_included
             , all_of(rsq_pattern)) %>%
      mutate(sunscreen_included = gsub(" "
                                       , "_"
                                       , sunscreen_included)) %>%
      pivot_wider(names_from = "sunscreen_included"
                  , values_from = rsq_pattern) %>%
      full_join(.
                , subset_stats_contribution
                , by = "race") %>%
      mutate(label = round(value*100, digits = 1) %>%
               paste(.
                     , "%"
                     , sep = "")) %>%
      mutate(mean_rsq = (without_sunscreen_usage + with_sunscreen_usage)/2) %>%
      arrange(value)
    print(subset_sunscreen_wide)
    
    ordered_race <- subset_sunscreen_wide %>%
      pull(race)
    
    subset_sunscreen_wide <- subset_sunscreen_wide %>%
      mutate(race = factor(race
                           , levels = ordered_race))
    
    subset_stats_rsq <- subset_stats_rsq %>%
      mutate(race = factor(race
                           , levels = ordered_race))
    
    alphabet_soup_plot <- ggplot() +
      geom_segment(data = subset_sunscreen_wide
                   , mapping = aes(x = without_sunscreen_usage
                                   , xend = with_sunscreen_usage
                                   , y = race
                                   , yend = race)) +
      geom_point(data = subset_stats_rsq
                 , mapping = aes(x = !!sym(rsq_pattern) 
                                 , y =  race
                                 , shape = sunscreen_included
                                 , color = sunscreen_included)
                 , size = 3) +
      geom_text(data = subset_sunscreen_wide
                , mapping = aes(y = race 
                                , x = mean_rsq
                                , label = label)
                , nudge_y = 0.2) +
      xlim(0,0.5) +
      theme(legend.position = "top")
    
  }
  

  
  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  alphabet_soup_plot
}