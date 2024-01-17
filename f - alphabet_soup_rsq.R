alphabet_soup_rsq <- function(regression_stratified 
                              , regression_all 
                              , name_of_folder
                              , current_directory
                              , is_adult = TRUE)
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
                                                    , "separate"))
  # View(df_rsq)
  # print(colnames(df_rsq))
  
  df_stats <- df_rsq %>%
    group_by(race
             , race_weight_perception_together
             , baseline_covariates
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
           , baseline_covariates
           , type_sample_size
           , rsq_type) %>%
    unique(.) %>%
    mutate(combination = paste(account_sampling_design
                               , type_sample_size
                               , rsq_type
                               , baseline_covariates
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

    type_rsq_i <- subset_combination %>%
      pull(rsq_type)

    baseline_covariates_i <- subset_combination %>%
      pull(baseline_covariates)

    subset_stats_contribution <- df_stats %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i) %>%
      filter(rsq_type == type_rsq_i) %>%
      filter(baseline_covariates == baseline_covariates_i)
    # print(subset_stats_contribution)
    # print(colnames(subset_stats_contribution))

    subset_stats_rsq <- df_rsq %>%
      filter(account_sampling_design == type_sampling_design_i) %>%
      filter(type_sample_size == type_sample_size_i) %>%
      filter(baseline_covariates == baseline_covariates_i)
    # View(subset_stats_rsq)
    # print(colnames(subset_stats_rsq))

    rsq_pattern <- ifelse(type_rsq_i == "rsq"
                          , "r.squared"
                          , "adj.r.squared")

    # print(subset_stats_rsq[,rsq_pattern])
    # print(subset_stats_rsq$race)

    subset_sunscreen_wide <- subset_stats_rsq %>%
      select(race
             , sunscreen_included
             , race_weight_perception_together
             , all_of(rsq_pattern)) %>%
      mutate(sunscreen_included = gsub(" "
                                       , "_"
                                       , sunscreen_included)) %>%
      pivot_wider(names_from = "sunscreen_included"
                  , values_from = rsq_pattern) %>%
      full_join(.
                , subset_stats_contribution
                , by = c("race"
                         , "race_weight_perception_together")) %>%
      mutate(label = round(value*100, digits = 1) %>%
               paste(.
                     , "%"
                     , sep = "")) %>%
      mutate(mean_rsq = (without_sunscreen_usage + with_sunscreen_usage)/2) %>%
      arrange(value)
    # View(subset_sunscreen_wide)
    # print(subset_sunscreen_wide)

    ordered_race <- subset_sunscreen_wide %>%
      pull(race) %>%
      unique(.)
    # print(ordered_race)
    
    df_positions_of_segments <- define_positions_segments(ordered_race = ordered_race)

    subset_sunscreen_wide <- subset_sunscreen_wide %>%
      mutate(race = factor(race
                           , levels = ordered_race)) 
    # View(subset_sunscreen_wide)

    subset_stats_rsq <- subset_stats_rsq %>%
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
                    , .)) 
    
    current_levels_regression_models <- subset_stats_rsq %>% 
      pull(regression_formula_label) %>%
      unique(.) %>%
      sort(.) %>%
      rev(.) %>%
      .[c(2,1,4,3,6,5)]
      
    subset_stats_rsq <- subset_stats_rsq %>%
      mutate(regression_formula_label = factor(regression_formula_label
                                               , levels = current_levels_regression_models))
    # View(subset_stats_rsq)
    # print(subset_stats_rsq$regression_formula_label %>% unique(.))
    
    alphabet_soup_plot <- ggplot(data = subset_stats_rsq
                                 , mapping = aes(x = !!sym(rsq_pattern)
                                                 , y =  race
                                                 , group = race_weight_perception_together
                                                 )) +
      geom_point(data = subset_stats_rsq
                 , mapping = aes(shape = regression_formula_label
                                 )
                 , position = position_dodge(0.9)
                 , size = 5) +
      geom_segment(data = subset_sunscreen_wide 
                   , mapping = aes(x = without_sunscreen_usage
                                   , xend = with_sunscreen_usage
                                   , y = unlist(df_positions_of_segments$position) #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                   , yend = unlist(df_positions_of_segments$position) #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                   , group = race_weight_perception_together
                                   , color = race_weight_perception_together
                                   )
                   , inherit.aes = FALSE
                   ) +
      geom_text(data = subset_sunscreen_wide  
                , mapping = aes(y = unlist(df_positions_of_segments$position) #c(1, 2, 3, 3.75, 4.25, 5, 6, 7)
                                , x = mean_rsq
                                , group = race_weight_perception_together
                                , label = label)
                , size = 5
                , nudge_y = 0.2
                , inherit.aes = FALSE
                ) +
      scale_shape_manual(values = c(49
                                    , 52
                                    , 50
                                    , 53
                                    , 51
                                    , 54)) +
      xlab("Coefficient of Determination, R2") +
      xlim(0,0.5) +
      guides(shape = guide_legend(title = "Regression Models")
             , color = "none"
                                    ) +
      theme(legend.position = "top"
            , legend.direction = "vertical"
            , axis.title.y = element_blank()
            , axis.text = element_text(size = 12)
            , axis.title.x = element_text(size = 12)
            , legend.text = element_text(size = 12)
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
           , height = 9
           , units = "in")
    print(plot_name.pdf)
    ggsave(filename = plot_name.pdf
           , plot = alphabet_soup_plot
           , width = 14
           , height = 9
           , units = "in")

  }

  # Set the directory to the folder containing the function and main scripts
  setwd(current_directory)
  alphabet_soup_plot
}