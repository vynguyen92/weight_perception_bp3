form_table_with_and_without_weight_perception <- function(list_all
                                                          , list_stratified)
{
  df_glance_all <- list_all[["glance"]] %>%
    filter(type_sample_size != "max sample size") %>%
    # mutate(covariates = gsub(" \\+ weight_perception| \\+ race_weight_perception"
    #                          , ""
    #                          , covariates) %>%
    #          gsub("log10\\(URXBP3\\) \\~ "
    #               , ""
    #               , .)) %>%
    mutate(covariates =  gsub("log10\\(URXBP3\\) \\~ "
                              , ""
                              , covariates)) %>%
    mutate(covariates = ifelse(grepl("log10\\(URXBP3\\) \\~ race \\+ ", regression_formula) == TRUE
                               , paste0("race + "
                                       , covariates)
                               , covariates))
    # mutate(equation = case_when(covariates == "RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + INDFMPIR" ~ "1"
    #                             , ))
    
  View(df_glance_all)
  print(colnames(df_glance_all))
  
  # df_stats <- df_glance_all %>%
  #   group_by(covariates
  #            , account_sampling_design) %>%
  #   summarise(contribution = diff(r.squared)) %>%
  #   ungroup(.)
  # View(df_stats)
  
  
}