#############################################################################################################
################  MAIN SCRIPT - CHECK SAMPLE SIZES AND US REPRESENTATION FOR COLON CANCER  ##################
#############################################################################################################

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")

working_directory <- "/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Form merge, working dataset  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

df_self_reported_weight <- form_specific_nhanes_dataset(pattern = "WHQ")

df_self_reported_weight_youth <- form_specific_nhanes_dataset(pattern = "WHQMEC")

df_dermatology <- form_specific_nhanes_dataset(pattern = "DEQ")

chemical_biomarker <- "URXBP3"

df_merge_youth <- merge_nhanes_dataset_together(list_dataset = list("self_reported_weight" = df_self_reported_weight_youth
                                                                    , "chemicals" = chemicals_clean
                                                                    , "demographics" = demographics_clean
                                                                    , "response" = response_clean
                                                                    , "weights" = weights_clean)
                                                , vector_chemical_codenames = chemical_biomarker
                                                , boolean_adult = FALSE)

# count the excluded participants and the reasons for exclusion
participant_counts_youth <- count_excluded_participants(list_dataset = list("self_reported_weight" = df_self_reported_weight_youth
                                                                      , "chemicals" = chemicals_clean
                                                                      , "demographics" = demographics_clean
                                                                      , "response" = response_clean
                                                                      , "weights" = weights_clean)
                                                  , vector_chemical_codenames = chemical_biomarker
                                                  , weight_perception = "WHQ030M"
                                                  , is_adult = FALSE)

participant_counts <- count_excluded_participants(list_dataset = list("self_reported_weight" = df_self_reported_weight 
                                                                            , "dermatology" = df_dermatology
                                                                            , "chemicals" = chemicals_clean
                                                                            , "demographics" = demographics_clean
                                                                            , "response" = response_clean
                                                                            , "weights" = weights_clean)
                                                  , vector_chemical_codenames = chemical_biomarker
                                                  , weight_perception = "WHQ030"
                                                  , is_adult = TRUE)

df_merge <- merge_nhanes_dataset_together(list_dataset = list("self_reported_weight" = df_self_reported_weight
                                                              , "dermatology" = df_dermatology
                                                              , "chemicals" = chemicals_clean
                                                              , "demographics" = demographics_clean
                                                              , "response" = response_clean
                                                              , "weights" = weights_clean)
                                          , vector_chemical_codenames = chemical_biomarker)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Population Statistics  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

continuous_variables <- c("RIDAGEYR" 
                          , "URXUCR"  
                          , "BMXBMI"
                          , "INDFMPIR"
                          , "URXBP3"
                          , "WT_URXBP3"
                          , "WTINT2YR")

categorical_variables <- c("weight_perception"
                           , "sunscreen_usage_cat")

df_population_stats <- create_table_population_statistics(df_nhanes = df_merge
                                                          , continuous_variables = continuous_variables
                                                          , categorical_variables = categorical_variables)
  
df_population_stats_youth <- create_table_population_statistics(df_nhanes = df_merge_youth
                                                                , continuous_variables = continuous_variables
                                                                , categorical_variables = "weight_perception"
                                                                , boolean_adult = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~  Chi-Square Tests & Regression on Sunscreen Usage and Weight Perception  ~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

list_chi_square <- conduct_chi_square_test(df_nhanes = df_merge
                                         , variable_1 = "weight_perception"
                                         , variable_2 = "sunscreen_usage_cat")

list_polyr_sunscreen_weight_perception <- run_univariate_ordinal_models(df_nhanes = df_merge
                                                                        , outcome = "sunscreen_usage_cat"
                                                                        , predictor = "weight_perception"
                                                                        , by_group = TRUE)

list_polyr_sunscreen_race_weight_perception <- run_univariate_ordinal_models(df_nhanes = df_merge
                                                                        , outcome = "sunscreen_usage_cat"
                                                                        , predictor = "race_weight_perception"
                                                                        , by_group = FALSE)

list_polyr_sunscreen_race <- run_univariate_ordinal_models(df_nhanes = df_merge
                                                           , outcome = "sunscreen_usage_cat"
                                                           , predictor = "race"
                                                           , by_group = FALSE)

list_polyr_sunscreen_PIR <- run_univariate_ordinal_models(df_nhanes = df_merge
                                                           , outcome = "sunscreen_usage_cat"
                                                           , predictor = "INDFMPIR"
                                                           , by_group = FALSE)

list_polyr_sunscreen_PIR_race <- run_univariate_ordinal_models(df_nhanes = df_merge
                                                          , outcome = "sunscreen_usage_cat"
                                                          , predictor = c("race", "INDFMPIR")
                                                          , by_group = FALSE)

list_polyr_sunscreen_race_weight_perception_fully <- run_univariate_ordinal_models(df_nhanes = df_merge
                                                            , outcome = "sunscreen_usage_cat"
                                                            , predictor = c("race_weight_perception"
                                                                            , "INDFMPIR"
                                                                            , "RIDAGEYR")
                                                            , by_group = FALSE)

list_polyr_sunscreen_race_fully <- run_univariate_ordinal_models(df_nhanes = df_merge
                                                            , outcome = "sunscreen_usage_cat"
                                                            , predictor = c("weight_perception"
                                                                            , "race"
                                                                            , "INDFMPIR"
                                                                            , "RIDAGEYR")
                                                            , by_group = FALSE)

list_polyr_sunscreen_stratified_by_race <- run_univariate_ordinal_models(df_nhanes = df_merge
                                                                        , outcome = "sunscreen_usage_cat"
                                                                        , predictor = c("weight_perception"
                                                                                        , "INDFMPIR"
                                                                                        , "RIDAGEYR")
                                                                        , by_group = TRUE)

list_lm_sunscreen_weight_perception <- run_univariate_lm_models(df_nhanes = df_merge
                                                                , outcome = "sunscreen_usage_ordinal"
                                                                , predictor = "weight_perception"
                                                                , by_group = TRUE)

list_lm_sunscreen_race_weight_perception <- run_univariate_lm_models(df_nhanes = df_merge
                                                                , outcome = "sunscreen_usage_ordinal"
                                                                , predictor = "race_weight_perception"
                                                                , by_group = FALSE)

list_lm_sunscreen_race <- run_univariate_lm_models(df_nhanes = df_merge
                                                   , outcome = "sunscreen_usage_ordinal"
                                                   , predictor = "race"
                                                   , by_group = FALSE)

list_lm_sunscreen_race_weight_perception_sep <- run_univariate_lm_models(df_nhanes = df_merge
                                                   , outcome = "sunscreen_usage_ordinal"
                                                   , predictor = c("race", "weight_perception")
                                                   , by_group = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Step-wise Regression Models  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

vector_regression_models <- c("log10(URXBP3) ~ race + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                              , "log10(URXBP3) ~ race + weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                              , "log10(URXBP3) ~ race_weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                              , "log10(URXBP3) ~ race + weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + INDFMPIR"
                              , "log10(URXBP3) ~ race_weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + INDFMPIR"
                              , "log10(URXBP3) ~ race + weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + sunscreen_usage_ordinal"
                              , "log10(URXBP3) ~ race_weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + sunscreen_usage_ordinal"
                              , "log10(URXBP3) ~ race + weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + INDFMPIR + sunscreen_usage_ordinal"
                              , "log10(URXBP3) ~ race_weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + INDFMPIR + sunscreen_usage_ordinal")

vector_regression_models_youth <- c("log10(URXBP3) ~ race + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                                    , "log10(URXBP3) ~ race + weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                                    , "log10(URXBP3) ~ race_weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                                    , "log10(URXBP3) ~ race + weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + INDFMPIR"
                                    , "log10(URXBP3) ~ race_weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + INDFMPIR")

vector_covariates <- c("race"
                       , "RIDAGEYR" 
                       , "URXUCR"  
                       , "BMXBMI"
                       , "INDFMPIR"
                       , "SDDSRVYR"
                       , "weight_perception"
                       , "race_weight_perception"
                       , "sunscreen_usage_ordinal")

vector_covariates_youth <- c("race"
                       , "RIDAGEYR" 
                       , "URXUCR"  
                       , "BMXBMI"
                       , "INDFMPIR"
                       , "SDDSRVYR"
                       , "weight_perception"
                       , "race_weight_perception")


list_regression_stats <- run_regression_models(df_nhanes = df_merge
                                             , covariates = vector_covariates
                                             , chemical = chemical_biomarker
                                             , regression_formulas = vector_regression_models)

list_regression_stats_youth <- run_regression_models(df_nhanes = df_merge_youth
                                                     , covariates = vector_covariates_youth
                                                     , chemical = chemical_biomarker
                                                     , regression_formulas = vector_regression_models_youth)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~  Regression Models in Non-Hispanic Blacks - Weight Perception Compared to All  ~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

vector_stratified_models <- c("log10(URXBP3) ~ weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                              , "log10(URXBP3) ~ weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + INDFMPIR"
                              , "log10(URXBP3) ~ weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + sunscreen_usage_ordinal"
                              , "log10(URXBP3) ~ weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + INDFMPIR + sunscreen_usage_ordinal")

vector_stratified_models_youth <- c("log10(URXBP3) ~ weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                                    , "log10(URXBP3) ~ weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + INDFMPIR")


vector_covariates_stratified <- c("race"
                                  , "RIDAGEYR" 
                                  , "URXUCR"  
                                  , "BMXBMI"
                                  , "INDFMPIR"
                                  , "SDDSRVYR"
                                  , "weight_perception"
                                  , "sunscreen_usage_ordinal")

vector_covariates_stratified_youth <- c("race"
                                        , "RIDAGEYR" 
                                        , "URXUCR"  
                                        , "BMXBMI"
                                        , "INDFMPIR"
                                        , "SDDSRVYR"
                                        , "weight_perception")

list_perception_vs_all_blacks <- run_perception_vs_all_models(df_nhanes = df_merge
                                                              , covariates = vector_covariates_stratified
                                                              , chemical = chemical_biomarker
                                                              , regression_formulas = vector_stratified_models
                                                              , race_group = "Non-Hispanic Black")

list_perception_vs_all_blacks_youth <- run_perception_vs_all_models(df_nhanes = df_merge_youth
                                                                    , covariates = vector_covariates_stratified_youth
                                                                    , chemical = chemical_biomarker
                                                                    , regression_formulas = vector_stratified_models_youth
                                                                    , race_group = "Non-Hispanic Black")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Stratified Regression Models  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

list_regression_stats_stratified <- run_stratified_regression_models(df_nhanes = df_merge
                                                                     , covariates = vector_covariates_stratified
                                                                     , chemical = chemical_biomarker
                                                                     , regression_formulas = vector_stratified_models)

list_regression_stats_stratified_youth <- run_stratified_regression_models(df_nhanes = df_merge_youth
                                                                     , covariates = vector_covariates_stratified_youth
                                                                     , chemical = chemical_biomarker
                                                                     , regression_formulas = vector_stratified_models_youth)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Visualization  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Box plots  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")
boxplot_chemical_race_weight_perception(df_nhanes = df_merge
                                        , covariates = vector_covariates
                                        , chemical = chemical_biomarker
                                        , name_of_folder = "Box Plot - BP3 by race and weight perception"
                                        , current_directory = working_directory
                                        , is_adult = TRUE)

boxplot_chemical_race_weight_perception(df_nhanes = df_merge_youth
                                        , covariates = vector_covariates_youth
                                        , chemical = chemical_biomarker
                                        , name_of_folder = "Box Plot - BP3 by race and weight perception"
                                        , current_directory = working_directory
                                        , is_adult = FALSE)

panel_boxplot_chemical_race_weight_perception(df_adults = df_merge
                                              , df_youth = df_merge_youth
                                              , covariates_adults = vector_covariates
                                              , covariates_youth = vector_covariates_youth
                                              , chemical = chemical_biomarker
                                              , name_of_folder = "Panel Box Plot - BP3 by race and weight perception"
                                              , current_directory = working_directory)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Arrow divergence plots  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")
arrow_forest_plot_same_ref(list_all = list_regression_stats
                           , list_ref = list_perception_vs_all_blacks
                           , name_of_folder = "Arrow Forest Plot Same Reference - Differences by race and weight perception"
                           , current_directory = working_directory
                           , is_adult = TRUE)

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")
arrow_forest_plot_same_ref(list_all = list_regression_stats_youth
                           , list_ref = list_perception_vs_all_blacks_youth
                           , name_of_folder = "Arrow Forest Plot Same Reference - Differences by race and weight perception"
                           , current_directory = working_directory
                           , is_adult = FALSE)

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")
arrow_forest_plot_same_ref_adult_youth(list_adults = list("all" = list_regression_stats
                                                          , "ref" = list_perception_vs_all_blacks)
                                       , list_youth = list("all" = list_regression_stats_youth
                                                           , "ref" = list_perception_vs_all_blacks_youth)
                                       , name_of_folder = "Arrow Forest Plot Same Reference Adult & Youth - Differences by race and weight perception"
                                       , current_directory = working_directory)

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")
arrow_forest_plot_all_stratified(list_all = list_regression_stats
                                 , list_stratified = list_regression_stats_stratified
                                 , name_of_folder = "Arrow Forest Plot - Differences by race and weight perception"
                                 , current_directory = working_directory
                                 , is_adult = TRUE)

arrow_forest_plot_all_stratified(list_all = list_regression_stats_youth
                                 , list_stratified = list_regression_stats_stratified_youth
                                 , name_of_folder = "Arrow Forest Plot - Differences by race and weight perception"
                                 , current_directory = working_directory
                                 , is_adult = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Alphabet soup plots  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")
alphabet_soup_rsq(regression_stratified = list_regression_stats_stratified
                  , regression_all = list_regression_stats
                  , name_of_folder = "Alphabet Soup Plot - Contribution of sunscreen for BP3"
                  , current_directory = working_directory
                  , is_adult = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Forest plots  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")
forest_plots_same_ref(list_all = list_regression_stats
                      , list_ref = list_perception_vs_all_blacks
                      , name_of_folder = "Forest Plot Same Reference - Differences by race and weight perception"
                      , current_directory = working_directory
                      , is_adult = TRUE)
