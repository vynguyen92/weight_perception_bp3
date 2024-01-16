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
participant_counts <- count_excluded_participants(list_dataset = list("self_reported_weight" = df_self_reported_weight
                                                              , "dermatology" = df_dermatology
                                                              , "chemicals" = chemicals_clean
                                                              , "demographics" = demographics_clean
                                                              , "response" = response_clean
                                                              , "weights" = weights_clean)
                                          , vector_chemical_codenames = chemical_biomarker)

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
                           , "URXBP3")

categorical_variables <- c("weight_perception"
                           , "sunscreen_usage_cat")

df_population_stats <- create_table_population_statistics(df_nhanes = df_merge
                                                          , continuous_variables = continuous_variables
                                                          , categorical_variables = categorical_variables)
  
df_population_stats_youth <- create_table_population_statistics(df_nhanes = df_merge_youth
                                                                , continuous_variables = continuous_variables
                                                                , categorical_variables = "weight_perception"
                                                                , boolean = FALSE)

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
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Stratified Regression Models  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")
boxplot_chemical_race_weight_perception(df_nhanes = df_merge
                                        , covariates = vector_covariates
                                        , chemical = chemical_biomarker
                                        , name_of_folder = "Box Plot - BP3 by race and weight perception"
                                        , current_directory = working_directory,
                                        is_adult=TRUE)

boxplot_chemical_race_weight_perception(df_nhanes = df_merge
                                        , covariates = vector_covariates_youth
                                        , chemical = chemical_biomarker
                                        , name_of_folder = "Box Plot - BP3 by race and weight perception"
                                        , current_directory = working_directory
                                        ,is_adult=FALSE)


setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")
forest_plot_gap_widening(list_all = list_regression_stats
                         , list_stratified = list_regression_stats_stratified
                         , name_of_folder = "Forest Plot - Differences by race and weight perception"
                         , current_directory = working_directory
                         ,is_adult=TRUE)

forest_plot_gap_widening(list_all = list_regression_stats_youth
                         , list_stratified = list_regression_stats_stratified_youth
                         , name_of_folder = "Forest Plot - Differences by race and weight perception"
                         , current_directory = working_directory
                         ,is_adult = FALSE)

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")
alphabet_soup_rsq(regression_stratified = list_regression_stats_stratified
                  , regression_all = list_regression_stats
                  , name_of_folder = "Alphabet Soup Plot - Contribution of sunscreen for BP3"
                  , current_directory = working_directory)
