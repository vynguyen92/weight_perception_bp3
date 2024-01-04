#############################################################################################################
################  MAIN SCRIPT - CHECK SAMPLE SIZES AND US REPRESENTATION FOR COLON CANCER  ##################
#############################################################################################################

setwd("/Users/vynguyen/Dropbox/Mac/Documents/GitHub/weight_perception_bp3")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Form merge, working dataset  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

df_self_reported_weight <- form_specific_nhanes_dataset(pattern = "WHQ")

df_dermatology <- form_specific_nhanes_dataset(pattern = "DEQ")

chemical_biomarker <- "URXBP3"

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
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Step-wise Regression Models  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

vector_regression_models <- c("log10(URXBP3) ~ race + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                              , "log10(URXBP3) ~ race + weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                              , "log10(URXBP3) ~ race_weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI"
                              , "log10(URXBP3) ~ race_weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + sunscreen_usage_ordinal"
                              , "log10(URXBP3) ~ race_weight_perception + RIDAGEYR + SDDSRVYR + URXUCR + BMXBMI + sunscreen_usage_ordinal + INDFMPIR")

vector_covariates <- c("race"
                       , "RIDAGEYR" 
                       , "URXUCR"  
                       , "BMXBMI"
                       , "INDFMPIR"
                       , "SDDSRVYR"
                       , "weight_perception"
                       , "race_weight_perception"
                       , "sunscreen_usage_ordinal")

df_regression_stats <- run_regression_models(df_nhanes = df_merge
                                             , covariates = vector_covariates
                                             , chemical = chemical_biomarker
                                             , regression_formulas = vector_regression_models)
