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
