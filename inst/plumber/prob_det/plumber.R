library(plumber)

#* @apiTitle Probability of detection given presence

#* Return the probability of detection given presence based on a specified number of surveys
#* @param species_code
#* @param survey_length
#* @param number_of_surveys
#* @post /prob_det
wildRtrax::wt_prob_det
