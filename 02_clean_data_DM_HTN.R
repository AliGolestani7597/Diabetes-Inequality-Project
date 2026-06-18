# Data preparation for the diabetes and hypertension inequality analyses.
#
# Raw survey files are not distributed with this repository. By default, place
# them in data/raw/. Alternatively, set the STEPS_DATA_DIR environment variable
# to the directory containing the two files before running this script.

library(haven)
library(dplyr)
library(survey)
library(openxlsx)

data_dir <- Sys.getenv("STEPS_DATA_DIR", unset = file.path("data", "raw"))
derived_data_dir <- file.path("data", "derived")
dir.create(derived_data_dir, recursive = TRUE, showWarnings = FALSE)

steps1400 <- read_dta(file.path(data_dir, "steps_2020_7.11.2021_01_p3_final_4.1.2.dta"))
steps1395 <- read_dta(file.path(data_dir, "steps_data_main_20171217.dta"))

#year
steps1395$year <- 1395
steps1400$year <- 1400
# c1 is sex in both
steps1395$sex <- steps1395$c1
steps1400$sex <- steps1400$c1
# age in 1400 and c3y in 1395
steps1395$age <- steps1395$c3y
# area in both
# i07 is province in both
steps1395$province <- steps1395$i07
steps1400$province <- steps1400$i07
# WI and WI_National are in both
steps1395$WI_ranked <- ifelse(is.na(steps1395$WI) , NA,
                              ifelse(steps1395$WI == 5,1,
                                     ifelse(steps1395$WI == 4,2,
                                            ifelse(steps1395$WI == 3,3,
                                                   ifelse(steps1395$WI == 2,4,5)))))
steps1400$WI_ranked <- ifelse(is.na(steps1400$WI) , NA,
                              ifelse(steps1400$WI == 5,1,
                                     ifelse(steps1400$WI == 4,2,
                                            ifelse(steps1400$WI == 3,3,
                                                   ifelse(steps1400$WI == 2,4,5)))))
steps1395$WI_National_ranked <- ifelse(is.na(steps1395$WI_National) , NA,
                              ifelse(steps1395$WI_National == 5,1,
                                     ifelse(steps1395$WI_National == 4,2,
                                            ifelse(steps1395$WI_National == 3,3,
                                                   ifelse(steps1395$WI_National == 2,4,5)))))
steps1400$WI_National_ranked <- ifelse(is.na(steps1400$WI_National) , NA,
                              ifelse(steps1400$WI_National == 5,1,
                                     ifelse(steps1400$WI_National == 4,2,
                                            ifelse(steps1400$WI_National == 3,3,
                                                   ifelse(steps1400$WI_National == 2,4,5)))))
# i20 is similar for education
steps1400$education <- steps1400$i20
#steps1395$education <- steps1395$i20

steps1395$education <-ifelse(is.na(steps1395$i22) , NA,
                               ifelse(steps1395$i22 == 0,0,
                                ifelse(steps1395$i22 >0 & steps1395$i22 <7,1,
                                        ifelse(steps1395$i22 >6 & steps1395$i22 <12,2,3))))

steps1400$education_ranked <- ifelse(is.na(steps1400$education) , NA,
                                                            ifelse(steps1400$education == 3,1,
                                                                   ifelse(steps1400$education == 2,2,
                                                                          ifelse(steps1400$education == 1,3,4))))
steps1395$education_ranked <- ifelse(is.na(steps1395$education) , NA,
                                     ifelse(steps1395$education == 3,1,
                                            ifelse(steps1395$education == 2,2,
                                                   ifelse(steps1395$education == 1,3,4))))
# marriagestatus in 1400 and i19
steps1395$marriagestatus <- steps1395$i19 - 1
#Labels:
#  value                         label
#0                        Single
#1                       Married
#2 Divorced/seprate with partner
#3                         Widow

#Weights
steps1395$W_Anthropometry <- steps1395$weight_antropo
steps1395$W_Questionnaire <- steps1395$weight_Q
steps1395$W_Laboratory <- steps1395$weight_lab


#HTN cascade of care 1400
steps1400$taking_htn_drug <- steps1400$h3c 
steps1400[(steps1400$taking_htn_drug %in% c(-555)),]$taking_htn_drug <- 0
steps1400$MeanSys_ag <- steps1400$MeanSys
steps1400$MeanDias_ag <- steps1400$MeanDias
steps1400$HTN <- ifelse((is.na(steps1400$MeanSys_ag) | is.na(steps1400$MeanDias_ag)),
                        NA,
                        ifelse(steps1400$MeanSys_ag >= 140 | steps1400$MeanDias_ag >= 90 | steps1400$taking_htn_drug == 1,1,0))

steps1400$ever_told_HTN <- steps1400$h2e 
steps1400[(steps1400$ever_told_HTN %in% c(-555)),]$ever_told_HTN <- 0
steps1400$HTN_detection <- ifelse((is.na(steps1400$HTN) | steps1400$HTN == 0),
                                  NA,
                                  ifelse(steps1400$ever_told_HTN == 1,1,0))

steps1400$HTN_treatment <- ifelse((is.na(steps1400$HTN) | steps1400$HTN == 0),
                                  NA,
                                  ifelse(steps1400$taking_htn_drug == 1,1,0))

steps1400$HTN_control <- ifelse((is.na(steps1400$HTN) | steps1400$HTN == 0),
                                  NA,
                                  ifelse(steps1400$MeanSys < 140 & steps1400$MeanDias < 90,1,0))


#Diabetes cascade of care 1400
#steps1400$FBS <- ifelse(steps1400$GLUC3 >600 | steps1400$GLUC3 <20, NA,steps1400$GLUC3)
steps1400$FBS <- steps1400$GLUC3
steps1400$taking_dm_drug <- steps1400$h88ma
steps1400[(steps1400$taking_dm_drug %in% c(-555)),]$taking_dm_drug <- 0
steps1400$DM <- ifelse(is.na(steps1400$FBS),
                       NA,
                       ifelse(steps1400$FBS >= 126 |  steps1400$taking_dm_drug == 1,1,0))

steps1400$ever_told_dm <- steps1400$x9 
steps1400[(steps1400$ever_told_dm %in% c(-555)),]$ever_told_dm <- 0
steps1400$DM_detection <- ifelse((is.na(steps1400$DM) | steps1400$DM == 0),
                                  NA,
                                  ifelse(steps1400$ever_told_dm == 1,1,0))

steps1400$DM_treatment <- ifelse((is.na(steps1400$DM) | steps1400$DM == 0),
                                  NA,
                                  ifelse(steps1400$taking_dm_drug == 1,1,0))

steps1400$DM_control_good <- ifelse((is.na(steps1400$DM) | steps1400$DM == 0),
                                  NA,
                                  ifelse(steps1400$HbA1C < 7,1,0))

steps1400$DM_control_fair <- ifelse((is.na(steps1400$DM) | steps1400$DM == 0),
                                    NA,
                                    ifelse(steps1400$HbA1C < 8,1,0))


#HTN cascade of care 1395
steps1395$m11b_checker <- ifelse(is.na(steps1395$m11b), FALSE, TRUE)
steps1395[(steps1395$m11b_checker == TRUE) & (steps1395$m11b > 200),]$m11b <- NA

steps1395$m12b_checker <- ifelse(is.na(steps1395$m12b), FALSE, TRUE)
steps1395[(steps1395$m12b_checker == TRUE) & (steps1395$m12b > 200),]$m12b <- NA


BPmean <- function(rec1, rec2, rec3){
  if (!is.na(rec2) & !is.na(rec3)){
    return ((rec2+rec3)/2)
  } else if (!is.na(rec1) & is.na(rec3) & !is.na(rec2)) {
    return(rec2)
  } else if (!is.na(rec1) & is.na(rec2) & !is.na(rec3)){
    return(rec3)
  } else if (sum(is.na(c(rec1, rec2, rec3))) == 2) {
    return(sum(c(rec1,rec2,rec3), na.rm = TRUE))
  } else {
    return (NA)
  }
}

steps1395$MeanSys_ag <- apply(steps1395[,c('m11a', 'm12a', 'm13a')], 1, function(x) BPmean(x[1], x[2], x[3]))
steps1395$MeanDias_ag <- apply(steps1395[,c('m11b', 'm12b', 'm13b')], 1, function(x) BPmean(x[1], x[2], x[3]))


jump_modifier <- function(jump_var, var){
  if (is.na(jump_var)){
    return(var)
  } else if (jump_var == 0 & is.na(var)){
    return(0)
  } else {
    return(var)
  }
}

steps1395$h3c_ag <- apply(steps1395[,c('h2y','h3c')], 1, function(x) jump_modifier(x[1],x[2]))
steps1395$h3a_ag <- apply(steps1395[,c('h2y','h3a')], 1, function(x) jump_modifier(x[1],x[2]))
steps1395$taking_htn_drug <- ifelse(steps1395$h3c_ag ==1 | steps1395$h3a_ag  == 1, 1,0)
steps1395$h2e_ag <- apply(steps1395[,c('h0e','h2e')], 1, function(x) jump_modifier(x[1],x[2]))
steps1395$ever_told_HTN <- ifelse(steps1395$h2e_ag ==1 | steps1395$h2y  == 1, 1,0)

steps1395$HTN <- ifelse((is.na(steps1395$MeanSys_ag) | is.na(steps1395$MeanDias_ag)),
                        NA,
                        ifelse(steps1395$MeanSys_ag >= 140 | steps1395$MeanDias_ag >= 90 | steps1395$taking_htn_drug == 1,1,0))

steps1395$HTN_detection <- ifelse((is.na(steps1395$HTN) | steps1395$HTN == 0),
                                  NA,
                                  ifelse(steps1395$ever_told_HTN == 1,1,0))

steps1395$HTN_treatment <- ifelse((is.na(steps1395$HTN) | steps1395$HTN == 0),
                                  NA,
                                  ifelse(steps1395$taking_htn_drug == 1,1,0))

steps1395$HTN_control <- ifelse((is.na(steps1395$HTN) | steps1395$HTN == 0),
                                NA,
                                ifelse(steps1395$MeanSys_ag < 140 & steps1395$MeanDias_ag < 90,1,0))

#DM cascade of care 1395
steps1395$FBS <- steps1395$GLUC3
steps1395$HbA1C <- steps1395$HbA1c
steps1395$h8a_ag <- apply(steps1395[,c('h7c','h8a')], 1, function(x) jump_modifier(x[1],x[2]))
steps1395$h8b_ag <- apply(steps1395[,c('h7c','h8b')], 1, function(x) jump_modifier(x[1],x[2]))
steps1395$h88_ag <- apply(steps1395[,c('h7c','h88')], 1, function(x) jump_modifier(x[1],x[2]))
steps1395$taking_dm_drug <- ifelse(steps1395$h8a_ag ==1 | steps1395$h8b_ag  == 1 | steps1395$h88_ag  == 1, 1,0)
steps1395$x9_ag <- apply(steps1395[,c('h6','x9')], 1, function(x) jump_modifier(x[1],x[2]))
steps1395$ever_told_dm <- ifelse(steps1395$h7c ==1 | steps1395$x9_ag  == 1, 1,0)

steps1395$DM <- ifelse(is.na(steps1395$FBS),
                       NA,
                       ifelse(steps1395$FBS >= 126 |  steps1395$taking_dm_drug == 1,1,0))

steps1395$DM_detection <- ifelse((is.na(steps1395$DM) | steps1395$DM == 0),
                                 NA,
                                 ifelse(steps1395$ever_told_dm == 1,1,0))

steps1395$DM_treatment <- ifelse((is.na(steps1395$DM) | steps1395$DM == 0),
                                 NA,
                                 ifelse(steps1395$taking_dm_drug == 1,1,0))

steps1395$DM_control_good <- ifelse((is.na(steps1395$DM) | steps1395$DM == 0),
                                    NA,
                                    ifelse(steps1395$HbA1C < 7,1,0))

steps1395$DM_control_fair <- ifelse((is.na(steps1395$DM) | steps1395$DM == 0),
                                    NA,
                                    ifelse(steps1395$HbA1C < 8,1,0))


###
steps1395_inequality <- steps1395[,c('year', 'age', 'sex', 'area', 'province', 'WI', 'WI_ranked', 'WI_National',
                                     'WI_National_ranked', 'education', 'education_ranked', 'marriagestatus',
                                     'taking_htn_drug', 'ever_told_HTN', 'MeanSys_ag', 'MeanDias_ag', 'HTN',
                                     'HTN_detection', 'HTN_treatment', 'HTN_control', 'taking_dm_drug',
                                     'ever_told_dm', 'FBS', 'HbA1C', 'DM', 'DM_detection', 'DM_treatment',
                                     'DM_control_good', 'DM_control_fair', 'W_Questionnaire', 'W_Anthropometry',
                                     'W_Laboratory')]

steps1400_inequality <- steps1400[,c('year', 'age', 'sex', 'area', 'province', 'WI', 'WI_ranked', 'WI_National',
                                     'WI_National_ranked', 'education', 'education_ranked', 'marriagestatus',
                                     'taking_htn_drug', 'ever_told_HTN', 'MeanSys_ag', 'MeanDias_ag', 'HTN',
                                     'HTN_detection', 'HTN_treatment', 'HTN_control', 'taking_dm_drug',
                                     'ever_told_dm', 'FBS', 'HbA1C', 'DM', 'DM_detection', 'DM_treatment',
                                     'DM_control_good', 'DM_control_fair', 'W_Questionnaire', 'W_Anthropometry',
                                     'W_Laboratory')]



steps_1395_1400_dm_htn_inequality <- rbind(steps1395_inequality, steps1400_inequality)
write_dta(
  steps_1395_1400_dm_htn_inequality,
  file.path(derived_data_dir, "steps_1395_1400_dm_htn_inequality.dta")
)

