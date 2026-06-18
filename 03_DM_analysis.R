# Primary analysis: inequalities in the diabetes care cascade.
# Run from the repository root after 00, 01, and 02 (see run_analysis.R).

dm_output_dir <- file.path("results", "DM")
dir.create(dm_output_dir, recursive = TRUE, showWarnings = FALSE)

dm_df <- subset(steps_1395_1400_dm_htn_inequality, !is.na(DM) & !(is.na(education) & is.na(WI_National)) & age >= 25)
dm_df$id <- 1:nrow(dm_df)


age_intervals <- c(25,40,50,60,70,Inf)
category_labels <- c("25-39", "40-49", "50-59", "60-69",'>= 70')
dm_df$age_category <- cut(dm_df$age, age_intervals, labels = seq_along(category_labels) - 1, right = FALSE)

dm_df$age_cat_m <- cut(dm_df$age, breaks = c(25,30,35,40,45,50,55,60,65,70,Inf), 
                               labels = seq_along(c("25-29","30-34",
                                          "35-39" , "40-44",'45-49','50-54','55-59',
                                          '60-64','65-69','>=70'))-1,
                               include.lowest = TRUE, right = FALSE)


dm_df$year_cat <- ifelse(dm_df$year == 1395, 0,1)

dm_df$marriagestatus <- ifelse(dm_df$marriagestatus == 3, 2, dm_df$marriagestatus)

dm_df$WI_National_ranked <- dm_df$WI_National_ranked -1 
dm_df$education_ranked <- dm_df$education_ranked -1 

categorical_variables_to_check <- c('age_category', 'sex', 'area', 'marriagestatus',
                                    'WI_National_ranked', 'education_ranked',
                                    'DM', 'DM_detection', 'DM_treatment', 'DM_control_good', 'DM_control_fair'
                                    )

dm_df$age_category <- factor(dm_df$age_category)
dm_df$sex <- factor(dm_df$sex)
dm_df$area <- factor(dm_df$area)
dm_df$marriagestatus <- factor(dm_df$marriagestatus)
dm_df$WI_National_ranked <- factor(dm_df$WI_National_ranked)
dm_df$education_ranked <- factor(dm_df$education_ranked)
dm_df$DM <- factor(dm_df$DM)
dm_df$DM_detection <- factor(dm_df$DM_detection)
dm_df$DM_treatment <- factor(dm_df$DM_treatment)
dm_df$DM_control_good <- factor(dm_df$DM_control_good)
dm_df$DM_control_fair <- factor(dm_df$DM_control_fair)


for (var in categorical_variables_to_check) {
  if (is.factor(dm_df)) {
    if (any(table(dm_df[[var]]) == 0)) {
      dm_df[[var]] <- droplevels(dm_df[[var]]) 
    }
  }
}


label_mappings <- list(
  age_category = c("25-39", "40-49", "50-59", "60-69",'>= 70'),
  sex = c( 'Female','Male'),
  area = c("Rural", "Urban"), 
  marriagestatus = c("Single", "Married", "Divorced/widow"),
  education_ranked = c( ">=12", "7-11","1-6", "0"),
  WI_National_ranked = c("1 (wealthiest)", "2", "3", "4", "5 (poorest)"),
  DM = c('No', 'Yes'),
  DM_detection =  c('No', 'Yes'),
  DM_treatment = c('No', 'Yes'),
  DM_control_good = c('No', 'Yes'),
  DM_control_fair = c('No', 'Yes')
)


label_df <- do.call(rbind, lapply(names(label_mappings), function(var) {
  data.frame(
    Variable = var,
    Category = label_mappings[[var]],
    Sublevel = seq_along(label_mappings[[var]]) - 1
  )
}))


######
dm_design_lab <-
  svydesign(
    weights = dm_df$W_Laboratory,
    data = dm_df,
    id = dm_df$id
  )

dm_design_quest <-
  svydesign(
    weights = dm_df$W_Questionnaire,
    data = dm_df,
    id = dm_df$id
  )

#####

variable_list_table1_ques <- c('sex', 'age_category','area','marriagestatus','education_ranked', 'WI_National_ranked')
variable_list_table1_dm <- c('DM', 'DM_detection','DM_treatment','DM_control_good','DM_control_fair')

baseline_1395_ques <- baseline_table_final_process_withoutCI(subset(dm_df, year_cat==0), i, subset(dm_design_quest, year_cat==0), 1, variable_list_table1_ques, 'questionnaire_1395')
baseline_1395_dm <- baseline_table_final_process(subset(dm_df, year_cat==0), i, subset(dm_design_lab, year_cat==0), 1, variable_list_table1_dm, 'diabetes_1395')

baseline_1400_ques <- baseline_table_final_process_withoutCI(subset(dm_df, year_cat==1), i, subset(dm_design_quest, year_cat==1), 1, variable_list_table1_ques, 'questionnaire_1400')
baseline_1400_dm <- baseline_table_final_process(subset(dm_df, year_cat==1), i, subset(dm_design_lab, year_cat==1), 1, variable_list_table1_dm, 'diabetes_1400')


baseline_1395_ques$RowOrder <- seq_len(nrow(baseline_1395_ques))
baseline_1395_dm$RowOrder <- seq_len(nrow(baseline_1395_dm))
baseline_dm <- merge(baseline_1395_dm, baseline_1400_dm, by = c("Variable", "Category"), all.x = TRUE)
baseline_ques <- merge(baseline_1395_ques, baseline_1400_ques, by = c("Variable", "Category"), all.x = TRUE)
baseline_dm <- baseline_dm[order(baseline_dm$RowOrder), ]
baseline_ques <- baseline_ques[order(baseline_ques$RowOrder), ]
baseline_dm$RowOrder <- NULL
baseline_ques$RowOrder <- NULL
write.xlsx(baseline_dm, file.path(dm_output_dir, "DM_cascade_table.xlsx"))
write.xlsx(baseline_ques, file.path(dm_output_dir, "Baseline_table.xlsx"))

#
descriptive_variables <- c( 'education_ranked', 'WI_National_ranked')

DM_1395 <- process_descriptive_table(label_df, descriptive_variables, 'DM', subset(dm_df, year_cat==0), 2, subset(dm_design_lab, year_cat==0))
DM_1395$year <- 1395
DM_1400 <- process_descriptive_table(label_df, descriptive_variables, 'DM', subset(dm_df, year_cat==1), 2, subset(dm_design_lab, year_cat==1))
DM_1400$year <- 1400
write.xlsx(rbind(DM_1395, DM_1400), file.path(dm_output_dir, "DM_descriptive.xlsx"))

DM_detection_1395 <- process_descriptive_table(label_df, descriptive_variables, 'DM_detection', subset(dm_df, year_cat==0), 2, subset(dm_design_lab, year_cat==0))
DM_detection_1395$year <- 1395
DM_detection_1400 <- process_descriptive_table(label_df, descriptive_variables, 'DM_detection', subset(dm_df, year_cat==1), 2, subset(dm_design_lab, year_cat==1))
DM_detection_1400$year <- 1400
write.xlsx(rbind(DM_detection_1395, DM_detection_1400), file.path(dm_output_dir, "DM_detection_descriptive.xlsx"))

DM_treatment_1395 <- process_descriptive_table(label_df, descriptive_variables, 'DM_treatment', subset(dm_df, year_cat==0), 2, subset(dm_design_lab, year_cat==0))
DM_treatment_1395$year <- 1395
DM_treatment_1400 <- process_descriptive_table(label_df, descriptive_variables, 'DM_treatment', subset(dm_df, year_cat==1), 2, subset(dm_design_lab, year_cat==1))
DM_treatment_1400$year <- 1400
write.xlsx(rbind(DM_treatment_1395, DM_treatment_1400), file.path(dm_output_dir, "DM_treatment_descriptive.xlsx"))

DM_control_good_1395 <- process_descriptive_table(label_df, descriptive_variables, 'DM_control_good', subset(dm_df, year_cat==0), 2, subset(dm_design_lab, year_cat==0))
DM_control_good_1395$year <- 1395
DM_control_good_1400 <- process_descriptive_table(label_df, descriptive_variables, 'DM_control_good', subset(dm_df, year_cat==1), 2, subset(dm_design_lab, year_cat==1))
DM_control_good_1400$year <- 1400
write.xlsx(rbind(DM_control_good_1395, DM_control_good_1400), file.path(dm_output_dir, "DM_control_good_descriptive.xlsx"))

DM_control_fair_1395 <- process_descriptive_table(label_df, descriptive_variables, 'DM_control_fair', subset(dm_df, year_cat==0), 2, subset(dm_design_lab, year_cat==0))
DM_control_fair_1395$year <- 1395
DM_control_fair_1400 <- process_descriptive_table(label_df, descriptive_variables, 'DM_control_fair', subset(dm_df, year_cat==1), 2, subset(dm_design_lab, year_cat==1))
DM_control_fair_1400$year <- 1400
write.xlsx(rbind(DM_control_fair_1395, DM_control_fair_1400), file.path(dm_output_dir, "DM_control_fair_descriptive.xlsx"))

####
age_sex_adjusted_df <- data.frame()
for (outcome in c("DM", "DM_detection", "DM_treatment", "DM_control_good", "DM_control_fair")) {
  for (year_ in c(1395, 1400)) {
    temp_result <- age_sex_adjusted_prevalence_maker(outcome, 'age + sex', descriptive_variables, subset(dm_design_lab, year == year_))
    temp_result$year <- year_
    age_sex_adjusted_df <- rbind(age_sex_adjusted_df,temp_result)
  }
}
write.xlsx(age_sex_adjusted_df, file.path(dm_output_dir, "age_sex_adjusted_prevalences.xlsx"))


###
sii_binomial_final <- data.frame()
for (ranks in c('WI_National_ranked', 'education_ranked')) {
  for (j in c(1395, 1400)) {
    for (i in c('DM', 'DM_detection', 'DM_treatment', 'DM_control_good', 'DM_control_fair')) {
      for (adj in list(NULL, 
                       if (ranks == 'WI_National_ranked') 'age + sex' else 'age + sex', 
                       if (ranks == 'WI_National_ranked') 'age + sex + area + education_ranked + marriagestatus' 
                       else 'age + sex + area + WI_National_ranked + marriagestatus')) {
          sii_df <- sii_modified_binom(subset(dm_df, year==j), i, ranks, adjust = adj, pop = NULL, weight = 'W_Laboratory', 
                                       conf.level = 0.95, linear = FALSE)
          sii_df$year <- j
          sii_df$Rank <- ranks
          sii_binomial_final <- rbind(sii_binomial_final,sii_df)
        }
      }
    }
  }
write.xlsx(sii_binomial_final, file.path(dm_output_dir, "sii_binomial.xlsx"))

sii_poisson_final <- data.frame()
for (ranks in c('WI_National_ranked', 'education_ranked')) {
  for (j in c(1395, 1400)) {
    for (i in c('DM', 'DM_detection', 'DM_treatment', 'DM_control_good', 'DM_control_fair')) {
      for (adj in list(NULL, 
                       if (ranks == 'WI_National_ranked') 'age + sex' else 'age + sex', 
                       if (ranks == 'WI_National_ranked') 'age + sex + area + education_ranked + marriagestatus' 
                       else 'age + sex + area + WI_National_ranked + marriagestatus')) {
        sii_df <- sii_modified_poisson(subset(dm_df, year==j), i, ranks, adjust = adj, pop = NULL, weight = 'W_Laboratory', 
                                     conf.level = 0.95, linear = FALSE)
        sii_df$year <- j
        sii_df$Rank <- ranks
        sii_poisson_final <- rbind(sii_poisson_final,sii_df)
      }
    }
  }
}
write.xlsx(sii_poisson_final, file.path(dm_output_dir, "sii_poisson.xlsx"))

rii_binomial_final <- data.frame()
for (ranks in c('WI_National_ranked', 'education_ranked')) {
  for (j in c(1395, 1400)) {
    for (i in c('DM', 'DM_detection', 'DM_treatment', 'DM_control_good', 'DM_control_fair')) {
      for (adj in list(NULL, 
                       if (ranks == 'WI_National_ranked') 'age + sex' else 'age + sex', 
                       if (ranks == 'WI_National_ranked') 'age + sex + area + education_ranked + marriagestatus' 
                       else 'age + sex + area + WI_National_ranked + marriagestatus')) {
        sii_df <- rii_modified_binom(subset(dm_df, year==j), i, ranks, adjust = adj, pop = NULL, weight = 'W_Laboratory', 
                                     conf.level = 0.95, linear = FALSE)
        sii_df$year <- j
        sii_df$Rank <- ranks
        rii_binomial_final <- rbind(rii_binomial_final,sii_df)
      }
    }
  }
}
write.xlsx(rii_binomial_final, file.path(dm_output_dir, "rii_binomial.xlsx"))

rii_poisson_final <- data.frame()
for (ranks in c('WI_National_ranked', 'education_ranked')) {
  for (j in c(1395, 1400)) {
    for (i in c('DM', 'DM_detection', 'DM_treatment', 'DM_control_good', 'DM_control_fair')) {
      for (adj in list(NULL, 
                       if (ranks == 'WI_National_ranked') 'age + sex' else 'age + sex', 
                       if (ranks == 'WI_National_ranked') 'age + sex + area + education_ranked + marriagestatus' 
                       else 'age + sex + area + WI_National_ranked + marriagestatus')) {
        sii_df <- rii_modified_poisson(subset(dm_df, year==j), i, ranks, adjust = adj, pop = NULL, weight = 'W_Laboratory', 
                                       conf.level = 0.95, linear = FALSE)
        sii_df$year <- j
        sii_df$Rank <- ranks
        rii_poisson_final <- rbind(rii_poisson_final,sii_df)
      }
    }
  }
}
write.xlsx(rii_poisson_final, file.path(dm_output_dir, "rii_poisson.xlsx"))


pval_interaction_WI_df <- data.frame()
for (outcome in c('DM', 'DM_detection', 'DM_treatment', 'DM_control_good', 'DM_control_fair')){
  df1 <- df_rank_maker(subset(dm_df,year == 1395), outcome, 'WI_National_ranked',  pop = NULL, weight = 'W_Laboratory') 
  df2 <- df_rank_maker(subset(dm_df,year == 1400), outcome, 'WI_National_ranked',  pop = NULL, weight = 'W_Laboratory')
  df <- rbind(df1,df2)
  model1 <- svyglm(as.formula(paste(outcome, "~ Xrank_WI_National_ranked + year_cat + Xrank_WI_National_ranked:year_cat")),
                   design = svydesign(weights = ~W_Laboratory, data = df, id = ~1),
                   family = quasibinomial(link = "logit"))
  pval_interaction1 <- round(summary(model1)$coefficients["Xrank_WI_National_ranked:year_cat", "Pr(>|t|)"],3)

  model2 <- svyglm(as.formula(paste(outcome, "~ Xrank_WI_National_ranked + year_cat + Xrank_WI_National_ranked:year_cat + age + sex")),
                   design = svydesign(weights = ~W_Laboratory, data = df, id = ~1),
                   family = quasibinomial(link = "logit"))
  pval_interaction2 <- round(summary(model2)$coefficients["Xrank_WI_National_ranked:year_cat", "Pr(>|t|)"],3)
  
  model3 <- svyglm(as.formula(paste(outcome, "~ Xrank_WI_National_ranked + year_cat + Xrank_WI_National_ranked:year_cat + age + sex  + area + education_ranked + marriagestatus")),
                   design = svydesign(weights = ~W_Laboratory, data = df, id = ~1),
                   family = quasibinomial(link = "logit"))
  pval_interaction3 <- round(summary(model3)$coefficients["Xrank_WI_National_ranked:year_cat", "Pr(>|t|)"],3)
  
  pval_interaction_WI_df <- rbind( pval_interaction_WI_df, data.frame(variable = outcome, p_value_model1 = pval_interaction1 ,p_value_model2= pval_interaction2 ,p_value_model3=pval_interaction3))
}
write.xlsx(pval_interaction_WI_df, file.path(dm_output_dir, "p_value_interaction_WI.xlsx"))


pval_interaction_edu_df <- data.frame()
for (outcome in c('DM', 'DM_detection', 'DM_treatment', 'DM_control_good', 'DM_control_fair')){
  df1 <- df_rank_maker(subset(dm_df,year == 1395), outcome, 'education_ranked',  pop = NULL, weight = 'W_Laboratory') 
  df2 <- df_rank_maker(subset(dm_df,year == 1400), outcome, 'education_ranked',  pop = NULL, weight = 'W_Laboratory')
  df <- rbind(df1,df2)
  model1 <- svyglm(as.formula(paste(outcome, "~ Xrank_education_ranked + year_cat + Xrank_education_ranked:year_cat")),
                   design = svydesign(weights = ~W_Laboratory, data = df, id = ~1),
                   family = quasibinomial(link = "logit"))
  pval_interaction1 <- round(summary(model1)$coefficients["Xrank_education_ranked:year_cat", "Pr(>|t|)"],3)
  
  model2 <- svyglm(as.formula(paste(outcome, "~ Xrank_education_ranked + year_cat + Xrank_education_ranked:year_cat + age + sex")),
                   design = svydesign(weights = ~W_Laboratory, data = df, id = ~1),
                   family = quasibinomial(link = "logit"))
  pval_interaction2 <- round(summary(model2)$coefficients["Xrank_education_ranked:year_cat", "Pr(>|t|)"],3)
  
  model3 <- svyglm(as.formula(paste(outcome, "~ Xrank_education_ranked + year_cat + Xrank_education_ranked:year_cat + age + sex  + area + WI_National_ranked + marriagestatus")),
                   design = svydesign(weights = ~W_Laboratory, data = df, id = ~1),
                   family = quasibinomial(link = "logit"))
  pval_interaction3 <- round(summary(model3)$coefficients["Xrank_education_ranked:year_cat", "Pr(>|t|)"],3)
  
  pval_interaction_edu_df <- rbind( pval_interaction_edu_df, data.frame(variable = outcome, p_value_model1 = pval_interaction1 ,p_value_model2= pval_interaction2 ,p_value_model3=pval_interaction3))
}
write.xlsx(pval_interaction_edu_df, file.path(dm_output_dir, "p_value_interaction_edu.xlsx"))

