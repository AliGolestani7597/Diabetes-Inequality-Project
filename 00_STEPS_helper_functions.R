# General helper functions developed for analyses of WHO STEPS survey data.
# Function bodies are retained from the original research workflow.

library(haven)
library(nrba)
library(survey)
library(jtools)
library(dplyr)
library(readxl)
library(patchwork)
library(tableone)
library(tidyr)
library(dplyr)
library(car)
library(reshape2)
library(openxlsx)

#Baseline Table
baseline_table_final_process <- function(data, var_name, survey_design, n_round, variable_list, report_variable){
  
  baseline_table_maker <- function(data, var_name, survey_design, n_round) {
    freqs <- table(data[[var_name]], useNA = 'ifany')
    freqs_df <- data.frame(
      Variable = var_name,                  
      Sublevel = names(freqs),              
      Number = as.numeric(freqs)            
    )
    
    total_df <- data.frame()
    levels_var <- levels(factor(data[[var_name]]))
    for (lvl in levels_var) {
      binary_formula <- as.formula(paste0("~I(factor(", var_name, ") == '", lvl, "')"))
      prop_result <- svyciprop(binary_formula, survey_design, method = "logit")
      ci <- confint(prop_result)
      total_df <- rbind(total_df, data.frame(
        Variable = var_name,
        Sublevel = lvl,
        weighted_prevalence = round(coef(prop_result) * 100, n_round),
        Lower_CI = round(ci[1] * 100, n_round),
        Upper_CI = round(ci[2] * 100, n_round)
      ))
    }
    
    final_table <- merge(freqs_df, total_df, by = c("Variable", "Sublevel"), all = TRUE)
    return(final_table)
  }
  
  baseline_df <- data.frame()
  for (i in variable_list) {
    df_int <- baseline_table_maker(data, i, survey_design, 2)
    baseline_df <- rbind(baseline_df, df_int)
  }
  baseline_df$RowOrder <- seq_len(nrow(baseline_df))
  baseline_df_f <- merge(baseline_df, label_df, by = c("Variable", "Sublevel"), all.x = TRUE)
  baseline_df_f <- baseline_df_f[order(baseline_df_f$RowOrder), ]
  baseline_df_f$RowOrder <- NULL
  baseline_df_f <- baseline_df_f[c('Variable', 'Sublevel', 'Category', 'Number', 'weighted_prevalence', 'Lower_CI', 'Upper_CI')]
  baseline_df_f[[report_variable]] <- paste(baseline_df_f$Number, " (", baseline_df_f$weighted_prevalence, "; ",
                                baseline_df_f$Lower_CI , "-", baseline_df_f$Upper_CI, ")",sep = "")
  baseline_df_f <- baseline_df_f[c('Variable', 'Category', report_variable)]
  return(baseline_df_f)
}

baseline_table_final_process_withoutCI <- function(data, var_name, survey_design, n_round, variable_list, report_variable){
  
  baseline_table_maker <- function(data, var_name, survey_design, n_round) {
    freqs <- table(data[[var_name]], useNA = 'ifany')
    freqs_df <- data.frame(
      Variable = var_name,                  
      Sublevel = names(freqs),              
      Number = as.numeric(freqs)            
    )
    
    total_df <- data.frame()
    levels_var <- levels(factor(data[[var_name]]))
    for (lvl in levels_var) {
      binary_formula <- as.formula(paste0("~I(factor(", var_name, ") == '", lvl, "')"))
      prop_result <- svyciprop(binary_formula, survey_design, method = "logit")
      ci <- confint(prop_result)
      total_df <- rbind(total_df, data.frame(
        Variable = var_name,
        Sublevel = lvl,
        weighted_prevalence = round(coef(prop_result) * 100, n_round),
        Lower_CI = round(ci[1] * 100, n_round),
        Upper_CI = round(ci[2] * 100, n_round)
      ))
    }
    
    final_table <- merge(freqs_df, total_df, by = c("Variable", "Sublevel"), all = TRUE)
    return(final_table)
  }
  
  baseline_df <- data.frame()
  for (i in variable_list) {
    df_int <- baseline_table_maker(data, i, survey_design, n_round)
    baseline_df <- rbind(baseline_df, df_int)
  }
  baseline_df$RowOrder <- seq_len(nrow(baseline_df))
  baseline_df_f <- merge(baseline_df, label_df, by = c("Variable", "Sublevel"), all.x = TRUE)
  baseline_df_f <- baseline_df_f[order(baseline_df_f$RowOrder), ]
  baseline_df_f$RowOrder <- NULL
  baseline_df_f <- baseline_df_f[c('Variable', 'Sublevel', 'Category', 'Number', 'weighted_prevalence', 'Lower_CI', 'Upper_CI')]
  baseline_df_f[[report_variable]] <- paste(baseline_df_f$Number, " (", baseline_df_f$weighted_prevalence, ")",sep = "")
  baseline_df_f <- baseline_df_f[c('Variable', 'Category', report_variable)]
  return(baseline_df_f)
}


descriptive_table_maker <- function(variables, outcome, func, data, n_round, study_design){
  final_df <- data.frame()
  for (variable_name in variables) {
    summary_table <- func(outcome, variable_name, study_design, data, n_round)
    if (nrow(final_df) == 0) {
      final_df <- summary_table
    } else {
      final_df <- rbind(final_df, summary_table)
    }
  }
  return(final_df)
}


# Baseline table for continuous variables
baseline_table_continuous <- function(data, variable_list, survey_design, 
                                      n_round = 2, report_variable = "Mean (95% CI)") {
  
  results_df <- data.frame()
  
  for (var_name in variable_list) {
    
    # Remove missing for unweighted N
    n_unweighted <- sum(!is.na(data[[var_name]]))
    
    # Survey mean
    formula_mean <- as.formula(paste0("~", var_name))
    mean_result <- svymean(formula_mean, survey_design, na.rm = TRUE)
    
    # Confidence interval
    ci <- confint(mean_result)
    
    mean_value <- round(coef(mean_result)[1], n_round)
    lower_ci <- round(ci[1], n_round)
    upper_ci <- round(ci[2], n_round)
    
    temp_df <- data.frame(
      Variable = var_name,
      N = n_unweighted,
      Mean = mean_value,
      Lower_CI = lower_ci,
      Upper_CI = upper_ci
    )
    
    results_df <- rbind(results_df, temp_df)
  }
  
  # Format output
  results_df[[report_variable]] <- paste0(
    results_df$Mean, " (",
    results_df$Lower_CI, " - ",
    results_df$Upper_CI, ")"
  )
  
  results_df <- results_df[, c("Variable", "N", report_variable)]
  
  return(results_df)
}

baseline_table_final_process_prevalence <- function(data, var_name, survey_design, n_round, variable_list, report_variable){
  
  baseline_table_maker <- function(data, var_name, survey_design, n_round) {
    freqs <- table(data[[var_name]], useNA = 'ifany')
    freqs_df <- data.frame(
      Variable = var_name,                  
      Sublevel = names(freqs),              
      Number = as.numeric(freqs)            
    )
    
    total_df <- data.frame()
    levels_var <- levels(factor(data[[var_name]]))
    for (lvl in levels_var) {
      binary_formula <- as.formula(paste0("~I(factor(", var_name, ") == '", lvl, "')"))
      prop_result <- svyciprop(binary_formula, survey_design, method = "logit")
      ci <- confint(prop_result)
      total_df <- rbind(total_df, data.frame(
        Variable = var_name,
        Sublevel = lvl,
        weighted_prevalence = round(coef(prop_result) * 100, n_round),
        Lower_CI = round(ci[1] * 100, n_round),
        Upper_CI = round(ci[2] * 100, n_round)
      ))
    }
    
    final_table <- merge(freqs_df, total_df, by = c("Variable", "Sublevel"), all = TRUE)
    return(final_table)
  }
  
  baseline_df <- data.frame()
  for (i in variable_list) {
    df_int <- baseline_table_maker(data, i, survey_design, 2)
    baseline_df <- rbind(baseline_df, df_int)
  }
  baseline_df$RowOrder <- seq_len(nrow(baseline_df))
  baseline_df_f <- merge(baseline_df, label_df, by = c("Variable", "Sublevel"), all.x = TRUE)
  baseline_df_f <- baseline_df_f[order(baseline_df_f$RowOrder), ]
  baseline_df_f$RowOrder <- NULL
  baseline_df_f <- baseline_df_f[c('Variable', 'Sublevel', 'Category', 'Number', 'weighted_prevalence', 'Lower_CI', 'Upper_CI')]
  baseline_df_f[[report_variable]] <- paste(baseline_df_f$weighted_prevalence, " (",
                                            baseline_df_f$Lower_CI , "-", baseline_df_f$Upper_CI, ")",sep = "")
  baseline_df_f <- baseline_df_f[c('Variable', 'Category', report_variable)]
  return(baseline_df_f)
}


generate_prevalence_table <- function(outcome_name, var_name, survey_design, data, n_round = 2) {
  outcome_by_cat <- svyby(
    as.formula(paste0("~factor(", outcome_name, ")")), 
    as.formula(paste0("~factor(", var_name, ")")),  
    survey_design, 
    FUN = svymean, 
    na.rm = TRUE
  )
  
  levels_of_outcome <- length(names(table(data[[outcome_name]])))
  
  # Reshape point estimates to long format
  point_est <- outcome_by_cat[, 1:(levels_of_outcome + 1)]
  point_est_long <- point_est %>%
    pivot_longer(
      cols = starts_with(paste0("factor(", outcome_name, ")")),
      names_to = "outcome_level",
      values_to = "Value"
    ) %>%
    mutate(
      outcome_level = as.numeric(gsub(paste0("factor\\(", outcome_name, "\\)(\\d+)"), "\\1", outcome_level)),
      Value = round(Value * 100, n_round)
    ) %>%
    rename(variable_level = !!sym(paste0("factor(", var_name, ")")))
  
  
  CI_df <- data.frame()
  for (var_value in levels(factor(data[[var_name]]))) {
    for (lvl in 0:(levels_of_outcome - 1)) {
      suvset_term <- paste0(var_name, ' == ', var_value)
      binary_formula <- as.formula(paste0("~I(factor(", outcome_name, ") == '", lvl, "')"))
      ci_result <- svyciprop(binary_formula, subset(survey_design, eval(parse(text = suvset_term))), method = "logit")
      ci <- confint(ci_result)
      
      CI_df <- rbind(CI_df, data.frame(
        Variable = var_name,
        variable_level = var_value, 
        outcome = outcome_name, 
        outcome_level = lvl,
        lower_2.5 = round(ci[1] * 100, n_round),
        upper_97.5 = round(ci[2] * 100, n_round)
      ))
    }
  }
  
  # Merge point estimates and confidence intervals
  final_table <- merge(CI_df, point_est_long, by = c("variable_level", "outcome_level"), all = TRUE)
  final_table <- final_table[c('Variable', 'variable_level', 'outcome', 'outcome_level', 'Value', 'lower_2.5', 'upper_97.5')]
  
  p <- round(svychisq(as.formula(paste0("~", outcome_name, " + ", var_name)),  design = survey_design, statistic = "adjWald")$p.value, 4)
  if (p >= 0.0001) {
    if (p < 0.001) {
      p <- round(p, 4)
    } else {
      p <- round(p, 3)
    }
  } else {
    p <- '<0.0001'
  }
  final_table$p_value <- c(p, rep(NaN, length(unique(rownames(final_table))) - 1))
  return(final_table)
}

generate_summary_table_binary_quantitaive <- function(outcome_name, var_name, survey_design, data, n_round = 2) {
  is_categorical <- is.factor(data[[outcome_name]]) || is.character(data[[outcome_name]]) || all(na.omit(data[[outcome_name]]) %in% c(0, 1))
  
  if (is_categorical) {
    # Prevalence logic
    outcome_by_cat <- svyby(
      as.formula(paste0("~factor(", outcome_name, ")")), 
      as.formula(paste0("~factor(", var_name, ")")),  
      survey_design, 
      FUN = svymean, 
      na.rm = TRUE
    )
    
    levels_of_outcome <- length(names(table(data[[outcome_name]])))
    
    point_est <- outcome_by_cat[, 1:(levels_of_outcome + 1)]
    point_est_long <- point_est %>%
      pivot_longer(
        cols = starts_with(paste0("factor(", outcome_name, ")")),
        names_to = "outcome_level",
        values_to = "Value"
      ) %>%
      mutate(
        outcome_level = as.numeric(gsub(paste0("factor\\(", outcome_name, "\\)(\\d+)"), "\\1", outcome_level)),
        Value = round(Value * 100, n_round)
      ) %>%
      rename(variable_level = !!sym(paste0("factor(", var_name, ")")))
    
    CI_df <- data.frame()
    for (var_value in levels(factor(data[[var_name]]))) {
      for (lvl in 0:(levels_of_outcome - 1)) {
        suvset_term <- paste0(var_name, ' == ', var_value)
        binary_formula <- as.formula(paste0("~I(factor(", outcome_name, ") == '", lvl, "')"))
        ci_result <- svyciprop(binary_formula, subset(survey_design, eval(parse(text = suvset_term))), method = "logit")
        ci <- confint(ci_result)
        
        CI_df <- rbind(CI_df, data.frame(
          Variable = var_name,
          variable_level = var_value, 
          outcome = outcome_name, 
          outcome_level = lvl,
          lower_2.5 = round(ci[1] * 100, n_round),
          upper_97.5 = round(ci[2] * 100, n_round)
        ))
      }
    }
    
    final_table <- merge(CI_df, point_est_long, by = c("variable_level", "outcome_level"), all = TRUE)
    final_table <- final_table[c('Variable', 'variable_level', 'outcome', 'outcome_level', 'Value', 'lower_2.5', 'upper_97.5')]
    
    # p-value
    p <- round(svychisq(as.formula(paste0("~", outcome_name, " + ", var_name)),  design = survey_design, statistic = "adjWald")$p.value, 4)
  } else {
    # Quantitative outcome
    mean_df <- svyby(
      as.formula(paste0("~", outcome_name)),
      as.formula(paste0("~factor(", var_name, ")")),
      survey_design,
      svymean,
      na.rm = TRUE
    )
    
    mean_df <- mean_df %>%
      rename(
        variable_level = !!sym(paste0("factor(", var_name, ")")),
        Value = !!sym(outcome_name)
      )
    
    mean_df$Variable <- var_name
    mean_df$outcome <- outcome_name
    ci_mat <- confint(mean_df)
    mean_df$lower_2.5 <- round(ci_mat[, 1], n_round)
    mean_df$upper_97.5 <- round(ci_mat[, 2], n_round)
    mean_df$Value <- round(mean_df$Value, n_round)
    
    final_table <- mean_df[, c("Variable", "variable_level", "outcome", "Value", "lower_2.5", "upper_97.5")]
    
    # Step 3: Compute p-value
    n_levels <- length(unique(na.omit(data[[var_name]])))
    fmla <- as.formula(paste0(outcome_name, " ~ factor(", var_name, ")"))
    test_term <- as.formula(paste0("~factor(", var_name, ")"))
    
    if (n_levels == 2) {
      p <- round(svyttest(fmla, design = survey_design)$p.value, 4)
    } else {
      mod <- svyglm(fmla, design = survey_design)
      p <- round(regTermTest(mod, test_term)$p, 4)
    }
  }
  
  
  # Format p-value
  if (p >= 0.0001) {
    if (p < 0.001) {
      p <- round(p, 4)
    } else {
      p <- round(p, 3)
    }
  } else {
    p <- '<0.0001'
  }
  
  final_table$p_value <- c(p, rep(NaN, nrow(final_table) - 1))
  return(final_table)
}


process_descriptive_table_mean <- function(label_df, variables, outcome_variable, data, n_round, study_design) {
  descriptive_table <- descriptive_table_maker(variables, outcome_variable, generate_summary_table_binary_quantitaive,data, n_round, study_design)
  descriptive_table$RowOrder <- seq_len(nrow(descriptive_table))
  
  descriptive_table_f <- merge(
    descriptive_table,
    label_df[label_df$Variable != outcome_variable, ],
    by.x = c("Variable", "variable_level"),
    by.y = c("Variable", "Sublevel"),
    all.x = TRUE
  )
  
  descriptive_table_f <- descriptive_table_f[order(descriptive_table_f$RowOrder), ]
  descriptive_table_f$RowOrder <- NULL
  
  descriptive_table_f$report <- paste(
    descriptive_table_f$Value, " (",
    descriptive_table_f$lower_2.5, "-",
    descriptive_table_f$upper_97.5, ")", sep = ""
  )
  
  is_categorical <- is.factor(data[[outcome_variable]]) || is.character(data[[outcome_variable]]) || all(na.omit(data[[outcome_variable]]) %in% c(0, 1))
  
  if (is_categorical) {
    # Two outcome levels assumed (0/1)
    df1 <- descriptive_table_f[descriptive_table_f$outcome_level == 0, ][c('Variable', 'variable_level', 'Category', 'report', 'p_value')]
    df2 <- descriptive_table_f[descriptive_table_f$outcome_level == 1, ][c('Variable', 'variable_level', 'Category', 'report')]
    
    colnames(df1) <- c('Variable', 'variable_level', 'Category', 'No', 'p-value')
    colnames(df2) <- c('Variable', 'variable_level', 'Category', 'Yes')
    
    df1$RowOrder <- seq_len(nrow(df1))
    final_table <- merge(df1, df2, by = c("Variable", "variable_level", 'Category'), all = TRUE)
    final_table <- final_table[order(final_table$RowOrder), ]
    final_table$RowOrder <- NULL
    
    final_table <- final_table[c('Variable', 'variable_level', 'Category', 'No', 'Yes', 'p-value')]
  } else {
    # Quantitative outcome: just one line per variable_level
    final_table <- descriptive_table_f[, c("Variable", "variable_level", "Category", "report", "p_value")]
    colnames(final_table) <- c("Variable", "variable_level", "Category", "Mean (95% CI)", "p-value")
  }
  
  return(final_table)
}

process_descriptive_table <- function(label_df, variables, outcome_variable, data, n_round, study_design) {
  descriptive_table <- descriptive_table_maker(variables, outcome_variable, generate_prevalence_table, data, n_round, study_design)
  descriptive_table$RowOrder <- seq_len(nrow(descriptive_table))
  descriptive_table_f <- merge(
    descriptive_table,
    label_df[label_df$Variable != outcome_variable, ],
    by.x = c("Variable", "variable_level"),
    by.y = c("Variable", "Sublevel"),
    all.x = TRUE
  )
  
  descriptive_table_f <- descriptive_table_f[order(descriptive_table_f$RowOrder), ]
  descriptive_table_f$RowOrder <- NULL
  
  descriptive_table_f$report <- paste(
    descriptive_table_f$Value, " (",
    descriptive_table_f$lower_2.5, "-",
    descriptive_table_f$upper_97.5, ")", sep = ""
  )
  
  no_col_name <- paste('No ',outcome_variable)
  yes_col_name <- paste('Yes ',outcome_variable)
  p_value_col_name <- paste('p-value ',outcome_variable)
  df1 <- descriptive_table_f[descriptive_table_f$outcome_level == 0, ][c('Variable', 'variable_level', 'Category', 'report', 'p_value')]
  colnames(df1) <- c('Variable', 'variable_level', 'Category', no_col_name, p_value_col_name)
  df1$RowOrder <- seq_len(nrow(df1))
  
  df2 <- descriptive_table_f[descriptive_table_f$outcome_level == 1, ][c('Variable', 'variable_level', 'Category', 'report')]
  colnames(df2) <- c('Variable', 'variable_level', 'Category', yes_col_name)
  
  final_table <- merge(df1, df2, by = c("Variable", "variable_level", 'Category'), all = TRUE)
  
  final_table <- final_table[order(final_table$RowOrder), ]
  final_table$RowOrder <- NULL
  final_table <- final_table[c('Variable', 'variable_level', 'Category', no_col_name, yes_col_name, p_value_col_name)]
  
  return(final_table)
}


process_descriptive_table_plus2outcome <- function(label_df, variables, outcome_variable, data, n_round, study_design) {
  descriptive_table <- descriptive_table_maker(variables, outcome_variable, generate_prevalence_table, data, n_round, study_design)
  descriptive_table$RowOrder <- seq_len(nrow(descriptive_table))
  descriptive_table_f <- merge(
    descriptive_table,
    label_df[label_df$Variable != outcome_variable, ],
    by.x = c("Variable", "variable_level"),
    by.y = c("Variable", "Sublevel"),
    all.x = TRUE
  )
  
  descriptive_table_f <- descriptive_table_f[order(descriptive_table_f$RowOrder), ]
  descriptive_table_f$RowOrder <- NULL
  
  descriptive_table_f$report <- paste(
    descriptive_table_f$Value, " (",
    descriptive_table_f$lower_2.5, "-",
    descriptive_table_f$upper_97.5, ")", sep = ""
  )
  
  df_wide <- dcast(
    descriptive_table_f,
    Variable + variable_level + Category ~ outcome_level,
    value.var = "report"
  )
  
  df_wide <- df_wide[order(df_wide$Variable, df_wide$variable_level), ]
  
  outcome_levels <- sort(unique(descriptive_table_f$outcome_level))
  colnames(df_wide)[which(colnames(df_wide) %in% as.character(outcome_levels))] <-
    paste0("Level_", outcome_levels)
  
  level_cols <- paste0("Level_", outcome_levels)
  df_wide <- df_wide[, c("Variable", "variable_level", "Category", level_cols)]
  
  p_value_df <- descriptive_table_f[(descriptive_table_f$p_value) != NaN, c("Variable", "variable_level", "Category", "p_value")]
  df_wide <- merge(df_wide, p_value_df, by = c("Variable", "variable_level", "Category"), all.x = TRUE)
  df_wide <- df_wide[, c("Variable", "variable_level", "Category", level_cols, "p_value")]
  
  df_wide$Variable <- factor(df_wide$Variable, levels = variables)
  df_wide <- df_wide[order(df_wide$Variable, df_wide$variable_level), ]
  
  # Step 1: Get mapping from outcome level (e.g., 0, 1...) to label (e.g., "Never", "Rarely")
  outcome_labels <- label_df[label_df$Variable == outcome_variable, ]
  outcome_labels$colname <- paste0("Level_", outcome_labels$Sublevel)
  
  # Step 2: Create a named vector for renaming
  rename_vector <- setNames(outcome_labels$Category, outcome_labels$colname)
  
  # Step 3: Rename columns if they exist in df_wide
  colnames(df_wide) <- ifelse(
    colnames(df_wide) %in% names(rename_vector),
    rename_vector[colnames(df_wide)],
    colnames(df_wide)
  )
  
  return(df_wide)
}


crude_LR <- function(outcome, variable, design){
  var_data <- design$variables[[variable]]
  
  if (is.factor(var_data) || is.character(var_data)) {
    formula_text <- paste0(outcome, " ~ factor(", variable, ")")
  } else {
    formula_text <- paste0(outcome, " ~ ", variable)
  }
  
  model <- svyglm(
    as.formula(formula_text),
    family = quasibinomial,
    design = design,
    na.action = na.omit
  )
  
  coef_table <- summary(model)$coefficients
  exponential_t <- exp(confint(model))
  # If exponential_t is a matrix, extract the confidence intervals as columns; otherwise, treat it as a vector
  if (is.matrix(exponential_t)) {
    lower_ci <- exponential_t[,"2.5 %"][2:nrow(exponential_t)]
    upper_ci <- exponential_t[,"97.5 %"][2:nrow(exponential_t)]
  } else {
    lower_ci <- exponential_t[1]
    upper_ci <- exponential_t[2]
  }
  primary_table <- data.frame(
    OR = exp(coef_table[, "Estimate"])[2:length(coef_table[, "Estimate"])],
    Lower_CI = lower_ci,
    Upper_CI = upper_ci,
    p_value = coef_table[,'Pr(>|t|)'][2:length(coef_table[, "Estimate"])]
  )
  
  primary_table[c("OR", "Lower_CI", "Upper_CI")] <- round(primary_table[c("OR", "Lower_CI", "Upper_CI")], 2)
  round_pvalue <- function(p){
    ifelse(p >= 0.0001, round(p, 3), '<0.0001')
  }
  primary_table$p_value <- round_pvalue(primary_table$p_value)
  primary_table$crude_OR <- paste(primary_table$OR, " (", primary_table$Lower_CI, "-", primary_table$Upper_CI, ")", sep = "")
  report_table <- primary_table[c("crude_OR", "p_value")]
  return(report_table)
}


crudeLR_table_maker <- function(variables, group_var, outcome,design, func, label_df) {
  final_df <- data.frame()
  for (variable_name in variables) {
    summary_table <- func(outcome,variable_name, design)
    if (nrow(final_df) == 0) {
      final_df <- summary_table
    } else {
      final_df <- rbind(final_df, summary_table)
    }
  }
  final_df <- as.data.frame(final_df)
  final_df$Variable <- gsub("factor\\(([^)]+)\\).*", "\\1", rownames(final_df))
  final_df$Sublevel <- gsub(".*\\)(\\d+).*", "\\1", rownames(final_df))
  label_df_model <- subset(label_df, Variable %in% variables)
  label_df_model$Category <- ifelse(label_df_model$Sublevel == 0, 
                                    paste(label_df_model$Category, "(ref)"), 
                                    label_df_model$Category)
  label_df_model$RowOrder <- seq_len(nrow(label_df_model))
  final_df <- merge(final_df, label_df_model[label_df_model$Variable != outcome, ],
                                 by = c("Variable", "Sublevel"), all = TRUE)
  final_df <- final_df[order(final_df$RowOrder), ]
  final_df$RowOrder <- NULL
  final_df[is.na(final_df)] <- "-"
  final_df <- final_df[c('Variable', 'Sublevel', 'Category', 'crude_OR', 'p_value')]
  colnames(final_df) <- c('Variable', 'Sublevel', 'Category', paste(group_var ,' crude_OR'), paste(group_var ,' p_value'))
  return(final_df)
}

adj_LR_t <- function(base_formula, adj_variables, variable, design){
  
  var_data <- design$variables[[variable]]
  
  if (variable %in% adj_variables) {
    model <- svyglm(as.formula(base_formula),
                    family = quasibinomial,
                    design = design,
                    na.action = na.omit)  
  } else {
    if (is.factor(var_data) || is.character(var_data)) {
      formula_text <- paste0(base_formula, " + factor(", variable, ")")
    } else {
      formula_text <- paste0(base_formula, " + ", variable)
    }
    
    model <- svyglm(as.formula(formula_text),
                    family = quasibinomial,
                    design = design,
                    na.action = na.omit)
  }

  
  coef_table <- summary(model)$coefficients
  coef_table <- coef_table[2:nrow(coef_table),]
  primary_table <- as.data.frame(coef_table)
  primary_table$Variable <- gsub("factor\\(([^)]+)\\).*", "\\1", rownames(primary_table))
  primary_table <- primary_table[primary_table$Variable == variable, ]
  primary_table$OR <- exp(primary_table$Estimate)
  primary_table$p_value <-  primary_table[,'Pr(>|t|)']
  if (length(rownames(primary_table)) > 1){
    primary_table$Lower_CI <-  exp(confint(model))[rownames(primary_table),][,"2.5 %"]
    primary_table$Upper_CI <-  exp(confint(model))[rownames(primary_table),][,"97.5 %"]
  } else {
    exponential_t <- exp(confint(model))[grep(variable, rownames(exp(confint(model)))), ]
    primary_table$Lower_CI <- exponential_t[1]
    primary_table$Upper_CI <- exponential_t[2]
  }
  primary_table <- primary_table[,c("OR", "Lower_CI", "Upper_CI", "p_value")]
  primary_table[c("OR", "Lower_CI", "Upper_CI")] <- round(primary_table[c("OR", "Lower_CI", "Upper_CI")], 2)
  round_pvalue <- function(p){
    ifelse(p >= 0.0001, round(p, 3), '<0.0001')
  }
  primary_table$p_value <- round_pvalue(primary_table$p_value)
  primary_table$adjusted_OR <- paste(primary_table$OR, " (", primary_table$Lower_CI, "-", primary_table$Upper_CI, ")", sep = "")
  report_table <- primary_table[c("adjusted_OR", "p_value")]
  
  return(report_table)
}


adjLR_table_maker <- function(base_formula, adj_variables, variables, design, outcome, group_var, func, label_df) {
  final_df <- data.frame()
  for (variable_name in variables) {
    summary_table <- func(base_formula, adj_variables, variable_name, design)
    if (nrow(final_df) == 0) {
      final_df <- summary_table
    } else {
      final_df <- rbind(final_df, summary_table)
    }
  }
  final_df <- as.data.frame(final_df)
  final_df$Variable <- gsub("factor\\(([^)]+)\\).*", "\\1", rownames(final_df))
  final_df$Sublevel <- gsub(".*\\)(\\d+).*", "\\1", rownames(final_df))
  label_df_model <- subset(label_df, Variable %in% variables)
  label_df_model$Category <- ifelse(label_df_model$Sublevel == 0, 
                                    paste(label_df_model$Category, "(ref)"), 
                                    label_df_model$Category)
  label_df_model$RowOrder <- seq_len(nrow(label_df_model))
  final_df <- merge(final_df, label_df_model[label_df_model$Variable != outcome, ],
                    by = c("Variable", "Sublevel"), all = TRUE)
  final_df <- final_df[order(final_df$RowOrder), ]
  final_df$RowOrder <- NULL
  final_df[is.na(final_df)] <- "-"
  final_df <- final_df[c('Variable', 'Sublevel', 'Category', 'adjusted_OR', 'p_value')]
  colnames(final_df) <- c('Variable', 'Sublevel', 'Category', paste(group_var ,' adjusted_OR'), paste(group_var ,' p_value'))
  return(final_df)
}




crude_lm <- function(outcome, variable, design) {
  model <- svyglm(as.formula(paste0(outcome, " ~ factor(", variable, ")")),
                  design = design,
                  na.action = na.omit)
  
  coef_table <- summary(model)$coefficients
  conf_int <- confint(model)
  
  if (is.matrix(conf_int)) {
    lower_ci <- conf_int[,"2.5 %"][2:nrow(conf_int)]
    upper_ci <- conf_int[,"97.5 %"][2:nrow(conf_int)]
  } else {
    lower_ci <- conf_int[1]
    upper_ci <- conf_int[2]
  }
  
  primary_table <- data.frame(
    Estimate = coef_table[, "Estimate"][2:length(coef_table[, "Estimate"])],
    Lower_CI = lower_ci,
    Upper_CI = upper_ci,
    p_value = coef_table[,"Pr(>|t|)"][2:length(coef_table[, "Estimate"])]
  )
  
  primary_table[c("Estimate", "Lower_CI", "Upper_CI")] <- round(primary_table[c("Estimate", "Lower_CI", "Upper_CI")], 2)
  
  round_pvalue <- function(p){
    ifelse(p >= 0.0001, round(p, 3), '<0.0001')
  }
  
  primary_table$p_value <- round_pvalue(primary_table$p_value)
  primary_table$crude_Beta <- paste(primary_table$Estimate, " (", primary_table$Lower_CI, "-", primary_table$Upper_CI, ")", sep = "")
  report_table <- primary_table[c("crude_Beta", "p_value")]
  return(report_table)
}


crudeLM_table_maker <- function(variables, group_var, outcome, design, func, label_df) {
  final_df <- data.frame()
  for (variable_name in variables) {
    summary_table <- func(outcome, variable_name, design)
    if (nrow(final_df) == 0) {
      final_df <- summary_table
    } else {
      final_df <- rbind(final_df, summary_table)
    }
  }
  
  final_df <- as.data.frame(final_df)
  final_df$Variable <- gsub("factor\\(([^)]+)\\).*", "\\1", rownames(final_df))
  final_df$Sublevel <- gsub(".*\\)(\\d+).*", "\\1", rownames(final_df))
  
  label_df_model <- subset(label_df, Variable %in% variables)
  label_df_model$Category <- ifelse(label_df_model$Sublevel == 0, 
                                    paste(label_df_model$Category, "(ref)"), 
                                    label_df_model$Category)
  label_df_model$RowOrder <- seq_len(nrow(label_df_model))
  
  final_df <- merge(final_df, label_df_model[label_df_model$Variable != outcome, ],
                    by = c("Variable", "Sublevel"), all = TRUE)
  
  final_df <- final_df[order(final_df$RowOrder), ]
  final_df$RowOrder <- NULL
  final_df[is.na(final_df)] <- "-"
  final_df <- final_df[c('Variable', 'Sublevel', 'Category', 'crude_Beta', 'p_value')]
  colnames(final_df) <- c('Variable', 'Sublevel', 'Category', paste(group_var ,' crude_Beta'), paste(group_var ,' p_value'))
  return(final_df)
}


age_sex_adjusted_prevalence_maker <- function(outcome_var, age_sex_term, variable_list, survey_design, n_round = 2){
  age_sex_adjusted <- as.formula(paste0( outcome_var , ' ~ ',age_sex_term))
  model <- svyglm(age_sex_adjusted, survey_design,  family = quasibinomial(link = "logit"))
  result_df <- data.frame()
  for (i in variable_list){
    #prediction is equivalent to margin, at in STATA
    mod <- svypredmeans(model,as.formula(paste0("~factor(", i, ")")))
    df <- data.frame(mod)
    final_df <- data.frame(variable_level = as.integer(rownames(df)),
                           Value = round(df[,'mean'] * 100, 2),
                           lower_2.5 = round(confint(mod)[,'2.5 %'] * 100,2),
                           upper_97.5 = round(confint(mod)[,'97.5 %'] * 100,2))
    final_df$Variable <- i
    final_df <- final_df[order(final_df$variable_level), ]
    final_df$age_sex_adjusted_prevalence <- paste(
      final_df$Value, " (",
      final_df$lower_2.5, "-",
      final_df$upper_97.5, ")", sep = ""
    )
    final_df <- final_df[c('Variable', 'variable_level', 'age_sex_adjusted_prevalence')]
    result_df <- rbind(result_df,final_df)
  }
  result_df <- merge(
    result_df,
    label_df[label_df$Variable != outcome_var, ],
    by.x = c("Variable", "variable_level"),
    by.y = c("Variable", "Sublevel"),
    all.x = TRUE
  )
  result_df$outcome <- outcome_var
  result_df <- result_df[c('outcome','Variable', 'variable_level', 'Category', 'age_sex_adjusted_prevalence')]
  return(result_df)
}


trend_test_median <- function(base_formula, variable, continuous_var, design){
  
  # Step 1: Extract raw data
  df <- design$variables
  
  # Step 2: Calculate median of continuous variable within each category
  median_map <- tapply(df[[continuous_var]], df[[variable]], 
                       median, na.rm = TRUE)
  
  # Step 3: Create median-scored variable
  trend_var_name <- paste0(variable, "_median_trend")
  
  df[[trend_var_name]] <- median_map[as.character(df[[variable]])]
  
  # Step 4: Update design object
  design$variables <- df
  
  # Step 5: Fit model
  trend_formula <- paste0(base_formula, " + ", trend_var_name)
  
  model <- svyglm(
    as.formula(trend_formula),
    family = quasibinomial,
    design = design,
    na.action = na.omit
  )
  
  coef_table <- summary(model)$coefficients
  
  trend_row <- coef_table[grep(trend_var_name, rownames(coef_table)), , drop = FALSE]
  
  p_value <- trend_row[, "Pr(>|t|)"]
  
  round_pvalue <- function(p){
    ifelse(p >= 0.0001, round(p, 3), "<0.0001")
  }
  
  return(round_pvalue(p_value))
}


run_adj_model_with_trend <- function(outcome,
                                     base_adjusters,   # e.g. "age + factor(sex)"
                                     adj_variables,
                                     exposure_vars,
                                     trend_map,        # named list: quartile -> continuous
                                     design,
                                     label_df) {
  
  # Build base formula
  base_formula <- paste0(outcome, " ~ ", base_adjusters)
  
  # Run adjusted model table
  adj_table <- adjLR_table_maker(
    base_formula = base_formula,
    adj_variables = adj_variables,
    variables = exposure_vars,
    design = design,
    outcome = outcome,
    group_var = outcome,
    func = adj_LR_t,
    label_df = label_df
  )
  
  # Add empty trend column
  trend_colname <- paste0(outcome, " trend_p")
  adj_table[[trend_colname]] <- NA
  
  # Compute trend for each ordered exposure
  for (quartile_var in names(trend_map)) {
    
    continuous_var <- trend_map[[quartile_var]]
    
    p_trend <- trend_test_median(
      base_formula = base_formula,
      variable = quartile_var,
      continuous_var = continuous_var,
      design = design
    )
    
    # Insert only in last category row (Sublevel == 3 assumed Q4)
    adj_table[[trend_colname]][
      adj_table$Variable == quartile_var &
        adj_table$Sublevel == 3
    ] <- p_trend
  }
  
  return(adj_table)
}



trend_test_median_crude <- function(outcome, variable, continuous_var, design){
  
  df <- design$variables
  
  # Compute median of continuous variable within each category
  median_map <- tapply(df[[continuous_var]],
                       df[[variable]],
                       median,
                       na.rm = TRUE)
  
  trend_var_name <- paste0(variable, "_median_trend")
  df[[trend_var_name]] <- median_map[as.character(df[[variable]])]
  design$variables <- df
  
  trend_formula <- paste0(outcome, " ~ ", trend_var_name)
  
  model <- svyglm(
    as.formula(trend_formula),
    family = quasibinomial,
    design = design,
    na.action = na.omit
  )
  
  p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]
  
  ifelse(p_value >= 0.0001, round(p_value, 3), "<0.0001")
}


run_crude_model_with_trend <- function(outcome,
                                       variables,
                                       trend_map,
                                       design,
                                       label_df){
  
  crude_table <- crudeLR_table_maker(
    setdiff(variables, outcome),
    outcome,
    outcome,
    design,
    crude_LR,
    label_df
  )
  
  trend_colname <- paste0(outcome, " trend_p")
  crude_table[[trend_colname]] <- NA
  
  # Add trend for ordered exposures only
  for (quartile_var in names(trend_map)) {
    
    if (quartile_var %in% variables) {
      
      p_trend <- trend_test_median_crude(
        outcome = outcome,
        variable = quartile_var,
        continuous_var = trend_map[[quartile_var]],
        design = design
      )
      
      crude_table[[trend_colname]][
        crude_table$Variable == quartile_var &
          crude_table$Sublevel == 3
      ] <- p_trend
    }
  }
  
  return(crude_table)
}


run_crude_subgroup_with_trend <- function(outcome,
                                          variables,
                                          trend_map,
                                          design,
                                          label_df,
                                          subgroup_var = NULL,
                                          subgroup_value = NULL){
  
  # Apply subgroup if provided
  if (!is.null(subgroup_var)) {
    design <- subset(design,
                     design$variables[[subgroup_var]] == subgroup_value)
    
    # Remove subgroup variable from crude list
    variables <- setdiff(variables, subgroup_var)
  }
  
  # Remove outcome from predictors
  variables_model <- setdiff(variables, outcome)
  
  crude_table <- crudeLR_table_maker(
    variables_model,
    outcome,
    outcome,
    design,
    crude_LR,
    label_df
  )
  
  trend_colname <- paste0(outcome, " trend_p")
  crude_table[[trend_colname]] <- NA
  
  # Add trend p
  for (quartile_var in names(trend_map)) {
    
    if (quartile_var %in% variables_model) {
      
      p_trend <- trend_test_median_crude(
        outcome = outcome,
        variable = quartile_var,
        continuous_var = trend_map[[quartile_var]],
        design = design
      )
      
      crude_table[[trend_colname]][
        crude_table$Variable == quartile_var &
          crude_table$Sublevel == 3
      ] <- p_trend
    }
  }
  
  return(crude_table)
}




rcs_logistic_ag_modified <- function (data, knot, y, x, covs, prob, title,
                                      y_title, x_title) 
{
  pacman::p_load(rms, ggplot2, survminer, survival, dplyr, 
                 patchwork, Cairo)
  if (!missing(knot)) {
    warning("please be sure of knot by AIC min(default) or preliminary investigation suggested")
  }
  if (missing(data)) {
    stop("data required.")
  }
  if (missing(x)) {
    stop("x required.")
  }
  if (missing(prob)) {
    prob <- 0.5
  }
  else {
    assign("prob", prob)
  }
  
  call <- match.call()
  data <- as.data.frame(data)
  y <- y
  x <- x
  if (missing(covs)) {
    covs = NULL
    indf <- dplyr::select(data, y, x)
  }
  else {
    assign("covs", covs)
    indf <- dplyr::select(data, y, x, covs)
  }
  indf[, "y"] <- indf[, y]
  indf[, "x"] <- indf[, x]
  sum(!complete.cases(indf[, c(y, x)]))
  indf <- indf[complete.cases(indf[, c(y, x)]), ]
  dd <- NULL
  dd <<- rms::datadist(indf)
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")
  aics <- NULL
  for (i in 3:7) {
    if (is.null(covs)) {
      formula <- paste0("y~ rcs(x, ", i, ")")
    }
    else {
      formula <- paste0("y~ rcs(x, ", i, ")", " + ", paste0(covs, 
                                                            collapse = " + "))
    }
    fit <- rms::lrm(as.formula(formula), data = indf, x = TRUE, 
                    se.fit = TRUE, tol = 1e-25)
    summary(fit)
    aics <- c(aics, AIC(fit))
    kn <- seq(3, 7)[which.min(aics)]
  }
  if (missing(knot)) {
    knot <- kn
  }
  if (is.null(covs)) {
    formula <- paste0("y~ rcs(x, ", knot, ")", paste0(covs, 
                                                      collapse = " + "))
  }
  else {
    formula <- paste0("y~ rcs(x, ", knot, ")", " + ", paste0(covs, 
                                                             collapse = " + "))
  }
  model <- rms::lrm(as.formula(formula), data = indf, x = TRUE, 
                    se.fit = TRUE, tol = 1e-25)
  model.logistic <- model
  anova(model)
  pvalue_all <- anova(model)[1, 3]
  pvalue_nonlin <- round(anova(model)[2, 3], 3)
  pre.model <- rms::Predict(model.logistic, x, fun = exp, 
                            type = "predictions", ref.zero = T, conf.int = 0.95, 
                            digits = 2)
  Q20 <- quantile(indf$x, probs = seq(0, 1, 0.05))
  probtemp <- prob
  refvalue <- quantile(indf[, "x"], prob = probtemp)
  dd <<- rms::datadist(indf)
  dd[["limits"]]["Adjust to", "x"] <<- refvalue
  old <- options()
  on.exit(options(old))
  options(datadist = "dd")
  model <- update(model)
  model.logistic <- model
  pre.model <- rms::Predict(model.logistic, x, fun = exp, 
                            type = "predictions", ref.zero = T, conf.int = 0.95, 
                            digits = 2)
  newdf1 <- as.data.frame(dplyr::select(pre.model, x, yhat, 
                                        lower, upper))
  colnames(newdf1) <- c("x", "y", "lower", "upper")
  min(newdf1[, "x"])
  max(newdf1[, "x"])
  xmin <- min(newdf1[, "x"])
  xmax <- max(newdf1[, "x"])
  min(newdf1[, "lower"])
  max(newdf1[, "upper"])
  ymax1 <- ceiling(max(newdf1[, "upper"]))
  newdf2 <- indf[indf[, "x"] >= xmin & indf[, "x"] <= xmax, 
  ]
  breaks <- seq(xmin, xmax, length = 20)
  h <- hist(newdf2[, "x"], breaks = breaks, right = TRUE)
  max(h[["counts"]]/sum(h[["counts"]]))
  ymax2 <- 20
  newdf3 <- data.frame(x = h[["mids"]], freq = h[["counts"]], 
                       pct = h[["counts"]]/sum(h[["counts"]]))
  freq <- cut(newdf2[, "x"], breaks = breaks, dig.lab = 6, 
              right = TRUE)
  as.data.frame(table(freq))
  scale_factor <- ymax2/ymax1
  xtitle <- x_title
  #ytitle1 <- paste0("OR where the refvalue for ", x, " is ", 
  #                  sprintf("%.3f", refvalue))
  ytitle1 <- y_title
  ytitle2 <- "Percentage of Population (%)"
  offsetx1 <- (xmax - xmin) * 0.02
  offsety1 <- ymax1 * 0.02
  labelx1 <- xmin + (xmax - xmin) * 0.2
  labely1 <- ymax1 * 0.9
  label1 <- paste0("Estimation", "\n", "95% CI")
  labelx2 <- xmin + (xmax - xmin) * 0.7
  labely2 <- ymax1 * 0.9
  label2 <- paste0("P-overall = ", ifelse(pvalue_all < 0.001, 
                                          "< 0.001", sprintf("%.3f", pvalue_all)), "\nP-non-linear = ", 
                   ifelse(pvalue_nonlin < 0.001, "< 0.001", sprintf("%.3f", 
                                                                    pvalue_nonlin)), "\nReference Point (",x, ' = ',sprintf("%.2f", refvalue), ')')
  plot.prob.type1 <- ggplot2::ggplot() +
    geom_bar(data = newdf3, 
             aes(x = x, y = pct * 100/scale_factor), stat = "identity", 
             width = (xmax - xmin)/(length(breaks) - 1), fill = "#f9f7f7", 
             color = "grey") + geom_hline(yintercept = 1, linetype = 2,  color = "grey") + 
    geom_line(data = newdf1, aes(x = x, y = lower), linetype = 2, color = "#ff9999", size = 0.8) + 
    geom_line(data = newdf1, aes(x = x, y = upper), linetype = 2, 
              color = "#ff9999", size = 0.8) + geom_line(data = newdf1, 
                                                         aes(x = x, y = y), color = "#e23e57", size = 1) +
    geom_point(aes(x = refvalue,   y = 1), color = "#e23e57", size = 2) +
    geom_text(aes(x = labelx1, y = labely1, label = label2), hjust = 0) +
    scale_x_continuous(xtitle,  expand = c(0, 0.01), limit = c(xmin, xmax)) +
    scale_y_continuous(ytitle1,   expand = c(0, 0), limit = c(0, ymax1)#, sec.axis = sec_axis(ytitle2,trans = ~. * scale_factor, )
    ) + 
    labs(
      title = title
    ) + theme_minimal(base_size = 14) +
    theme(axis.line = element_line(),  panel.grid = element_blank(), 
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          axis.text = element_text(face = "bold", color = "black", size = 14),
          plot.title = element_text(hjust = 0.5, face = "bold"))
  
  message.print <- list(aics = aics, kn = kn, Q20 = Q20)
  return(plot.prob.type1)
}
