# Modified Slope Index of Inequality (SII) and Relative Index of Inequality
# (RII) functions used by this study. Function bodies are retained from the
# original research workflow.

library(emmeans)
library(survey)
library(dplyr)

sii_modified_binom <- function (data, est, subgroup_order, adjust = NULL, pop = NULL, weight = NULL, 
                          #psu = NULL, strata = NULL, fpc = NULL, 
                          conf.level = 0.95, linear = FALSE, n_round = 2) 
{
  if (!all(c(est, subgroup_order) %in% names(data))) {
    stop("Make sure 'est_col' and 'subgroup_order_col' exist in the data.")
  }
  
  data <- data[!is.na(data[[est]]) & !is.na(data[[subgroup_order]]), ]
  data[[est]] <- as.numeric(as.character(data[[est]]))
  data[[subgroup_order]] <- as.numeric(as.character(data[[subgroup_order]]))
  
  if (length(unique(data[[est]])) == 1) {
    stop("All estimates have the same value; SII not calculated")
  }
  if (length(data[[est]]) <= 2) {
    stop("Estimates must be available for more than two subgroups")
  }
  if (!is.null(data[[est]])) {
    if (!is.numeric(data[[est]])) 
      stop("Estimates need to be numeric")
  }
  if (!is.null(pop)) {
    if (anyNA(data[[pop]])) {
      stop("Population is missing in some subgroups")
    }
    if (!is.numeric(data[[pop]])) {
      stop("Population variable needs to be numeric")
    }
    if (all(data[[pop]] == 0)) {
      stop("Population variable is of size 0 in all subgroups")
    }
  }
  if (is.null(subgroup_order)) {
    stop("Subgroup order variable needs to be declared")
  }
  sorted_order <- sort(data[[subgroup_order]])
  if (!is.null(pop) & (any(diff(sorted_order) != 1) || any(sorted_order%%1 != 
                                                           0))) {
    stop("Subgroup order variable must contain integers in increasing order")
  }
  if (!is.null(weight)) {
    if(!is.numeric(data[[weight]])){
      stop("Weight variable needs to be numeric")  
    }
  }
  if (is.null(pop) & is.null(weight)) {
    message("Neither a population variable nor a weight variable has been\n declared")
  }
  est_v <- data[[est]]
  
  scale <- ifelse(est_v <= 1, 1, ifelse(est_v > 1 & est_v <= 100, 
                                        100, ifelse(est_v > 100 & est_v <= 1000, 1000, ifelse(est_v > 
                                                                                                1000 & est_v <= 10000, 10000, ifelse(est_v > 10000 & 
                                                                                                                                       est_v <= 1e+05, 1e+05, 1e+06)))))
  scale <- max(scale)
  y <- NULL
  ny <- NULL
  if (is.null(pop) & is.null(weight)) {
    data$pop <- rep(1, length(data[[est]]))
  } 
  if (is.null(pop) & !is.null(weight)) {
    data$pop <- data[[weight]]
  } else if (!is.null(pop) & is.null(weight)){
    data$pop <- ceiling(data[[pop]])
    data$y <- round((data[[est]]/scale) * data$pop)
    data$ny <- data$pop - data$y
  } else if (is.null(pop) & is.null(weight)){
    data$pop <- ceiling(data$pop)
    data$y <- round((data[[est]]/scale) * data$pop)
    data$ny <- data$pop - data$y
  }
  data <- data[order(data[[subgroup_order]]), ]
  data$est_sc <- data[[est]]/scale
  sumw <- sum(data$pop, na.rm = TRUE)
  data$sumw <- sumw
  data$cumw <- cumsum(data$pop)
  data$cumw1 <- lag(data$cumw)
  data$cumw1[is.na(data$cumw1)] <- 0
  newdat_sii <- data %>% 
    group_by(across(all_of(subgroup_order))) %>% 
    mutate(
      cumwr  = max(.data$cumw, na.rm = TRUE),     # max cumw within each group
      cumwr1 = min(.data$cumw1, na.rm = TRUE)     # min cumw1 within each group
    ) %>% 
    ungroup() %>% 
    mutate(
      rank = (.data$cumwr1 + 0.5 * (.data$cumwr - .data$cumwr1)) / .data$sumw
    )
  
  adjust_str <- if (!is.null(adjust)) paste("+", adjust) else ""
  model_formula <- if (!is.null(adjust)) {
    as.formula(paste("est_sc ~ rank +", adjust))
  } else {
    as.formula("est_sc ~ rank")
  }
  
  if (is.null(weight)) {
    if (!linear) {
      model_formula2 <- if (!is.null(adjust)) {
        as.formula(paste("cbind(y, ny) ~ rank +", adjust))
      } else {
        as.formula("cbind(y, ny) ~ rank")
      }
      mod <- glm(formula = model_formula2, weights = pop, 
                 data = newdat_sii, family = quasibinomial("logit"))
    }
    else {
      mod <- glm(model_formula, data = newdat_sii, family = gaussian, 
                 weights = pop)
    }
  } else {
    newdat_sii_s <- svydesign(ids = ~1, weights = as.formula(paste("~", weight)),
                              data = newdat_sii)
    if (!linear) {
      mod <- svyglm(model_formula, design = newdat_sii_s, 
                    family = quasibinomial(link = "logit"))
    }
    else {
      mod <- svyglm(model_formula, design = newdat_sii_s, 
                    family = gaussian)
    }
  }
  siie_emmeans <- contrast(regrid(emmeans(mod, specs = ~rank, 
                                          at = list(rank = c(1, 0)))), method = "pairwise")
  siie_sum <- summary(siie_emmeans)
  sii <- siie_sum$estimate
  se.formula <- siie_sum$SE
  cilevel <- 1 - ((1 - conf.level)/2)
  lowerci <- sii - se.formula * qnorm(cilevel)
  upperci <- sii + se.formula * qnorm(cilevel)
  p <- siie_sum$p.value
  if (p >= 0.0001) {
    if (p < 0.001) {
      p <- round(p, 4)
    } else {
      p <- round(p, 3)
    }
  } else {
    p <- '<0.0001'
  }
  SII_CI <- paste(
    round(sii * scale,n_round), " (",
    round(lowerci * scale,n_round), ", ",
    round(upperci * scale,n_round), ")", sep = ""
  )
  return(data.frame(measure = "sii", variable = est,adjut_term = if (!is.null(adjust)) adjust else "-",estimate = sii * scale, 
                    se = se.formula, lowerci = lowerci * scale, upperci = upperci * scale, report = SII_CI,p_value = p))
}

sii_modified_poisson <- function (data, est, subgroup_order, adjust = NULL, pop = NULL, weight = NULL, 
                                #psu = NULL, strata = NULL, fpc = NULL, 
                                conf.level = 0.95, linear = FALSE, n_round = 2) 
{
  if (!all(c(est, subgroup_order) %in% names(data))) {
    stop("Make sure 'est_col' and 'subgroup_order_col' exist in the data.")
  }
  
  data <- data[!is.na(data[[est]]) & !is.na(data[[subgroup_order]]), ]
  data[[est]] <- as.numeric(as.character(data[[est]]))
  data[[subgroup_order]] <- as.numeric(as.character(data[[subgroup_order]]))
  
  if (length(unique(data[[est]])) == 1) {
    stop("All estimates have the same value; SII not calculated")
  }
  if (length(data[[est]]) <= 2) {
    stop("Estimates must be available for more than two subgroups")
  }
  if (!is.null(data[[est]])) {
    if (!is.numeric(data[[est]])) 
      stop("Estimates need to be numeric")
  }
  if (!is.null(pop)) {
    if (anyNA(data[[pop]])) {
      stop("Population is missing in some subgroups")
    }
    if (!is.numeric(data[[pop]])) {
      stop("Population variable needs to be numeric")
    }
    if (all(data[[pop]] == 0)) {
      stop("Population variable is of size 0 in all subgroups")
    }
  }
  if (is.null(subgroup_order)) {
    stop("Subgroup order variable needs to be declared")
  }
  sorted_order <- sort(data[[subgroup_order]])
  if (!is.null(pop) & (any(diff(sorted_order) != 1) || any(sorted_order%%1 != 
                                                           0))) {
    stop("Subgroup order variable must contain integers in increasing order")
  }
  if (!is.null(weight)) {
    if(!is.numeric(data[[weight]])){
      stop("Weight variable needs to be numeric")  
    }
  }
  if (is.null(pop) & is.null(weight)) {
    message("Neither a population variable nor a weight variable has been\n declared")
  }
  est_v <- data[[est]]
  
  scale <- ifelse(est_v <= 1, 1, ifelse(est_v > 1 & est_v <= 100, 
                                        100, ifelse(est_v > 100 & est_v <= 1000, 1000, ifelse(est_v > 
                                                                                                1000 & est_v <= 10000, 10000, ifelse(est_v > 10000 & 
                                                                                                                                       est_v <= 1e+05, 1e+05, 1e+06)))))
  scale <- max(scale)
  y <- NULL
  ny <- NULL
  if (is.null(pop) & is.null(weight)) {
    data$pop <- rep(1, length(data[[est]]))
  } 
  if (is.null(pop) & !is.null(weight)) {
    data$pop <- data[[weight]]
  } else if (!is.null(pop) & is.null(weight)){
    data$pop <- ceiling(data[[pop]])
    data$y <- round((data[[est]]/scale) * data$pop)
    data$ny <- data$pop - data$y
  } else if (is.null(pop) & is.null(weight)){
    data$pop <- ceiling(data$pop)
    data$y <- round((data[[est]]/scale) * data$pop)
    data$ny <- data$pop - data$y
  }
  data <- data[order(data[[subgroup_order]]), ]
  data$est_sc <- data[[est]]/scale
  sumw <- sum(data$pop, na.rm = TRUE)
  data$sumw <- sumw
  data$cumw <- cumsum(data$pop)
  data$cumw1 <- lag(data$cumw)
  data$cumw1[is.na(data$cumw1)] <- 0
  newdat_sii <- data %>% 
    group_by(across(all_of(subgroup_order))) %>% 
    mutate(
      cumwr  = max(.data$cumw, na.rm = TRUE),     # max cumw within each group
      cumwr1 = min(.data$cumw1, na.rm = TRUE)     # min cumw1 within each group
    ) %>% 
    ungroup() %>% 
    mutate(
      rank = (.data$cumwr1 + 0.5 * (.data$cumwr - .data$cumwr1)) / .data$sumw
    )
  
  adjust_str <- if (!is.null(adjust)) paste("+", adjust) else ""
  model_formula <- if (!is.null(adjust)) {
    as.formula(paste("est_sc ~ rank +", adjust))
  } else {
    as.formula("est_sc ~ rank")
  }
  
  if (is.null(weight)) {
    if (!linear) {
      model_formula2 <- if (!is.null(adjust)) {
        as.formula(paste("cbind(y, ny) ~ rank +", adjust))
      } else {
        as.formula("cbind(y, ny) ~ rank")
      }
      mod <- glm(formula = model_formula2, weights = pop, 
                 data = newdat_sii, family = quasipoisson("log"))
    }
    else {
      mod <- glm(model_formula, data = newdat_sii, family = gaussian, 
                 weights = pop)
    }
  } else {
    newdat_sii_s <- svydesign(ids = ~1, weights = as.formula(paste("~", weight)),
                              data = newdat_sii)
    if (!linear) {
      mod <- svyglm(model_formula, design = newdat_sii_s, 
                    family = quasipoisson(link = "log"))
    }
    else {
      mod <- svyglm(model_formula, design = newdat_sii_s, 
                    family = gaussian)
    }
  }
  siie_emmeans <- contrast(regrid(emmeans(mod, specs = ~rank, 
                                          at = list(rank = c(1, 0)))), method = "pairwise")
  siie_sum <- summary(siie_emmeans)
  sii <- siie_sum$estimate
  se.formula <- siie_sum$SE
  cilevel <- 1 - ((1 - conf.level)/2)
  lowerci <- sii - se.formula * qnorm(cilevel)
  upperci <- sii + se.formula * qnorm(cilevel)
  p <- siie_sum$p.value
  if (p >= 0.0001) {
    if (p < 0.001) {
      p <- round(p, 4)
    } else {
      p <- round(p, 3)
    }
  } else {
    p <- '<0.0001'
  }
  SII_CI <- paste(
    round(sii * scale,n_round), " (",
    round(lowerci * scale,n_round), ", ",
    round(upperci * scale,n_round), ")", sep = ""
  )
  return(data.frame(measure = "sii", variable = est,adjut_term = if (!is.null(adjust)) adjust else "-",estimate = sii * scale, 
                    se = se.formula, lowerci = lowerci * scale, upperci = upperci * 
                      scale, report = SII_CI,p_value = p))
}



rii_modified_binom <- function (data, est, subgroup_order, adjust = NULL, pop = NULL, weight = NULL, 
                          #psu = NULL, strata = NULL, fpc = NULL, 
                          conf.level = 0.95, linear = FALSE, n_round = 2) 
{
  if (!all(c(est, subgroup_order) %in% names(data))) {
    stop("Make sure 'est_col' and 'subgroup_order_col' exist in the data.")
  }
  
  data <- data[!is.na(data[[est]]) & !is.na(data[[subgroup_order]]), ]
  data[[est]] <- as.numeric(as.character(data[[est]]))
  data[[subgroup_order]] <- as.numeric(as.character(data[[subgroup_order]]))
  
  if (length(unique(data[[est]])) == 1) {
    stop("All estimates have the same value; SII not calculated")
  }
  if (length(data[[est]]) <= 2) {
    stop("Estimates must be available for more than two subgroups")
  }
  if (!is.null(data[[est]])) {
    if (!is.numeric(data[[est]])) 
      stop("Estimates need to be numeric")
  }
  if (!is.null(pop)) {
    if (anyNA(data[[pop]])) {
      stop("Population is missing in some subgroups")
    }
    if (!is.numeric(data[[pop]])) {
      stop("Population variable needs to be numeric")
    }
    if (all(data[[pop]] == 0)) {
      stop("Population variable is of size 0 in all subgroups")
    }
  }
  if (is.null(subgroup_order)) {
    stop("Subgroup order variable needs to be declared")
  }
  sorted_order <- sort(data[[subgroup_order]])
  if (!is.null(pop) & (any(diff(sorted_order) != 1) || any(sorted_order%%1 != 
                                                           0))) {
    stop("Subgroup order variable must contain integers in increasing order")
  }
  if (!is.null(weight)) {
    if(!is.numeric(data[[weight]])){
      stop("Weight variable needs to be numeric")  
    }
  }
  if (is.null(pop) & is.null(weight)) {
    message("Neither a population variable nor a weight variable has been\n declared")
  }
  est_v <- data[[est]]
  
  scale <- ifelse(est_v <= 1, 1, ifelse(est_v > 1 & est_v <= 100, 
                                        100, ifelse(est_v > 100 & est_v <= 1000, 1000, ifelse(est_v > 
                                                                                                1000 & est_v <= 10000, 10000, ifelse(est_v > 10000 & 
                                                                                                                                       est_v <= 1e+05, 1e+05, 1e+06)))))
  scale <- max(scale)
  y <- NULL
  ny <- NULL
  if (is.null(pop) & is.null(weight)) {
    data$pop <- rep(1, length(data[[est]]))
  } 
  if (is.null(pop) & !is.null(weight)) {
    data$pop <- data[[weight]]
  } else if (!is.null(pop) & is.null(weight)){
    data$pop <- ceiling(data[[pop]])
    data$y <- round((data[[est]]/scale) * data$pop)
    data$ny <- data$pop - data$y
  } else if (is.null(pop) & is.null(weight)){
    data$pop <- ceiling(data$pop)
    data$y <- round((data[[est]]/scale) * data$pop)
    data$ny <- data$pop - data$y
  }
  data <- data[order(data[[subgroup_order]]), ]
  data$est_sc <- data[[est]]/scale
  sumw <- sum(data$pop, na.rm = TRUE)
  data$sumw <- sumw
  data$cumw <- cumsum(data$pop)
  data$cumw1 <- lag(data$cumw)
  data$cumw1[is.na(data$cumw1)] <- 0
  newdat_sii <- data %>% 
    group_by(across(all_of(subgroup_order))) %>% 
    mutate(
      cumwr  = max(.data$cumw, na.rm = TRUE),     # max cumw within each group
      cumwr1 = min(.data$cumw1, na.rm = TRUE)     # min cumw1 within each group
    ) %>% 
    ungroup() %>% 
    mutate(
      rank = (.data$cumwr1 + 0.5 * (.data$cumwr - .data$cumwr1)) / .data$sumw
    )
  
  adjust_str <- if (!is.null(adjust)) paste("+", adjust) else ""
  model_formula <- if (!is.null(adjust)) {
    as.formula(paste("est_sc ~ rank +", adjust))
  } else {
    as.formula("est_sc ~ rank")
  }
  
  if (is.null(weight)) {
    if (!linear) {
      model_formula2 <- if (!is.null(adjust)) {
        as.formula(paste("cbind(y, ny) ~ rank +", adjust))
      } else {
        as.formula("cbind(y, ny) ~ rank")
      }
      mod <- glm(formula = model_formula2, weights = pop, 
                 data = newdat_sii, family = quasibinomial("logit"))
    }
    else {
      mod <- glm(model_formula, data = newdat_sii, family = gaussian, 
                 weights = pop)
    }
  } else {
    newdat_sii_s <- svydesign(ids = ~1, weights = as.formula(paste("~", weight)),
                              data = newdat_sii)
    if (!linear) {
      mod <- svyglm(model_formula, design = newdat_sii_s, 
                    family = quasibinomial(link = "logit"))
    }
    else {
      mod <- svyglm(model_formula, design = newdat_sii_s, 
                    family = gaussian)
    }
  }
  riie_mod <- contrast(regrid(emmeans(mod, "rank", at = list(rank = c(1, 
                                                                      0))), "log"), method = "pairwise")
  modsum <- summary(riie_mod)
  est_rii <- modsum$estimate
  se.formula <- modsum$SE
  cilevel <- 1 - ((1 - conf.level)/2)
  ci <- list(l = est_rii - se.formula * qnorm(cilevel), 
             u = est_rii + se.formula * qnorm(cilevel))
  p <- modsum$p.value
  if (p >= 0.0001) {
    if (p < 0.001) {
      p <- round(p, 4)
    } else {
      p <- round(p, 3)
    }
  } else {
    p <- '<0.0001'
  }
  RII_CI <- paste(
    round(exp(est_rii),n_round), " (",
    round(exp(ci$l),n_round), ", ",
    round(exp(ci$u),n_round), ")", sep = ""
  )
  return(data.frame(measure = "rii", variable = est,adjut_term = if (!is.null(adjust)) adjust else "-",estimate = exp(est_rii), 
                    se = se.formula, lowerci = exp(ci$l), upperci = exp(ci$u), report = RII_CI,p_value = p))
}

rii_modified_poisson <- function (data, est, subgroup_order, adjust = NULL, pop = NULL, weight = NULL, 
                                #psu = NULL, strata = NULL, fpc = NULL, 
                                conf.level = 0.95, linear = FALSE, n_round = 2) 
{
  if (!all(c(est, subgroup_order) %in% names(data))) {
    stop("Make sure 'est_col' and 'subgroup_order_col' exist in the data.")
  }
  
  data <- data[!is.na(data[[est]]) & !is.na(data[[subgroup_order]]), ]
  data[[est]] <- as.numeric(as.character(data[[est]]))
  data[[subgroup_order]] <- as.numeric(as.character(data[[subgroup_order]]))
  
  if (length(unique(data[[est]])) == 1) {
    stop("All estimates have the same value; SII not calculated")
  }
  if (length(data[[est]]) <= 2) {
    stop("Estimates must be available for more than two subgroups")
  }
  if (!is.null(data[[est]])) {
    if (!is.numeric(data[[est]])) 
      stop("Estimates need to be numeric")
  }
  if (!is.null(pop)) {
    if (anyNA(data[[pop]])) {
      stop("Population is missing in some subgroups")
    }
    if (!is.numeric(data[[pop]])) {
      stop("Population variable needs to be numeric")
    }
    if (all(data[[pop]] == 0)) {
      stop("Population variable is of size 0 in all subgroups")
    }
  }
  if (is.null(subgroup_order)) {
    stop("Subgroup order variable needs to be declared")
  }
  sorted_order <- sort(data[[subgroup_order]])
  if (!is.null(pop) & (any(diff(sorted_order) != 1) || any(sorted_order%%1 != 
                                                           0))) {
    stop("Subgroup order variable must contain integers in increasing order")
  }
  if (!is.null(weight)) {
    if(!is.numeric(data[[weight]])){
      stop("Weight variable needs to be numeric")  
    }
  }
  if (is.null(pop) & is.null(weight)) {
    message("Neither a population variable nor a weight variable has been\n declared")
  }
  est_v <- data[[est]]
  
  scale <- ifelse(est_v <= 1, 1, ifelse(est_v > 1 & est_v <= 100, 
                                        100, ifelse(est_v > 100 & est_v <= 1000, 1000, ifelse(est_v > 
                                                                                                1000 & est_v <= 10000, 10000, ifelse(est_v > 10000 & 
                                                                                                                                       est_v <= 1e+05, 1e+05, 1e+06)))))
  scale <- max(scale)
  y <- NULL
  ny <- NULL
  if (is.null(pop) & is.null(weight)) {
    data$pop <- rep(1, length(data[[est]]))
  } 
  if (is.null(pop) & !is.null(weight)) {
    data$pop <- data[[weight]]
  } else if (!is.null(pop) & is.null(weight)){
    data$pop <- ceiling(data[[pop]])
    data$y <- round((data[[est]]/scale) * data$pop)
    data$ny <- data$pop - data$y
  } else if (is.null(pop) & is.null(weight)){
    data$pop <- ceiling(data$pop)
    data$y <- round((data[[est]]/scale) * data$pop)
    data$ny <- data$pop - data$y
  }
  data <- data[order(data[[subgroup_order]]), ]
  data$est_sc <- data[[est]]/scale
  sumw <- sum(data$pop, na.rm = TRUE)
  data$sumw <- sumw
  data$cumw <- cumsum(data$pop)
  data$cumw1 <- lag(data$cumw)
  data$cumw1[is.na(data$cumw1)] <- 0
  newdat_sii <- data %>% 
    group_by(across(all_of(subgroup_order))) %>% 
    mutate(
      cumwr  = max(.data$cumw, na.rm = TRUE),     # max cumw within each group
      cumwr1 = min(.data$cumw1, na.rm = TRUE)     # min cumw1 within each group
    ) %>% 
    ungroup() %>% 
    mutate(
      rank = (.data$cumwr1 + 0.5 * (.data$cumwr - .data$cumwr1)) / .data$sumw
    )
  
  adjust_str <- if (!is.null(adjust)) paste("+", adjust) else ""
  model_formula <- if (!is.null(adjust)) {
    as.formula(paste("est_sc ~ rank +", adjust))
  } else {
    as.formula("est_sc ~ rank")
  }
  
  if (is.null(weight)) {
    if (!linear) {
      model_formula2 <- if (!is.null(adjust)) {
        as.formula(paste("cbind(y, ny) ~ rank +", adjust))
      } else {
        as.formula("cbind(y, ny) ~ rank")
      }
      mod <- glm(formula = model_formula2, weights = pop, 
                 data = newdat_sii, family = quasipoisson("log"))
    }
    else {
      mod <- glm(model_formula, data = newdat_sii, family = gaussian, 
                 weights = pop)
    }
  } else {
    newdat_sii_s <- svydesign(ids = ~1, weights = as.formula(paste("~", weight)),
                              data = newdat_sii)
    if (!linear) {
      mod <- svyglm(model_formula, design = newdat_sii_s, 
                    family = quasipoisson(link = "log"))
    }
    else {
      mod <- svyglm(model_formula, design = newdat_sii_s, 
                    family = gaussian)
    }
  }
  riie_mod <- contrast(regrid(emmeans(mod, "rank", at = list(rank = c(1, 
                                                                      0))), "log"), method = "pairwise")
  modsum <- summary(riie_mod)
  est_rii <- modsum$estimate
  se.formula <- modsum$SE
  cilevel <- 1 - ((1 - conf.level)/2)
  ci <- list(l = est_rii - se.formula * qnorm(cilevel), 
             u = est_rii + se.formula * qnorm(cilevel))
  p <- modsum$p.value
  if (p >= 0.0001) {
    if (p < 0.001) {
      p <- round(p, 4)
    } else {
      p <- round(p, 3)
    }
  } else {
    p <- '<0.0001'
  }
  RII_CI <- paste(
    round(exp(est_rii),n_round), " (",
    round(exp(ci$l),n_round), ", ",
    round(exp(ci$u),n_round), ")", sep = ""
  )
  return(data.frame(measure = "rii", variable = est,adjut_term = if (!is.null(adjust)) adjust else "-",estimate = exp(est_rii), 
                    se = se.formula, lowerci = exp(ci$l), upperci = exp(ci$u), report = RII_CI,p_value = p))
}


df_rank_maker <- function (data, est, subgroup_order,  pop = NULL, weight = NULL) 
{
  data <- data[!is.na(data[[est]]) & !is.na(data[[subgroup_order]]), ]
  data[[est]] <- as.numeric(as.character(data[[est]]))
  data[[subgroup_order]] <- as.numeric(as.character(data[[subgroup_order]]))
  first_columns <- colnames(data)
  
  sorted_order <- sort(data[[subgroup_order]])
  if (!is.null(pop) & (any(diff(sorted_order) != 1) || any(sorted_order%%1 != 
                                                           0))) {
    stop("Subgroup order variable must contain integers in increasing order")
  }
  if (!is.null(weight)) {
    if(!is.numeric(data[[weight]])){
      stop("Weight variable needs to be numeric")  
    }
  }
  if (is.null(pop) & is.null(weight)) {
    message("Neither a population variable nor a weight variable has been\n declared")
  }
  est_v <- data[[est]]
  
  scale <- ifelse(est_v <= 1, 1, ifelse(est_v > 1 & est_v <= 100, 
                                        100, ifelse(est_v > 100 & est_v <= 1000, 1000, ifelse(est_v > 
                                                                                                1000 & est_v <= 10000, 10000, ifelse(est_v > 10000 & 
                                                                                                                                       est_v <= 1e+05, 1e+05, 1e+06)))))
  scale <- max(scale)
  y <- NULL
  ny <- NULL
  if (is.null(pop) & is.null(weight)) {
    data$pop <- rep(1, length(data[[est]]))
  } 
  if (is.null(pop) & !is.null(weight)) {
    data$pop <- data[[weight]]
  } else if (!is.null(pop) & is.null(weight)){
    data$pop <- ceiling(data[[pop]])
    data$y <- round((data[[est]]/scale) * data$pop)
    data$ny <- data$pop - data$y
  } else if (is.null(pop) & is.null(weight)){
    data$pop <- ceiling(data$pop)
    data$y <- round((data[[est]]/scale) * data$pop)
    data$ny <- data$pop - data$y
  }
  data <- data[order(data[[subgroup_order]]), ]
  data$est_sc <- data[[est]]/scale
  sumw <- sum(data$pop, na.rm = TRUE)
  data$sumw <- sumw
  data$cumw <- cumsum(data$pop)
  data$cumw1 <- lag(data$cumw)
  data$cumw1[is.na(data$cumw1)] <- 0
  newdat_sii <- data %>% 
    group_by(across(all_of(subgroup_order))) %>% 
    mutate(
      cumwr  = max(.data$cumw, na.rm = TRUE),     # max cumw within each group
      cumwr1 = min(.data$cumw1, na.rm = TRUE)     # min cumw1 within each group
    ) %>% 
    ungroup() %>% 
    mutate(
      rank = (.data$cumwr1 + 0.5 * (.data$cumwr - .data$cumwr1)) / .data$sumw
    )
  
  data_final <- data[,c(names(data) %in% first_columns)]
  data_final[[paste0("Xrank_", subgroup_order)]] <- newdat_sii$rank
  return(data_final)
}
