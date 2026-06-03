library(dplyr)
library(purrr)
library(readr)

data_path <- "output/finalpace.csv"

dat <- read_csv(data_path)

names(dat) <- c(
  "ID", "participant_id_2", "Pubertal_Stage",
  "School_Disengagement", "School_Environment", "School_Involvement",
  "Prosocial_Behavior", "Emotional_Neglect", "Physical_Neglect",
  "Neighborhood_Safety", "Family_Conflict", "Physical_Activity",
  "Bullying", "screen_time", "Family_Mentalhealth",
  "Arousal_Disorder", "sleep1", "Excessive_Sleepiness",
  "Sleep_Hyperhidrosis", "sleep2", "Sleep_Wake_Transition",
  "Maternal_Age", "Paternal_Age", "Prematurity",
  "Cyanosis_at_Birth", "Bradycardia_at_Birth", "Apnea_at_Birth",
  "Neonatal_Convulsions", "Jaundice_Treatment", "Oxygen_Required",
  "Blood_Transfusion", "Rh_Incompatibility", "Planned_Pregnancy",
  "Nausea_Vomiting", "Heavy_Bleeding", "Pre_eclampsia",
  "Gallbladder_Attack", "Proteinuria", "Rubella",
  "Anemia", "Infection", "Diabetes",
  "Hypertension", "Placental_Complications", "Accident_Injury",
  "Alcohol_Pre", "Alcohol_During", "Cocaine_Pre",
  "Cocaine_During", "Marijuana_Pre", "Marijuana_During",
  "Tobacco_Pre", "Tobacco_During", "Opioids_Pre",
  "Opioids_During", "Rx_Opioids_Pre", "Rx_Opioids_During",
  "Prenatal_Vitamins", "Breastfeeding_Duration", "Motor_Development",
  "Speech_Development", "income", "Waist",
  "BMI", "Family_ID", "Ethnicity",
  "Race", "Sex", "age",
  "Education_P", "scanner_site", "ID_wave",
  "participant_id_73", "session_id", "BAS_Drive_04A",
  "FUN_Seeking_04A", "Reward_Responsiveness_04A", "BIS_Sum_04A",
  "Psychosis_04A", "Negative_Urgency_04A", "Lack_of_Planning_04A",
  "Sensation_Seeking_04A", "Positive_Urgency_04A", "Lack_of_Perseverance_04A",
  "Mania_04A", "OCD_04A", "Sluggish_Cognitive_Tempo_04A",
  "Stress_04A", "Aggressive_04A", "Anxiety_Depression_04A",
  "Attention_04A", "Externalizing_04A", "Internalizing_04A",
  "Other_Problems_04A", "Rule_Breaking_04A", "Social_04A",
  "Somatic_04A", "Thought_04A", "Withdrawn_Depression_04A",
  "Total_04A", "ID_wave_1", "participant_id_1",
  "session_id_1", "BAS_Drive_00A", "FUN_Seeking_00A",
  "Reward_Responsiveness_00A", "BIS_Sum_00A", "Psychosis_00A",
  "Negative_Urgency_00A", "Lack_of_Planning_00A", "Sensation_Seeking_00A",
  "Positive_Urgency_00A", "Lack_of_Perseverance_00A", "Mania_00A",
  "OCD_00A", "Sluggish_Cognitive_Tempo_00A", "Stress_00A",
  "Aggressive_00A", "Anxiety_Depression_00A", "Attention_00A",
  "Externalizing_00A", "Internalizing_00A", "Other_Problems_00A",
  "Rule_Breaking_00A", "Social_00A", "Somatic_00A",
  "Thought_00A", "Withdrawn_Depression_00A", "Total_00A",
  "PC1_pace", "group", "PC1_status"
)

predictors <- c(
  "Pubertal_Stage", "Planned_Pregnancy", "income", "sleep2",
  "sleep1", "screen_time", "Waist", "BMI", "Maternal_Age"
)

outcomes  <- c("Total_04A", "Psychosis_04A", "Mania_04A", "Internalizing_04A")
baselines <- c("Total_00A", "Psychosis_00A", "Mania_00A", "Internalizing_00A")
covariates <- c("age", "Sex", "Ethnicity", "Race")
mediator  <- "PC1_pace"

baseline_dict <- setNames(baselines, outcomes)

# Data quality check
message("=== Data quality check ===")
all_vars <- unique(c(predictors, mediator, outcomes, baselines, covariates))
for (v in all_vars) {
  n_na  <- sum(is.na(dat[[v]]))
  n_inf <- sum(is.infinite(dat[[v]]), na.rm = TRUE)
  if (n_na > 0 || n_inf > 0)
    message(sprintf("  %-30s  NA: %d  Inf: %d  valid: %d", v, n_na, n_inf, sum(is.finite(dat[[v]]))))
}
message("===========================")

run_mediation <- function(iv, dv) {

  baseline <- baseline_dict[[dv]]
  needed_vars <- c(iv, mediator, dv, baseline, covariates)
  df <- dat[, needed_vars]
  n_total      <- nrow(df)
  finite_mask  <- Reduce("&", lapply(df, is.finite))
  df           <- df[finite_mask, ]
  n_removed    <- n_total - nrow(df)
  if (n_removed > 0) message(iv, " -> ", dv, ": removed ", n_removed, " subjects with NA/Inf values (", nrow(df), " remaining)")

  cov_str   <- paste(covariates, collapse = " + ")
  formula_m <- as.formula(paste(mediator, "~", iv, "+", cov_str, "+", baseline))
  formula_y <- as.formula(paste(dv, "~", iv, "+", mediator, "+", cov_str, "+", baseline))

  model_m <- lm(formula_m, data = df)
  model_y <- lm(formula_y, data = df)

  # Point estimates
  a      <- coef(model_m)[iv]
  b      <- coef(model_y)[mediator]
  cprime <- coef(model_y)[iv]

  indirect_est <- a * b
  total_est    <- cprime + indirect_est

  # Direct and total p-values from lm
  direct_p <- summary(model_y)$coefficients[iv,      4]
  total_lm <- lm(as.formula(paste(dv, "~", iv, "+", cov_str, "+", baseline)), data = df)
  total_p  <- summary(total_lm)$coefficients[iv, 4]

  prop_mediated <- indirect_est / total_est

  # Bootstrap percentile CI + p-value for indirect effect and proportion mediated
  set.seed(42)
  boot_results <- replicate(5000, {
    idx        <- sample(nrow(df), replace = TRUE)
    bdf        <- df[idx, ]
    bm_fit     <- lm(formula_m, data = bdf)
    by_fit     <- lm(formula_y, data = bdf)
    ba         <- as.numeric(coef(bm_fit)[iv])
    bb         <- as.numeric(coef(by_fit)[mediator])
    bcprime    <- as.numeric(coef(by_fit)[iv])
    bind       <- ba * bb
    btotal     <- bcprime + bind
    c(as.numeric(bind), as.numeric(bind / btotal))
  })
  rownames(boot_results) <- c("indirect", "prop")

  boot_indirect <- boot_results["indirect", ]
  boot_prop     <- boot_results["prop", ]

  ci_lower   <- quantile(boot_indirect, 0.025, names = FALSE)
  ci_upper   <- quantile(boot_indirect, 0.975, names = FALSE)
  indirect_p <- 2 * min(mean(boot_indirect > 0), mean(boot_indirect < 0))

  prop_ci_lower <- quantile(boot_prop, 0.025, names = FALSE)
  prop_ci_upper <- quantile(boot_prop, 0.975, names = FALSE)

  tibble(
    Predictor        = iv,
    Outcome          = dv,
    N                = nrow(df),

    Indirect         = indirect_est,
    Indirect_p       = indirect_p,
    Indirect_CI_L    = ci_lower,
    Indirect_CI_U    = ci_upper,

    Direct           = cprime,
    Direct_p         = direct_p,

    Total            = total_est,
    Total_p          = total_p,

    Prop_Mediated    = prop_mediated,
    Prop_CI_L        = prop_ci_lower,
    Prop_CI_U        = prop_ci_upper
  )
}

mediation_results <- map_dfr(predictors, function(iv) {
  map_dfr(outcomes, function(dv) {
    tryCatch(
      run_mediation(iv, dv),
      error = function(e) {
        message("Failed: ", iv, " -> ", dv)
        message("  Error: ", conditionMessage(e))
        message("  Traceback: ", paste(capture.output(traceback()), collapse = "\n"))
        NULL
      }
    )
  })
})

if (nrow(mediation_results) == 0) {
  stop("All mediation models failed — check the error messages above.")
}

mediation_results <- mediation_results %>%
  mutate(
    Indirect_FDR = p.adjust(Indirect_p, method = "fdr"),
    Direct_FDR   = p.adjust(Direct_p,   method = "fdr"),
    Total_FDR    = p.adjust(Total_p,    method = "fdr")
  ) %>%
  mutate(
    Indirect_sig = ifelse(Indirect_FDR < 0.05, "Yes", "No"),
    Direct_sig   = ifelse(Direct_FDR   < 0.05, "Yes", "No"),
    Total_sig    = ifelse(Total_FDR    < 0.05, "Yes", "No")
  )

print(mediation_results)

write_csv(mediation_results, "output/mediation_results_medpkg.csv")
