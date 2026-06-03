library(dplyr)
library(purrr)
library(readr)

data_path <- "output/finalpace.csv"
dat <- read_csv(data_path, show_col_types = FALSE)

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
  "Pubertal_Stage", "income", "sleep1", "Waist", "BMI"
)

outcomes   <- c("Total_04A", "Psychosis_04A", "Mania_04A", "Internalizing_04A")
baselines  <- c("Total_00A", "Psychosis_00A", "Mania_00A", "Internalizing_00A")
covariates <- c("age", "Sex", "Ethnicity", "Race")
mediator   <- "PC1_pace"

baseline_dict <- setNames(baselines, outcomes)

# All predictors + covariates + mediator in one model per outcome.
# Each indirect effect = unique contribution of that predictor through PC1_pace,
# controlling for all other predictors.

run_multipred_mediation <- function(dv) {

  baseline    <- baseline_dict[[dv]]
  needed_vars <- c(predictors, mediator, dv, baseline, covariates)
  df          <- dat[, needed_vars]
  finite_mask <- Reduce("&", lapply(df, is.finite))
  df          <- df[finite_mask, ]
  n_removed   <- nrow(dat) - nrow(df)
  if (n_removed > 0) message(dv, ": removed ", n_removed, " subjects (", nrow(df), " remaining)")

  cov_str    <- paste(covariates, collapse = " + ")
  base_preds <- setdiff(predictors, c("Waist", "BMI"))

  # Model A: Waist included, BMI excluded — tests Waist and base predictors
  preds_A    <- c(base_preds, "Waist")
  formula_mA <- as.formula(paste(mediator, "~", paste(preds_A, collapse=" + "), "+", cov_str, "+", baseline))
  formula_yA <- as.formula(paste(dv, "~", paste(preds_A, collapse=" + "), "+", mediator, "+", cov_str, "+", baseline))
  formula_tA <- as.formula(paste(dv, "~", paste(preds_A, collapse=" + "), "+", cov_str, "+", baseline))
  model_mA   <- lm(formula_mA, data = df)
  model_yA   <- lm(formula_yA, data = df)
  model_tA   <- lm(formula_tA, data = df)
  b_A        <- coef(model_yA)[mediator]

  # Model B: BMI included, Waist excluded — tests BMI
  preds_B    <- c(base_preds, "BMI")
  formula_mB <- as.formula(paste(mediator, "~", paste(preds_B, collapse=" + "), "+", cov_str, "+", baseline))
  formula_yB <- as.formula(paste(dv, "~", paste(preds_B, collapse=" + "), "+", mediator, "+", cov_str, "+", baseline))
  formula_tB <- as.formula(paste(dv, "~", paste(preds_B, collapse=" + "), "+", cov_str, "+", baseline))
  model_mB   <- lm(formula_mB, data = df)
  model_yB   <- lm(formula_yB, data = df)
  model_tB   <- lm(formula_tB, data = df)
  b_B        <- coef(model_yB)[mediator]

  # Bootstrap both models simultaneously
  set.seed(42)
  boot_list <- replicate(5000, {
    idx    <- sample(nrow(df), replace = TRUE)
    bdf    <- df[idx, ]

    bm_A   <- lm(formula_mA, data = bdf)
    bm_yA  <- lm(formula_yA, data = bdf)
    bb_A   <- as.numeric(coef(bm_yA)[mediator])
    inds_A <- sapply(preds_A, function(p) as.numeric(coef(bm_A)[p]) * bb_A)

    bm_B   <- lm(formula_mB, data = bdf)
    bm_yB  <- lm(formula_yB, data = bdf)
    bb_B   <- as.numeric(coef(bm_yB)[mediator])

    c(inds_A, as.numeric(coef(bm_B)["BMI"]) * bb_B)
  }, simplify = TRUE)
  rownames(boot_list) <- c(preds_A, "BMI")

  map_dfr(predictors, function(iv) {
    if (iv == "BMI") {
      model_m_use <- model_mB
      model_y_use <- model_yB
      model_t_use <- model_tB
      b_use       <- b_B
    } else {
      model_m_use <- model_mA
      model_y_use <- model_yA
      model_t_use <- model_tA
      b_use       <- b_A
    }

    a            <- coef(model_m_use)[iv]
    cprime       <- coef(model_y_use)[iv]
    indirect_est <- a * b_use
    total_est    <- cprime + indirect_est
    direct_p     <- summary(model_y_use)$coefficients[iv, 4]
    total_p      <- summary(model_t_use)$coefficients[iv, 4]

    boot_ind   <- boot_list[iv, ]
    ci_lower   <- quantile(boot_ind, 0.025, names = FALSE)
    ci_upper   <- quantile(boot_ind, 0.975, names = FALSE)
    indirect_p <- 2 * min(mean(boot_ind > 0), mean(boot_ind < 0))

    tibble(
      Predictor     = iv,
      Outcome       = dv,
      N             = nrow(df),
      Indirect      = indirect_est,
      Indirect_p    = indirect_p,
      Indirect_CI_L = ci_lower,
      Indirect_CI_U = ci_upper,
      Direct        = cprime,
      Direct_p      = direct_p,
      Total         = total_est,
      Total_p       = total_p
    )
  })
}

results <- map_dfr(outcomes, function(dv) {
  tryCatch(
    run_multipred_mediation(dv),
    error = function(e) {
      message("Failed: ", dv, " | ", conditionMessage(e))
      NULL
    }
  )
})

results <- results %>%
  filter(!(Predictor %in% c("BMI", "Pubertal_Stage") & Outcome == "Psychosis_04A")) %>%
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

print(results)
write_csv(results, "output/mediation_multipred_results.csv")
cat("Saved: mediation_multipred_results.csv\n")
