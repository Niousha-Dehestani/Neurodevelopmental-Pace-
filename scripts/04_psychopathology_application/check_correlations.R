library(dplyr)
library(readr)

data_path <- "/Users/nioushad/Documents/Doc_p/myJupyter/normative modeling/code/component/release6/hpc/finalpaper/output/finalpace.csv"
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
  "Pubertal_Stage", "income",
  "sleep1", "Waist", "BMI",
)

df <- dat[, predictors]
finite_mask <- Reduce("&", lapply(df, is.finite))
df <- df[finite_mask, ]
cat("N after removing NA/Inf:", nrow(df), "\n\n")

cor_mat  <- cor(df, use = "pairwise.complete.obs")
cor_r    <- round(cor_mat, 3)

cat("=== Pearson Correlation Matrix ===\n")
print(cor_r)

# p-values
pval_mat <- matrix(NA, nrow = length(predictors), ncol = length(predictors),
                   dimnames = list(predictors, predictors))
for (i in seq_along(predictors)) {
  for (j in seq_along(predictors)) {
    if (i != j) {
      test <- cor.test(df[[predictors[i]]], df[[predictors[j]]], use = "complete.obs")
      pval_mat[i, j] <- test$p.value
    }
  }
}

cat("\n=== Pairs with |r| > 0.3 ===\n")
for (i in 1:(length(predictors) - 1)) {
  for (j in (i + 1):length(predictors)) {
    r <- cor_mat[i, j]
    if (abs(r) > 0.3) {
      cat(sprintf("  %s -- %s:  r = %.3f  p = %.4f\n",
                  predictors[i], predictors[j], r, pval_mat[i, j]))
    }
  }
}

write.csv(as.data.frame(cor_r),
          "/Users/nioushad/Documents/papers/normative/predictor_correlations.csv")
cat("\nSaved: predictor_correlations.csv\n")
