
library(dplyr)

derive_variable <- function(df_all){
  # recode acronyms
  df_all <- df_all %>% 
    mutate(
      sex = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
      
      race_eth = factor(RIDRETH1, levels = 1:5,
                        labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White",
                                   "Non-Hispanic Black", "Other / multiracial")),
      
      education = factor(DMDEDUC2, levels = 1:5,
                         labels = c("< 9th grade", "9-11th grade", "High school / GED",
                                    "Some college / AA", "College graduate or above")),
      
      # smoking: never / former / current (SMQ040 is only asked if SMQ020 == 1)
      smoking = case_when(
        SMQ020 == 2                          ~ "Never",
        SMQ020 == 1 & SMQ040 == 3            ~ "Former",
        SMQ020 == 1 & SMQ040 %in% c(1, 2)    ~ "Current",
        TRUE                                 ~ NA_character_
      ),
      smoking = factor(smoking, levels = c("Never", "Former", "Current")),
      
      diabetes_dx = factor(DIQ010, levels = c(1, 2, 3), labels = c("Yes", "No", "Borderline")),
      insulin     = factor(DIQ050, levels = c(1, 2),    labels = c("Yes", "No")),
      diab_pills  = factor(DIQ070, levels = c(1, 2),    labels = c("Yes", "No")),
      
      log_crp = log(LBXCRP),
      crp_cat = cut(LBXCRP, breaks = c(-Inf, 1, 3, Inf), right = FALSE,
                    labels = c("< 1 mg/L", "1-3 mg/L", ">= 3 mg/L"))
    )
  
  # calculate weights
  df_all %>% 
    mutate(wt_hba1c = case_when(
      is.na(WTMEC4YR) ~ 1 / 10 * WTMEC2YR, # 10 cycles availables
      TRUE ~ 2 / 10 * WTMEC4YR),
      wt_crp = case_when(
        is.na(WTMEC4YR) ~ 1 / 8 * WTMEC2YR, # 8 cycles: CRP not measured in cycles 7-8
        TRUE ~ 2 / 8 * WTMEC4YR)
    )  
  
}
