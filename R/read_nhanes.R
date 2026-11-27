
library(here)
library(haven)
library(readr)
library(dplyr)

read_cycle <- function(row){
  demo = read_xpt(here("data", "raw", paste0(row$DEMO, ".xpt"))) %>% 
    select(SEQN, SDDSRVYR, RIAGENDR, RIDAGEYR, RIDSTATR, RIDRETH1, SDMVSTRA, SDMVPSU, WTMEC2YR, DMDEDUC2, INDFMPIR)
  HbA1c = read_xpt(here("data", "raw", paste0(row$HbA1c, ".xpt"))) %>% 
    select(SEQN, LBXGH)
  BMX = read_xpt(here("data", "raw", paste0(row$BMX, ".xpt"))) %>% 
    select(SEQN, BMXBMI)
  SMQ = read_xpt(here("data", "raw", paste0(row$SMQ, ".xpt"))) %>% 
    select(SEQN, SMQ020, SMQ040)
  DIQ = read_xpt(here("data", "raw", paste0(row$DIQ, ".xpt"))) 
  # In some cycles (2005-2008) the diabetic pills question is named DID070
  if ("DID070" %in% names(DIQ)) DIQ <- rename(DIQ, DIQ070 = DID070)
  DIQ <- select(DIQ, SEQN, DIQ010, DIQ050, DIQ070)
  
  
  if(row$CRP == ""){
    # no CRP for this cycle: table of NA for all participants
    CRP <- tibble(SEQN = demo$SEQN, LBXCRP = NA_real_)
  }else if(grepl("HSCRP", row$CRP)){
    # hs-CRP, already in mg/L, variable LBXHSCRP
    CRP = read_xpt(here("data", "raw", paste0(row$CRP, ".xpt"))) %>% 
      select(SEQN, LBXHSCRP) %>% 
      rename("LBXCRP" = "LBXHSCRP")
  }else{
    CRP = read_xpt(here("data", "raw", paste0(row$CRP, ".xpt"))) %>% 
      select(SEQN, LBXCRP) %>% 
      mutate(LBXCRP = 10 * LBXCRP)
  }
  
  mort = read_fwf(file=here("data", "raw", paste0("NHANES_", gsub("-", "_", row$cycle), "_MORT_2019_PUBLIC.dat")),
                  col_types = "iiiiiiii",
                  fwf_cols(SEQN = c(1,6),
                           ELIGSTAT = c(15,15),
                           MORTSTAT = c(16,16),
                           UCOD_LEADING = c(17,19),
                           DIABETES = c(20,20),
                           HYPERTEN = c(21,21),
                           PERMTH_INT = c(43,45),
                           PERMTH_EXM = c(46,48)
                  ),
                  na = c("", ".")
  ) %>% 
    select(SEQN, ELIGSTAT, MORTSTAT, UCOD_LEADING, PERMTH_EXM)
  left_join(demo, HbA1c, by = "SEQN") %>% 
    left_join(CRP, by = "SEQN") %>%
    left_join(BMX, by = "SEQN") %>%
    left_join(SMQ, by = "SEQN") %>%
    left_join(DIQ, by = "SEQN") %>%
    left_join(mort, by = "SEQN")
}