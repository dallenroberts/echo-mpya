################################################################################
## Allen Roberts
## Load and clean ECHO-Kisumu data
################################################################################
here::i_am("R/clean_echo_data.R")

library(here)
library(tidyverse)
library(lubridate)

## Eligibility - note that only 901 met eligibility criteria
elig <- read_csv(
  here("raw", "KEMRI_Lumumba_fnl_CRF_csv_2019-07-29", "data001.csv")
)

## Enrollment - only 901 enrolled
enroll <- read_csv(
  here("raw", "KEMRI_Lumumba_fnl_CRF_csv_2019-07-29", "data070.csv")
)

## Baseline demographics
dem <- read_csv(
  here("raw", "KEMRI_Lumumba_fnl_CRF_csv_2019-07-29", "data090.csv")
)

dem <- dem %>%
  filter(DEM_VisitCode == 999) %>% ## Keep only those who were enrolled
  mutate(dob = dmy(DEM_DOB),
         visit_date = dmy(DEM_VisitDate),
         age = ifelse(is.na(DEM_DOB), DEM_Age, interval(dob, visit_date)/years(1)),
         married = ifelse(DEM_Married %in% c("married (monogamous)", "married (polygamous)"), 1, 0),
         lives_with_partner = ifelse(DEM_Living == "Yes", 1, 0),
         partner_support = ifelse(DEM_PartnerSupport == "Yes", 1, 0),
         highest_education = case_when(
           DEM_HighestEducation %in% c("No schooling", "Primary school, not complete") ~ "Less than primary",
           DEM_HighestEducation %in% c("Primary school, complete", "Secondary school, not complete") ~ "Primary",
           DEM_HighestEducation %in% c("Secondary school, complete", "Attended post-secondary school") ~ "Secondary"),
         highest_education = factor(highest_education, levels = c("Less than primary", "Primary", "Secondary")),
         drinks_alcohol = ifelse(DEM_NoDrinks == "Checked", 0, 1),
         weekly_alcohol = ifelse(DEM_NoDrinks == "Unchecked" & DEM_WeeklyDrinks >= 1, 1, 0),
         has_mobile_phone = ifelse(DEM_MobilePhone == "Yes", 1, 0),
         earns_income = ifelse(DEM_EarnIncome == "Yes", 1, 0)
         ) %>%
  select(PTID,
         DEM_VisitCode,
         visit_date, 
         age,
         married,
         lives_with_partner,
         partner_support,
         highest_education,
         drinks_alcohol,
         weekly_alcohol,
         has_mobile_phone,
         earns_income)

## Sexual behavior
ba1 <- read_csv(
  here("raw", "KEMRI_Lumumba_fnl_CRF_csv_2019-07-29", "data221.csv")
)

ba1 <- ba1 %>%
  mutate(visit_date = dmy(BA1_VisitDate),
         visit_code = BA1_VisitCode,
         seroconverter = ifelse(BA1_Seroconverter == "Checked", 1, 0),
         has_pp_past3mo = ifelse(BA1_Past3MoPrimarySexPtnr == "Yes", 1, 0),
         had_vag_sex_pp_past3mo = case_when(
           BA1_Past3MoVagSexPrimaryPtnr == "Yes" ~ 1L,
           BA1_Past3MoVagSexPrimaryPtnr == "No" ~ 0L,
           has_pp_past3mo == 0 ~ NA_integer_),
         pp_circumcised = case_when(
           BA1_PrimaryPtnrCircumcised == "yes" ~ 1L,
           BA1_PrimaryPtnrCircumcised %in% c("no") ~ 0L,
           has_pp_past3mo == 0 ~ NA_integer_,
           BA1_PrimaryPtnrCircumcised == "don't know" ~ NA_integer_),
         pp_hiv_status = case_when(
           BA1_HIVStatusPrimaryPtnr == "yes" ~ "Negative",
           BA1_HIVStatusPrimaryPtnr == "no" ~ "Positive",
           BA1_HIVStatusPrimaryPtnr == "don't know" ~ "Unsure",
           BA1_HIVStatusPrimaryPtnr == "0" ~ NA_character_),
         pp_on_art = case_when(
           BA1_PrimaryPtnrTakingARVs == "yes" ~ "Yes",
           BA1_PrimaryPtnrTakingARVs == "no" ~ "No",
           BA1_PrimaryPtnrTakingARVs == "don't know" ~ "Unsure",
           TRUE ~ NA_character_),
         pp_same_past3mo = case_when(
           BA1_SamePrimarySexPtnrPast3Mo == "Yes" ~ 1L,
           BA1_SamePrimarySexPtnrPast3Mo == "No" ~ 0L,
           has_pp_past3mo == 0 ~ NA_integer_),
         pp_sex_with_others = case_when(
           BA1_PrimaryPtnrSexWOthers == "yes" ~ "Yes",
           BA1_PrimaryPtnrSexWOthers == "no" ~ "No",
           BA1_PrimaryPtnrSexWOthers == "don't know" ~ "Unsure",
           has_pp_past3mo == 0 ~ NA_character_),
         num_partners_past3mo = case_when(
           BA1_TotalSexPtnrsPast3Mo == 0 ~ "0",
           BA1_TotalSexPtnrsPast3Mo == 1 ~ "1",
           BA1_TotalSexPtnrsPast3Mo > 1 ~ "2+",
           TRUE ~ NA_character_),
         new_partner_past3mo = case_when(
           BA1_NewSexPtnerPast3Mo == "Yes" ~ 1L,
           BA1_NewSexPtnerPast3Mo == "No" ~ 0L,
           TRUE ~ NA_integer_)
         ) %>%
  select(PTID,
         visit_date,
         visit_code,
         seroconverter,
         has_pp_past3mo,
         had_vag_sex_pp_past3mo,
         pp_circumcised,
         pp_hiv_status,
         pp_on_art,
         pp_same_past3mo,
         pp_sex_with_others,
         num_partners_past3mo,
         new_partner_past3mo)

ba2 <- read_csv(
  here("raw", "KEMRI_Lumumba_fnl_CRF_csv_2019-07-29", "data222.csv")
)

ba2 <- ba2 %>%
  mutate(visit_date = dmy(BA2_VisitDate),
         visit_code = BA2_VisitCode,
         has_sex_for_money = case_when(
           BA2_SexForMoney == "Yes" ~ 1L,
           BA2_SexForMoney == "No" ~ 0L,
           TRUE ~ NA_integer_
         ),
         num_vag_sex_acts_last3mo = BA2_TotalTimesVagSex,
         condom_use_last3mo = case_when(
           BA2_CondomUsed == 0 ~ NA_character_,
           TRUE ~ BA2_CondomUsed
         ),
         condom_use_last3mo = factor(condom_use_last3mo,
                                     levels = c("never", "rarely", "sometimes", "often", "always")),
         num_vag_sex_acts_last7days = case_when(
           num_vag_sex_acts_last3mo == 0 ~ 0,
           TRUE ~ BA2_Past7DaysVagSex),
         num_vag_sex_acts_with_condom_last7days = case_when(
           num_vag_sex_acts_last7days == 0 ~ NA_real_,
           TRUE ~ BA2_Past7DaysCondom),
         last_vag_sex = case_when(
          num_vag_sex_acts_last3mo == 0 ~ ">= 14 days ago",
          TRUE ~ BA2_LastTimeVagSex),
         last_vag_sex = factor(last_vag_sex,
                               levels = c(">= 14 days ago",
                                          "8-13 days ago",
                                          "5-7 days ago",
                                          "4 days ago",
                                          "3 days ago",
                                          "2 days ago",
                                          "yesterday",
                                          "today")),
         used_condom_last_vag_sex = case_when(
           BA2_LastTimeCondomUsed == "Yes" ~ 1L,
           BA2_LastTimeCondomUsed == "No" ~ 0L,
           TRUE ~ NA_integer_),
         had_anal_sex_last3mo = ifelse(BA2_AnalSex == 0, 0, 1),
         used_condom_last_anal_sex = case_when(
           had_anal_sex_last3mo == 0 ~ NA_integer_,
           BA2_AnalSexCondomUsed == "Yes" ~ 1L,
           BA2_AnalSexCondomUsed == "No" ~ 0L)
         ) %>%
  select(PTID,
         visit_date,
         visit_code,
         has_sex_for_money,
         num_vag_sex_acts_last3mo,
         condom_use_last3mo,
         num_vag_sex_acts_last7days,
         num_vag_sex_acts_with_condom_last7days,
         last_vag_sex,
         used_condom_last_vag_sex,
         had_anal_sex_last3mo,
         used_condom_last_anal_sex)

behav <- ba1 %>%
  left_join(ba2, by = c("PTID", "visit_date", "visit_code"))

## STI screening
sti <- read_csv(
  here("raw", "KEMRI_Lumumba_fnl_CRF_csv_2019-07-29", "data170.csv")
)
sti <- sti %>%
  mutate(visit_date = dmy(LAB_VisitDate),
         visit_code = LAB_VisitCode,
         gonorrhea_positive = case_when(
           LAB_NGonorrhoeae == "positive" ~ 1L,
           LAB_NGonorrhoeae == "negative" ~ 0L,
           LAB_NGonorrhoeae %in% c("0", "not done") ~ NA_integer_),
         chlamydia_positive = case_when(
           LAB_CTrachomatis == "positive" ~ 1L,
           LAB_CTrachomatis == "negative" ~ 0L,
           LAB_CTrachomatis %in% c("0", "not done") ~ NA_integer_)
         ) %>%
  select(PTID,
         visit_date,
         visit_code,
         gonorrhea_positive,
         chlamydia_positive)

## HSV-2
hsv <- read_csv(
  here("raw", "KEMRI_Lumumba_fnl_CRF_csv_2019-07-29", "HSV2_EIAresults.csv")
)

hsv <- hsv %>%
  mutate(hsv_collection_date = dmy(SS_Colldate),
         visit_code = SS_visitcode,
         hsv_serostatus = case_when(
           ELISA < 0.9 ~ "Negative",
           ELISA >= 0.9 & ELISA <= 3.5 ~ "Indeterminate",
           ELISA > 3.5 ~ "Positive")
         ) %>%
  select(PTID,
         hsv_collection_date,
         visit_code,
         hsv_serostatus)

## PrEP use
prep <- read_csv(
  here("raw", "KEMRI_Lumumba_fnl_CRF_csv_2019-07-29", "data101.csv")
)

prep <- prep %>%
  mutate(visit_date = dmy(VS_VisitDate),
         visit_code = VS_VisitCode,
         visit_type = VS_VisitType,
         using_prep = ifelse(VS_PrEPUse == "yes", 1, 0)) %>%
  select(PTID,
         visit_date,
         visit_code,
         visit_type,
         using_prep)

## Save         
save(dem, behav, sti, prep, hsv,
     file = here("output", "data", "echo_clean.RData"))
