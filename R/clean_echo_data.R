################################################################################
## Allen Roberts
## Load and clean ECHO-Kisumu data
################################################################################
here::i_am("R/clean_echo_data.R")

library(here)
library(tidyverse)

dem <- read_csv(
  here("raw", "KEMRI_Lumumba_fnl_CRF_csv_2019-07-29", "data090.csv")
)

dem %>%
  select(PTID, 
         DEM_VisitDate, 
         DEM_DOB,
         DEM_Age,
         DEM_Married,
         DEM_Living,
         DEM_PartnerSupport,
         DEM_HighestEducation,
         DEM_EthnicCode,
         DEM_WeeklyDrinks,
         DEM_NoDrinks,
         DEM_MobilePhone,
         DEM_EarnIncome,
         DEM_IncomeSource)