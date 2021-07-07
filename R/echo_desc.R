################################################################################
## Allen Roberts
## ECHO-Kisumu baseline descriptive statistics
################################################################################
here::i_am("R/echo_desc.R")

library(here)
library(tidyverse)
library(viridis)
library(viridisLite)

options(dplyr.summarise.inform=FALSE)

## Load cleaned ECHO data
load(here("output", "data", "echo_clean.RData"))

sti_comb <- sti %>%
  filter(visit_code == 999) %>%
  select(PTID, gonorrhea_positive, chlamydia_positive) %>%
  left_join(hsv, by = "PTID") %>%
  filter(visit_code == 1000) %>%
  select(PTID, gonorrhea_positive, chlamydia_positive, hsv_serostatus)

used_prep_during_study <- prep %>%
  group_by(PTID) %>%
  summarise(ever_used_prep_during_study = any(using_prep == 1))

all_vars <- behav %>%
  group_by(PTID) %>%
  mutate(seroconverter = any(seroconverter == 1)) %>%
  filter(visit_code == 1000) %>%
  select(-visit_date) %>%
  right_join(dem, by = "PTID") %>%
  right_join(sti_comb, by = "PTID") %>%
  right_join(used_prep_during_study, by = "PTID") %>%
  mutate(age_group = cut(age, breaks = c(0, 20, 25, 30, 35), right = FALSE),
         more_than_one_partner_past3mo = case_when(
           num_partners_past3mo <= 1 ~ 0L,
           num_partners_past3mo > 1 ~ 1L,
           is.na(num_partners_past3mo) ~ NA_integer_),
         voice_score =  2*(age < 25) +
                        2*(married == 0 & lives_with_partner == 0) +
                        drinks_alcohol +
                        partner_support +
                        2*!(has_pp_past3mo == 0 | pp_sex_with_others == "No"),
         voice_score_cat = cut(voice_score, 
                               breaks = c(0, 5, Inf), 
                               labels = c("< 5", "5+"),
                               right = FALSE))

## Save baseline dataset
save(all_vars,
     file = here("output", "data", "echo_baseline_clean.RData")
)

## Generate descriptive statistics table with the correct variable and value orderings
table_vars <- all_vars %>%
  select(PTID,
         age_group,
         married,
         lives_with_partner,
         highest_education,
         earns_income,
         drinks_alcohol,
         has_pp_past3mo,
         more_than_one_partner_past3mo,
         new_partner_past3mo,
         pp_sex_with_others,
         condom_use_last3mo,
         used_condom_last_vag_sex,
         has_sex_for_money,
         gonorrhea_positive,
         chlamydia_positive,
         hsv_serostatus,
         voice_score_cat,
         ever_used_prep_during_study) 

for(ii in 2:ncol(table_vars)) {
  
  sub <- table_vars %>%
    select(1, all_of(ii)) %>%
    pivot_longer(cols = -PTID, names_to = "variable", values_to = "value") %>%
    group_by(variable, value) %>%
    summarise(n = n()) %>%
    group_by(variable) %>%
    mutate(pct = round(100*n/sum(n), 1),
           value = as.character(value))
  
  if(ii == 2) {
    
    desc_table <- sub
    
  } else {
    
    desc_table <- bind_rows(desc_table, sub)
    
  }
}

write_csv(
  desc_table,
  here("output", "tables", "echo_descriptive_statistics.csv")
)

## Plot PrEP use over time
prep_plot_time <- prep %>%
  mutate(using_prep = factor(using_prep, levels = c(0, 1), labels = c("No", "Yes"))) %>%
  ggplot(aes(x = visit_date, group = using_prep)) +
    geom_histogram(aes(fill = factor(using_prep))) +
    scale_fill_viridis_d(name = "Reports using PrEP", begin = 0.25, end = 0.75) +
    labs(x = "Visit Date", y = "Count") +
    theme_classic()
  
ggsave(
  filename = here("output", "figures", "echo_prep_use_over_time.pdf"),
  plot = prep_plot_time,
  height = 6, width = 8, units = "in"
)
