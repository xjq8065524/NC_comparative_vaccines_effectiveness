# Load R packages ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(data.table) # for fread function
# Vaccination cohorts from GP EMIS prescription datasource (England) ----------------------------------
# England test results
covid19_result_england <- 
  fread( input = "D:/DPhil/UK_Biobank_opioid_application/COVID_test_results/Version_20211205/covid19_result_england.txt") %>% 
  mutate( specdate = lubridate::dmy( specdate))

#===========================the EMIS source===========================#
covid19_emis_gp_clinical <- 
  fread( input = "D:/DPhil/UK_Biobank_opioid_application/GP_covid_only/Version_20211209/covid19_emis_gp_clinical.txt") %>% 
  mutate( event_dt = lubridate::dmy(event_dt),
          code = as.character(code))

print(object.size(covid19_emis_gp_clinical), units = "Gb")

covid19_emis_gp_scripts <- 
  fread( input = "D:/DPhil/UK_Biobank_opioid_application/GP_covid_only/Version_20211209/covid19_emis_gp_scripts.txt") %>% 
  mutate( issue_date = lubridate::dmy(issue_date),
          code = as.character(code))

print(object.size(covid19_emis_gp_scripts), units = "Gb")

# UKBB England participants
Population_characteristics___Baseline_characteristics___Indices_of_Multiple_Deprivation <- readRDS("D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/R_datasets/Main_dataset/Population_characteristics___Baseline_characteristics___Indices_of_Multiple_Deprivation.rds")
England_participants_ID <- filter( Population_characteristics___Baseline_characteristics___Indices_of_Multiple_Deprivation, !is.na(f.26410.0.0))


# vaccine_codes
vaccine_dm_codes <- c( "Janssen-Cilag Ltd" = "39230211000001104",
                       "Moderna, Inc" = "39326911000001101",
                       "Medicago Inc" = "39826711000001101",
                       "Baxter Oncology GmbH" = "39473011000001103",
                       "AstraZeneca UK Ltd" = "39114911000001105",
                       "Valneva UK Ltd" = "39373511000001104",
                       "Pfizer Ltd" = "39115611000001103")

vaccine_cohort_extract_func <- function( vaccine_code_list){
  
  vaccine_cohort_emis_scripts <- 
    covid19_emis_gp_scripts  %>% 
    filter( code %in% vaccine_code_list) %>% 
    rename( dmd_code = code) %>% 
    select( -code_type) %>%  #attention: different coding systems
    mutate( dmd_code = factor( dmd_code, 
                               levels = c("39230211000001104", "39326911000001101", "39826711000001101", "39473011000001103", "39114911000001105", "39373511000001104", "39115611000001103"),
                               labels = c("Janssen", "Moderna", "Medicago", "Baxter", "AstraZeneca", "Valneva","Pfizer"))) 
  
  return( vaccine_cohort_emis_scripts)
  
}

vaccine_cohort_raw <- vaccine_cohort_extract_func( vaccine_code_list = vaccine_dm_codes)

vaccine_cohort_raw %>% summarise( across( .cols = everything(), ~ sum(is.na(.)))) #missing value check

vaccine_cohort <- 
  vaccine_cohort_raw %>% 
  arrange( issue_date) %>% 
  group_by( eid) %>% 
  mutate( dose_seq = row_number()) %>% 
  ungroup() %>% 
  select( eid, dmd_code, issue_date, dose_seq) %>% 
  pivot_wider( id_cols = eid,
               names_from = dose_seq, 
               values_from = c( issue_date, dmd_code)) 


table(vaccine_cohort_raw$dmd_code)

write_rds( vaccine_cohort_raw, "D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/Sub_project/Vac_effect/Derived_datasets/NC_second_round/vaccine_cohort_raw.rds")
write_rds( vaccine_cohort, "D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/Sub_project/Vac_effect/Derived_datasets/NC_second_round/vaccine_cohort.rds")

# Demographic information from recruitment survey  ----------------------------------------------
Population_characteristics___Baseline_characteristics <- readRDS("D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/R_datasets/Main_dataset/Population_characteristics___Baseline_characteristics.rds")
Population_characteristics___Baseline_characteristics___Indices_of_Multiple_Deprivation <- readRDS("D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/R_datasets/Main_dataset/Population_characteristics___Baseline_characteristics___Indices_of_Multiple_Deprivation.rds")
UK_Biobank_Assessment_Centre___Touchscreen___Sociodemographics___Education <- readRDS("D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/R_datasets/Main_dataset/UK_Biobank_Assessment_Centre___Touchscreen___Sociodemographics___Education.rds")
UK_Biobank_Assessment_Centre___Touchscreen___Sociodemographics___Ethnicity <- readRDS("D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/R_datasets/Main_dataset/UK_Biobank_Assessment_Centre___Touchscreen___Sociodemographics___Ethnicity.rds")
UK_Biobank_Assessment_Centre___Physical_measures___Anthropometry___Body_size_measures <- readRDS("D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/R_datasets/Main_dataset/UK_Biobank_Assessment_Centre___Physical_measures___Anthropometry___Body_size_measures.rds")


UK_Biobank_Assessment_Centre___Touchscreen___Sociodemographics___Ethnicity %>% 
  summarise( across( .cols = everything(), ~ mean(is.na(.))*100))

# extract sex and age
baseline_sex_birth <- 
  Population_characteristics___Baseline_characteristics %>% 
  select( f.eid, f.31.0.0, f.34.0.0, f.189.0.0) %>% 
  rename( sex = f.31.0.0, birth_year = f.34.0.0, downsend_deprivation_index = f.189.0.0) 

# extract arthropometry such as BMI
baseline_BMI <- 
  UK_Biobank_Assessment_Centre___Physical_measures___Anthropometry___Body_size_measures %>% 
  select( f.eid, f.21001.0.0) %>% 
  rename( BMI = f.21001.0.0)

# extract Multiple Deprivation
baseline_Index_of_Multiple_Deprivation <- 
  Population_characteristics___Baseline_characteristics___Indices_of_Multiple_Deprivation %>% 
  select( f.eid, f.26410.0.0:f.26416.0.0) %>% 
  rename( IMD = f.26410.0.0, 
          IMD_income_score = f.26411.0.0,
          IMD_employment_score = f.26412.0.0,
          IMD_health_score = f.26413.0.0,
          IMD_education_score = f.26414.0.0,
          IMD_housing_score = f.26415.0.0,
          IMD_crime_score = f.26416.0.0) 

# extract Education
baseline_education <- 
  UK_Biobank_Assessment_Centre___Touchscreen___Sociodemographics___Education %>% 
  select( f.eid, f.6138.0.0) %>% 
  rename( Education = f.6138.0.0) %>% 
  mutate( Education = factor(Education, ordered = FALSE))

# extract Ethnicity
coding <- reader::reader("D:\\DPhil\\UK_Biobank_opioid_application\\Raw_main_dataset\\coding1001.tsv")
vbs_ext_1 <- colnames(fread("D:\\DPhil\\UK_Biobank_opioid_application\\Raw_main_dataset\\ukb45017.tab", header=TRUE, sep="\t", nrows=1 ))
selected_vars <-c("f.eid",
                  grep("f.21000.",vbs_ext_1, value = TRUE),
                  grep("f.26410.",vbs_ext_1, value = TRUE),
                  grep("f.31.",vbs_ext_1, value = TRUE),
                  grep("f.34.",vbs_ext_1, value = TRUE),
                  grep("f.52.",vbs_ext_1, value = TRUE))

extract1 <- fread("D:\\DPhil\\UK_Biobank_opioid_application\\Raw_main_dataset\\ukb45017.tab",
                  header=TRUE, sep="\t", select = selected_vars)

baseline_ethnicity <- 
  extract1 %>%
  filter(!is.na(f.26410.0.0)) %>%
  select(c(f.eid,f.21000.0.0,f.21000.1.0,f.21000.2.0)) %>%
  mutate(f.21000.0.0=coalesce(f.21000.2.0,f.21000.1.0,f.21000.0.0)) %>%
  left_join(coding, by = c("f.21000.0.0" = "coding")) %>%
  left_join(coding, by = c("parent_id" = "coding")) %>%
  mutate(ethnicity=coalesce(meaning.y,meaning.x)) %>% 
  select( f.eid, ethnicity)


write_rds( baseline_ethnicity, "D:/DPhil/UK_Biobank_opioid_application/Albert_deriviation/baseline_ethnicity.rds")
  
#combined baseline charateristics
recruitment_charateristics <- 
  reduce( list( baseline_sex_birth, baseline_BMI, baseline_Index_of_Multiple_Deprivation, baseline_education, baseline_ethnicity), 
          left_join, by = "f.eid")%>% 
  labelled::remove_var_label()


#write_rds( recruitment_charateristics, "D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/Sub_project/Vac_effect/Derived_datasets/recruitment_charateristics.rds")


glimpse(Index_of_Multiple_Deprivation)
labelled::var_label( Index_of_Multiple_Deprivation)

# Hosptial admission number from HES --------------------------------------
hes_admission <- 
  hesin %>% 
  select( eid, admidate) %>% 
  arrange( eid, admidate) %>% 
  group_by( eid) %>% 
  distinct( admidate) %>% 
  ungroup()

write_rds( hes_admission, "D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/Sub_project/Vac_effect/Derived_datasets/hes_admission.rds")


# COVID-19 test results ---------------------------------------------------
covid19_result_england %>%  summarise( across( .cols = everything(), ~ sum(is.na(.))))#missing value check
 
#valid cohorts
first_dose_vaccine_cohort <- vaccine_cohort %>% filter( !is.na( dmd_code_1))
second_dose_vaccine_cohort <- vaccine_cohort %>% filter( !is.na( dmd_code_2))

#postive covid-19 testing results
All_covid_positive_result <- covid19_result_england %>% filter( result == 1)

# generate outcome index and follow-up days
first_dose_outcome_extract_func <- function( cohort , prop, lastest_update_date){
  
  start_time <- Sys.time()
  
  sample <- sample_frac( cohort, prop)
  print(names( sample))
  
  # link to covid test results and define follow-up period for the first dose cohort
  sample <- 
    sample %>% 
    #attention: the same date of first and second dose
    #attention: na.rm = true
    mutate( max_follow_up_date = pmin( issue_date_1 + 7*14, issue_date_2, lastest_update_date, na.rm = TRUE) + 1) 
  
  # infection outcome
  infection_outcome_cases <- 
    sample %>% 
    left_join( select( All_covid_positive_result, eid, specdate, result), by = "eid") %>% 
    filter( specdate > issue_date_1, specdate <= max_follow_up_date) %>% 
    arrange( specdate) %>% 
    group_by( eid) %>% 
    filter( row_number() == 1) %>% 
    select( eid, specdate, result)%>% 
    ungroup()
  
  infection_cohort <- 
    sample %>% 
    left_join( infection_outcome_cases, by = "eid") %>% 
    group_by( eid) %>% 
    mutate( result = case_when( is.na( result) ~ as.integer(0), TRUE ~ result),
            specdate = case_when( is.na( specdate) ~ max_follow_up_date, TRUE ~ specdate)) %>% 
    mutate( follow_up_days = (specdate - issue_date_1) + 1) %>% 
    ungroup()
  
  #hospitalization outcome
  hospitalization_outcome_cases <- 
    sample %>% 
    left_join( select( All_covid_positive_result, eid, specdate, result, origin), by = "eid") %>% 
    filter( specdate > issue_date_1, specdate <= max_follow_up_date, origin == 1) %>% 
    arrange( specdate) %>% 
    group_by( eid) %>% 
    filter( row_number() == 1) %>% 
    select( eid, specdate, result) %>% 
    ungroup()
  
  
  hospitalization_cohort <- 
    sample %>% 
    left_join( hospitalization_outcome_cases, by = "eid") %>% 
    group_by( eid) %>% 
    mutate( result = case_when( is.na( result) ~ as.integer(0), TRUE ~ result),
            specdate = case_when( is.na( specdate) ~ max_follow_up_date, TRUE ~ specdate)) %>% 
    mutate( follow_up_days = (specdate - issue_date_1) + 1) %>% 
    ungroup()
       
  
  combined <- list( infection_event = infection_cohort,
                    hospitalization_event = hospitalization_cohort)
  
  end_time <- Sys.time()
  
  print(end_time - start_time)
  
  return(combined)
  
} 
second_dose_outcome_extract_func <- function( cohort , prop, lastest_update_date){
  
  start_time <- Sys.time()
  
  sample <- sample_frac( cohort, prop)
  print(names( sample))
  
  # link to covid test results and define follow-up period for the first dose cohort
  sample <- 
    sample %>% 
    #attention: the same date of first and second dose
    #attention: na.rm = true
    #attention: no 14 weeks follow-up restriction for second dose
    mutate( max_follow_up_date = pmin( issue_date_3, lastest_update_date, na.rm = TRUE) + 1) 
  
  # infection outcome
  infection_outcome_cases <- 
    sample %>% 
    left_join( select( All_covid_positive_result, eid, specdate, result), by = "eid") %>% 
    filter( specdate > issue_date_2, specdate <= max_follow_up_date) %>% 
    arrange( specdate) %>% 
    group_by( eid) %>% 
    filter( row_number() == 1) %>% 
    select( eid, specdate, result)%>% 
    ungroup()
  
  infection_cohort <- 
    sample %>% 
    left_join( infection_outcome_cases, by = "eid") %>% 
    group_by( eid) %>% 
    mutate( result = case_when( is.na( result) ~ as.integer(0), TRUE ~ result),
            specdate = case_when( is.na( specdate) ~ max_follow_up_date, TRUE ~ specdate)) %>% 
    mutate( follow_up_days = (specdate - issue_date_2) + 1) %>% 
    ungroup()
  
  #hospitalization outcome
  hospitalization_outcome_cases <- 
    sample %>% 
    left_join( select( All_covid_positive_result, eid, specdate, result, origin), by = "eid") %>% 
    filter( specdate > issue_date_2, specdate <= max_follow_up_date, origin == 1) %>% 
    arrange( specdate) %>% 
    group_by( eid) %>% 
    filter( row_number() == 1) %>% 
    select( eid, specdate, result) %>% 
    ungroup()
  
  
  hospitalization_cohort <- 
    sample %>% 
    left_join( hospitalization_outcome_cases, by = "eid") %>% 
    group_by( eid) %>% 
    mutate( result = case_when( is.na( result) ~ as.integer(0), TRUE ~ result),
            specdate = case_when( is.na( specdate) ~ max_follow_up_date, TRUE ~ specdate)) %>% 
    mutate( follow_up_days = (specdate - issue_date_2) + 1) %>% 
    ungroup()
  
  
  combined <- list( infection_event = infection_cohort,
                    hospitalization_event = hospitalization_cohort)
  
  end_time <- Sys.time()
  
  print(end_time - start_time)
  
  return(combined)
  
} 

first_dose_link_outcome_cohort <- first_dose_outcome_extract_func( cohort = first_dose_vaccine_cohort, 
                                                                   prop = 1,
                                                                   lastest_update_date = as.Date("2021/10/18"))

second_dose_link_outcome_cohort <- second_dose_outcome_extract_func( cohort = second_dose_vaccine_cohort, 
                                                                     prop = 1,
                                                                     lastest_update_date = as.Date("2021/10/18"))

write_rds( first_dose_link_outcome_cohort, "D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/Sub_project/Vac_effect/Derived_datasets/NC_second_round/first_dose_link_outcome_cohort.rds")
write_rds( second_dose_link_outcome_cohort, "D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/Sub_project/Vac_effect/Derived_datasets/NC_second_round/second_dose_link_outcome_cohort.rds")

# curate counting cox regression format
#library(magrittr)
library(survival)
first_dose_construct_data_func <- function( cohort, vaccine, outcome, follow_up){
  
  
  sample <- cohort[[outcome]]
  
  dose_base <-
    sample %>%
    select( eid, all_of(vaccine), result, specdate, .data[[follow_up]]) %>%
    mutate( interval_total_number = case_when((.data[[follow_up]]/14) < 1 ~ 1,
                                              (.data[[follow_up]]/14) >= 1 & (.data[[follow_up]]/14) < 3 ~ 2,
                                              (.data[[follow_up]]/14) >= 3 & (.data[[follow_up]]/14) < 5 ~ 3,
                                              #(.data[[follow_up]]/14) >= 5 & (.data[[follow_up]]/14) < 7 ~ 4,
                                              # (.data[[follow_up]]/14) >= 4 & (.data[[follow_up]]/14) < 5 ~ 5,
                                              # (.data[[follow_up]]/14) >= 5 & (.data[[follow_up]]/14) < 6 ~ 6,
                                              (.data[[follow_up]]/14) >= 5  ~ 4))


  # Create an index of the rows you want with duplications
  temp_idx <- rep(1:nrow( dose_base), dose_base$interval_total_number)

  # Use that index to genderate your new data frame
  dose_base_dup <-
    dose_base[temp_idx,] %>%
    group_by( eid) %>%
    mutate( interval_index = row_number(),
            interval_days = case_when( interval_index == 1 ~ 0,
                                       interval_index == 2 ~ 14,
                                       interval_index == 3 ~ 42,
                                       interval_index == 4 ~ 70))

  
  dose_base_range <- tmerge( dose_base, dose_base, id = eid, outcome_status = event( dose_base[[follow_up]], result))  #set range
  
  counting_process <- tmerge(data1 = dose_base_range,
                                   data2 = dose_base_dup,
                                   id = eid,
                                   follow_cat = tdc( interval_days, interval_index)) %>%
    mutate( follow_cat = as.character(follow_cat))
  
  counting_process_ref <-
    counting_process %>%
    mutate( follow_cat_ref = case_when( .data[[vaccine]] == "AstraZeneca" ~ "0", TRUE ~ follow_cat))
  
  return( counting_process_ref)
  
}
second_dose_construct_data_func <- function( cohort, vaccine, outcome, follow_up){
  
  
  sample <- cohort[[outcome]]
  
  dose_base <-
    sample %>%
    select( eid, all_of(vaccine), result, specdate, .data[[follow_up]]) %>%
    mutate( interval_total_number = case_when((.data[[follow_up]]/28) < 1 ~ 1,
                                              (.data[[follow_up]]/28) >= 1 & (.data[[follow_up]]/28) < 2 ~ 2,
                                              (.data[[follow_up]]/28) >= 2 & (.data[[follow_up]]/28) < 3 ~ 3,
                                              (.data[[follow_up]]/28) >= 3 & (.data[[follow_up]]/28) < 4 ~ 4,
                                              (.data[[follow_up]]/28) >= 4 & (.data[[follow_up]]/28) < 5 ~ 5,
                                              (.data[[follow_up]]/28) >= 5 & (.data[[follow_up]]/28) < 6 ~ 6,
                                              (.data[[follow_up]]/28) >= 6  ~ 7))
  
  
  # Create an index of the rows you want with duplications
  temp_idx <- rep(1:nrow( dose_base), dose_base$interval_total_number)
  
  # Use that index to genderate your new data frame
  dose_base_dup <- 
    dose_base[temp_idx,] %>% 
    group_by( eid) %>% 
    mutate( interval_index = row_number(),
            interval_days = (interval_index - 1) * 28 )
  
  
  dose_base_range <- tmerge( dose_base, dose_base, id = eid, outcome_status = event( dose_base[[follow_up]], result))  #set range
  
  counting_process <- tmerge(data1 = dose_base_range,
                             data2 = dose_base_dup,
                             id = eid,
                             follow_cat = tdc( interval_days, interval_index)) %>%
    mutate( follow_cat = as.character(follow_cat))
  
  counting_process_ref <-
    counting_process %>%
    mutate( follow_cat_ref = case_when( .data[[vaccine]] == "AstraZeneca" ~ "0", TRUE ~ follow_cat))
  
  return( counting_process_ref)
  
}

first_dose_counting_infection_cohort <- first_dose_construct_data_func( cohort = first_dose_link_outcome_cohort, 
                                                                          outcome = "infection_event",
                                                                          follow_up = "follow_up_days",
                                                                          vaccine = "dmd_code_1")


first_dose_counting_hospitalization_cohort <- first_dose_construct_data_func( cohort = first_dose_link_outcome_cohort, 
                                                                   outcome = "hospitalization_event",
                                                                   follow_up = "follow_up_days",
                                                                   vaccine = "dmd_code_1")


second_dose_counting_infection_cohort <- second_dose_construct_data_func( cohort = second_dose_link_outcome_cohort, 
                                                                          outcome = "infection_event",
                                                                          follow_up = "follow_up_days",
                                                                          vaccine = "dmd_code_2")


second_dose_counting_hospitalization_cohort <- second_dose_construct_data_func( cohort = second_dose_link_outcome_cohort, 
                                                                   outcome = "hospitalization_event",
                                                                   follow_up = "follow_up_days",
                                                                   vaccine = "dmd_code_2")

# Curate baseline covariates ----------------------------------------------
link_baseline_func <- function( cohort, index_date){
  
  
  sample <- cohort
  
  
  # previous infection status
  infection_cases <- 
    sample %>% 
    left_join( select( All_covid_positive_result, eid, specdate, result), by = "eid") %>% 
    filter( specdate <= .data[[index_date]]) %>% 
    arrange( specdate) %>% 
    group_by( eid) %>% 
    filter( row_number() == 1) %>% 
    mutate( previous_infection_status = "1") %>% 
    select( eid, previous_infection_status)%>% 
    ungroup()
  
  # link to GP medication
  GP_medication_long <- 
    sample %>% 
    select( eid, all_of(index_date)) %>% 
    left_join( emis_gp_medication, by = "eid") %>%
    filter( issue_date <= .data[[index_date]] , issue_date > .data[[index_date]] - 365*3) %>% # medication history within 3 years before index date
    left_join( all_medication_code, by = "dmd_code") %>% 
    left_join( candidate_medication_list_dpa, by = c("top_level" = "Top_level_medication_classes"))
  
  GP_medication_long_wide <<- # to environment
    GP_medication_long %>% 
    group_by( eid, Manual_category) %>% 
    filter( row_number() == 1) %>% 
    ungroup() %>% 
    mutate( medication = 1) %>% 
    pivot_wider( id_cols = eid,names_from = Manual_category, values_from = medication) %>% 
    janitor::clean_names()
  
  drug_names <- setdiff( names(GP_medication_long_wide), "eid")
  
  #link to GP diagnosis
  GP_diagnosis_long <- 
    sample %>% 
    select( eid, all_of( index_date)) %>% 
    left_join( emis_gp_clinical, by = "eid") %>%
    filter( event_dt <= .data[[index_date]] ) %>% 
    left_join( all_clinical_emis_code, by = c("code" = "SnomedCTConceptId")) 
  
  GP_diagnosis_long %>% summarise( across( .cols = everything(), ~ mean(is.na(.))*100))
  
  GP_diagnosis_long_wide <<- 
    GP_diagnosis_long %>% 
    group_by( eid, second_level ) %>% 
    filter( row_number() == 1) %>% 
    ungroup() %>% 
    mutate( diagnosis = 1) %>% 
    pivot_wider( id_cols = eid,names_from = second_level, values_from = diagnosis) %>% 
    janitor::clean_names()
  
  #link to HES admission
  HES_admission <- 
    sample %>% 
    select( eid, all_of(index_date)) %>% 
    left_join( hes_admission, by = "eid") %>%
    filter( admidate <= .data[[index_date]] , admidate > .data[[index_date]] - 365*3) %>% 
    group_by( eid) %>% 
    summarise( admission_num = n()) %>% 
    ungroup()
  
  
  temp <- 
    sample %>% 
    select( eid, all_of(index_date)) %>% 
    left_join( infection_cases, by = "eid") %>% 
    left_join( GP_medication_long_wide, by = "eid") %>% 
    left_join( GP_diagnosis_long_wide, by = "eid") %>% 
    left_join( HES_admission, by = "eid") %>% 
    left_join( recruitment_charateristics, by = c( "eid" = "f.eid")) %>% 
    mutate( previous_infection_status = case_when( is.na(previous_infection_status) ~ "0", TRUE ~ previous_infection_status)) %>% 
    mutate( across( .cols = all_of( setdiff(names(GP_medication_long_wide), "eid")), ~ case_when( is.na(.) ~ 0, TRUE ~ .)),
            across( .cols = all_of( setdiff(names(GP_medication_long_wide), "eid")), ~ as.factor(.))) %>% 
    mutate( across( .cols = all_of( setdiff(names(GP_diagnosis_long_wide), "eid")), ~ case_when( is.na(.) ~ 0, TRUE ~ .)),
            across( .cols = all_of( setdiff(names(GP_diagnosis_long_wide), "eid")), ~ as.factor(.))) %>% 
    mutate( admission_num = case_when( is.na(admission_num) ~ as.integer(0), TRUE ~ admission_num)) %>% 
    mutate( age = year( .data[[index_date]]) - birth_year) %>%  
    mutate( across( starts_with( "BMI"), ~ case_when( is.na(.) ~ mean(., na.rm = TRUE), TRUE ~ .)), # missing data imputation
            across( starts_with( "IMD"), ~ case_when( is.na(.) ~ mean(., na.rm = TRUE), TRUE ~ .)),
            across( Education, ~ case_when( !is.na(.) ~ ., TRUE ~ as.factor("Prefer not to answer"))),
            across( ethnicity, ~ case_when( !is.na(.) ~ ., TRUE ~ "Prefer not to answer"))) %>% 
    mutate( calendar_scale_week = as.factor(isoweek( .data[[index_date]])), # epi calendar week, starting from monday rather than sunday 
            calendar_scale_num = isoweek( .data[[index_date]])) %>% 
    select( -all_of(index_date))
  
  
  print(summarise( temp, across( everything(), ~ sum( is.na(.)))))
  
  return( temp)
  
}

first_dose_baseline_covariates <- link_baseline_func( cohort = first_dose_vaccine_cohort, index_date = "issue_date_1")
second_dose_baseline_covariates <- link_baseline_func( cohort = second_dose_vaccine_cohort,  index_date = "issue_date_2")

#write_rds( first_dose_baseline_covariates, "D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/Sub_project/Vac_effect/Derived_datasets/first_dose_baseline_covariates.rds")
#write_rds( second_dose_baseline_covariates, "D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/Sub_project/Vac_effect/Derived_datasets/second_dose_baseline_covariates.rds")

########################Association analysis#############
# First dose cohot Baseline assessment and PS weighting -----------------------------
first_dose_eligible <- 
  first_dose_vaccine_cohort %>% 
  filter( issue_date_1 >= as.Date("2021/1/11"),
          issue_date_1 <= as.Date("2021/2/28"),
          dmd_code_1 %in% c("AstraZeneca", "Pfizer"))

# align baseline charateristics 
first_dose_align_baseline_cohort <- 
  first_dose_eligible %>% 
  select( eid, issue_date_1, dmd_code_1) %>% 
  left_join( first_dose_baseline_covariates, by = "eid") %>% 
  mutate( dmd_code_1 = fct_drop( dmd_code_1),
          calendar_scale_week = fct_drop( calendar_scale_week)) #important step for counting process

first_dose_align_baseline_cohort_dummy <-  # dummy baseline variables  
  fastDummies::dummy_cols( first_dose_align_baseline_cohort, select_columns = c( "calendar_scale_week", "Education","ethnicity")) %>% 
  mutate( across(starts_with("calendar_scale_week_"), ~ as.factor(.x))) %>% 
  mutate( across(starts_with("Education_"), ~ as.factor(.x))) %>% 
  mutate(across(starts_with("ethnicity_"), ~ as.factor(.x))) %>% 
  janitor::clean_names()


first_dose_covariates_list <- c(str_subset( names(first_dose_align_baseline_cohort_dummy), "calendar_scale_week_"), "previous_infection_status", "age", "sex", "bmi", 
                                "imd", "imd_income_score", "imd_employment_score","imd_health_score", "imd_education_score",  "imd_housing_score","imd_crime_score",
                                str_subset( names(first_dose_align_baseline_cohort_dummy), "ethnicity_"), 
                                str_subset( names(first_dose_align_baseline_cohort_dummy), "education_"),
                                setdiff( names( GP_medication_long_wide), "eid"),
                                "admission_num",
                                setdiff( names( GP_diagnosis_long_wide), "eid")) %>% unique()

print( first_dose_covariates_list)

# PS weighting
library( survey)
library( tableone)

first_dose_ps_func <- function( input_data, proportion, model_covariate, vaccine){
  
  set.seed(1)
    
    sample <- sample_frac( input_data, proportion)
    
    print(summarise( sample, across( all_of( model_covariate), ~ sum( is.na(.)))))
    
    sample <- 
      sample%>% 
      mutate( exposure = case_when( .data[[vaccine]] %in% "AstraZeneca" ~ 0, .data[[vaccine]] %in%  "Pfizer" ~ 1)) %>% 
      filter( !is.na(exposure))
    
    print( table(sample$exposure))
    
    PS_model <- glm( reformulate( termlabels = model_covariate , response = "exposure"), family  = binomial(link = "logit"), data = sample)
    
    # data with probability
    PS_probability <- mutate( sample, 
                              P_treatment = PS_model$fitted.values, 
                              P_control = 1 - PS_model$fitted.values,
                              PS_weights = case_when( exposure == 1 ~ 1/P_treatment, TRUE ~ 1/P_control))
    
    PS_probability_trim <-  
      PS_probability %>% 
      filter( P_treatment <= quantile( P_treatment, 0.99), P_treatment >= quantile( P_treatment, 0.01)) 

    
    ## Weighted data format
    PS_IPTW <- survey::svydesign( ids = ~ 1, data = PS_probability, weights = ~ PS_weights)
    PS_IPTW_trim <- survey::svydesign( ids = ~ 1, data = PS_probability_trim, weights = ~ PS_weights)
    
    return( list( PS_probability = PS_probability, 
                  PS_probability_trim = PS_probability_trim,
                  PS_IPTW = PS_IPTW,
                  PS_IPTW_trim = PS_IPTW_trim))
    
  }
  
first_dose_cohort_weighted <- first_dose_ps_func( input_data = first_dose_align_baseline_cohort_dummy, 
                                                   proportion = 1, 
                                                   model_covariate = first_dose_covariates_list, 
                                                   vaccine = "dmd_code_1")

table( first_dose_cohort_weighted$PS_probability_trim$dmd_code_1)

# second dose cohot Baseline assessment and PS weighting -----------------------------
second_dose_eligible <- 
  second_dose_vaccine_cohort %>% 
  filter( issue_date_2 >= as.Date("2021/3/22"),
          issue_date_2 <= as.Date("2021/5/09"),
          dmd_code_2 %in% c("AstraZeneca", "Pfizer"))

# align baseline charateristics 
second_dose_align_baseline_cohort <- 
  second_dose_eligible %>% 
  select( eid, issue_date_2, dmd_code_2) %>% 
  left_join( second_dose_baseline_covariates, by = "eid") %>% 
  mutate( dmd_code_2 = fct_drop( dmd_code_2),
          calendar_scale_week = fct_drop( calendar_scale_week)) #important step for counting process

second_dose_align_baseline_cohort_dummy <-  # dummy baseline variables  
  fastDummies::dummy_cols( second_dose_align_baseline_cohort, select_columns = c("calendar_scale_week", "Education","ethnicity")) %>% 
  mutate( across(starts_with("calendar_scale_week_"), ~ as.factor(.x))) %>% 
  mutate( across(starts_with("Education_"), ~ as.factor(.x))) %>% 
  mutate(across(starts_with("ethnicity_"), ~ as.factor(.x))) %>% 
  janitor::clean_names()

second_dose_covariates_list <- c(str_subset( names(second_dose_align_baseline_cohort_dummy), "calendar_scale_week_"), "previous_infection_status", "age", "sex", "bmi", 
                                "imd", "imd_income_score", "imd_employment_score","imd_health_score", "imd_education_score",  "imd_housing_score","imd_crime_score",
                                str_subset( names(second_dose_align_baseline_cohort_dummy), "ethnicity_"), 
                                str_subset( names(second_dose_align_baseline_cohort_dummy), "education_"),
                                setdiff(names(GP_medication_long_wide), "eid"),
                                "admission_num",
                                setdiff(names(GP_diagnosis_long_wide), "eid")) %>% unique()

print( second_dose_covariates_list)

# PS weighting
library( survey)
library( tableone)

second_ps_func <- function( input_data, proportion, model_covariate, vaccine){
  
  set.seed(1)
  
  sample <- sample_frac( input_data, proportion)
  
  print(summarise( sample, across( all_of( model_covariate), ~ sum( is.na(.)))))
  
  sample <- 
    sample%>% 
    mutate( exposure = case_when( .data[[vaccine]] %in% "AstraZeneca" ~ 0, .data[[vaccine]] %in%  "Pfizer" ~ 1)) %>% 
    filter( !is.na(exposure))
  
  print( table(sample$exposure))
  
  PS_model <- glm( reformulate( termlabels = model_covariate , response = "exposure"), family  = binomial(link = "logit"), data = sample)
  
  # data with probability
  PS_probability <- mutate( sample, 
                            P_treatment = PS_model$fitted.values, 
                            P_control = 1 - PS_model$fitted.values,
                            PS_weights = case_when( exposure == 1 ~ 1/P_treatment, TRUE ~ 1/P_control))
  
  PS_probability_trim <-  
    PS_probability %>% 
    filter( P_treatment <= quantile( P_treatment, 0.99), P_treatment >= quantile( P_treatment, 0.01)) 
  
  ## Weighted data format
  PS_IPTW <- survey::svydesign( ids = ~ 1, data = PS_probability, weights = ~ PS_weights)
  PS_IPTW_trim <- survey::svydesign( ids = ~ 1, data = PS_probability_trim, weights = ~ PS_weights)
  
  return( list( PS_probability = PS_probability, 
                PS_probability_trim = PS_probability_trim,
                PS_IPTW = PS_IPTW,
                PS_IPTW_trim = PS_IPTW_trim))
  
}

second_dose_cohort_weighted <- second_ps_func(input_data = second_dose_align_baseline_cohort_dummy, 
                                               proportion = 1, 
                                               model_covariate = second_dose_covariates_list, 
                                               vaccine = "dmd_code_2")

summary_after_IPTW <- 
  tableone::svyCreateTableOne( data = second_dose_cohort_weighted$PS_IPTW, 
                               vars = second_dose_covariates_list, 
                               includeNA = FALSE,
                               strata = "exposure",
                               test = FALSE) 

# Construct a baseline table 1 --------------------------------------------
first_dose_table <- function( df, sub_df, sub_df_weight){
  
  summary_before_IPTW <- 
    tableone::CreateTableOne( data = df[[sub_df]], 
                              vars = first_dose_covariates_list, 
                              includeNA = FALSE,
                              strata = "exposure",
                              test = FALSE) 
  
  summary_after_IPTW <- 
    tableone::svyCreateTableOne( data = df[[sub_df_weight]], 
                                 vars = first_dose_covariates_list, 
                                 includeNA = FALSE,
                                 strata = "exposure",
                                 test = FALSE) 
    
  first_dose_smd <- 
    data.frame(variable  = rownames( tableone::ExtractSmd( summary_before_IPTW)),
               before = as.numeric(tableone::ExtractSmd( summary_before_IPTW)),
               after = as.numeric(tableone::ExtractSmd( summary_after_IPTW))) 
  
  
  first_dose_baseline <<-
    print( summary_before_IPTW) %>%
    as_tibble( rownames = "rowname") %>%
    bind_cols( as_tibble(print( summary_after_IPTW)))
          
  
  return( first_dose_smd)
  
}  
second_dose_table <- function( df, sub_df, sub_df_weight){
  
  summary_before_IPTW <- 
    tableone::CreateTableOne( data = df[[sub_df]], 
                              vars = second_dose_covariates_list, 
                              includeNA = FALSE,
                              strata = "exposure",
                              test = FALSE) 
  
  summary_after_IPTW <- 
    tableone::svyCreateTableOne( data = df[[sub_df_weight]], 
                                 vars = second_dose_covariates_list, 
                                 includeNA = FALSE,
                                 strata = "exposure",
                                 test = FALSE) 
  
  second_dose_smd <- 
    data.frame(variable  = rownames( tableone::ExtractSmd( summary_before_IPTW)),
               before = as.numeric(tableone::ExtractSmd( summary_before_IPTW)),
               after = as.numeric(tableone::ExtractSmd( summary_after_IPTW)))
  
  
  second_dose_baseline <<-
    print( summary_before_IPTW) %>%
    as_tibble( rownames = "rowname") %>%
    bind_cols( as_tibble(print( summary_after_IPTW)))
  
  return( second_dose_smd)
  
  
} 

first_dose_smd <- first_dose_table( df = first_dose_cohort_weighted, sub_df = "PS_probability_trim", sub_df_weight = "PS_IPTW_trim")
second_dose_smd <- second_dose_table( df = second_dose_cohort_weighted, sub_df = "PS_probability_trim", sub_df_weight = "PS_IPTW_trim")

first_dose_name_list <- function(){
  #paste( first_dose_smd$variable, sep = "", collapse = '","')
  first_raw_names <- 
    c("calendar_scale_week_2",
      "calendar_scale_week_3",
      "calendar_scale_week_4",
      "calendar_scale_week_5",
      "calendar_scale_week_6",
      "calendar_scale_week_7",
      "calendar_scale_week_8",
      "previous_infection_status" ,
      "age","sex","bmi",
      "imd","imd_income_score",
      "imd_employment_score",
      "imd_health_score",
      "imd_education_score",
      "imd_housing_score",
      "imd_crime_score",
      "ethnicity_asian_or_asian_british",
      "ethnicity_black_or_black_british",
      "ethnicity_chinese",
      "ethnicity_do_not_know",
      "ethnicity_mixed",
      "ethnicity_other_ethnic_group",
      "ethnicity_prefer_not_to_answer",
      "ethnicity_white",
      "education_none_of_the_above",
      "education_prefer_not_to_answer",
      "education_college_or_university_degree",
      "education_a_levels_as_levels_or_equivalent",
      "education_o_levels_gcs_es_or_equivalent",
      "education_cs_es_or_equivalent",
      "education_nvq_or_hnd_or_hnc_or_equivalent",
      "education_other_professional_qualifications_eg_nursing_teaching",
      "ra_sinh","proton_pump_inhibitors",
      "systemic_glucocorticoids",
      "antithrombotics",
      "lipid_lowering_drugs",
      "antidepressants",
      "anticoagulants",
      "antihypertensives",
      "antidiabetes",
      "immunosuppressants_excl_corticosteroids",
      "antineoplastic_agents",
      "admission_num",
      "ch_pulmonary",
      "ch_cancer",
      "ch_cerebrovasc",
      "ch_renal",
      "ch_rheumatol",
      "ch_diabetes",
      "ch_dementia",
      "ch_diabetes_comp",
      "ch_mi",
      "ch_peripheralvasc",
      "ch_pepticulcer",
      "ch_liver_mod",
      "ch_heart_cong",
      "ch_cancer_meta",
      "ch_liver_mild",
      "ch_hemiplegia",
      "ch_aids")
  
  first_Var_labels_manual <- 
    c("Epi week_2",
      "Epi week_3",
      "Epi week_4",
      "Epi week_5",
      "Epi week_6",
      "Epi week_7",
      "Epi week_8",
      "Previous Covid-19 infection",
      "Age","Sex","BMI",
      "IMD",
      "Income",
      "Employment",
      "Health",
      "Education",
      "Housing",
      "Crime",
      "Asian or asian british",
      "Black or black british",
      "Chinese",
      "Do not know",
      "Mixed",
      "Other ethnic group",
      "Prefer not to answer (Race)",
      "White",
      "None of the_above",
      "Prefer not to answer",
      "College or university degree",
      "A levels as levels or equivalent",
      "O levels gcs es or equivalent",
      "Cs es or equivalent",
      "Nvq or hnd or hnc or equivalent",
      "Other professional qualifications",
      "RAS inhibitors",
      "Proton pump inhibitors",
      "Systemic glucocorticoids",
      "Antithrombotic",
      "Lipid lowering drugs",
      "Antidepressants",
      "Anticoagulants",
      "Other anti-hypertensives",
      "Diabetes medicines",
      "Immunosuppressants excl corticosteroids",
      "Antineoplastic agents",
      "Admission frequency",
      "COPD",
      "Cancer",
      "Cerebrovascular disease",
      "Chronic kidney disease",
      "Rheumatoid arthritis",
      "Diabetes (uncomplicated)",
      "Dementia",
      "Diabetes (end-organ damage)",
      "Myocardial infarction",
      "Peripheral vascular disease",
      "Peptic ulcer",
      "Liver disease (moderate to severe)",
      "Congestive heart failure",
      "Malignant cancer",
      "Liver disease (mild)",
      "Hemiplegia",
      "AIDS")
}
second_dose_name_list <- function(){
  
  second_raw_names <- 
    c("calendar_scale_week_12",
      "calendar_scale_week_13",
      "calendar_scale_week_14",
      "calendar_scale_week_15",
      "calendar_scale_week_16",
      "calendar_scale_week_17",
      "calendar_scale_week_18",
      "previous_infection_status" ,
      "age","sex","bmi",
      "imd","imd_income_score",
      "imd_employment_score",
      "imd_health_score",
      "imd_education_score",
      "imd_housing_score",
      "imd_crime_score",
      "ethnicity_asian_or_asian_british",
      "ethnicity_black_or_black_british",
      "ethnicity_chinese",
      "ethnicity_do_not_know",
      "ethnicity_mixed",
      "ethnicity_other_ethnic_group",
      "ethnicity_prefer_not_to_answer",
      "ethnicity_white",
      "education_none_of_the_above",
      "education_prefer_not_to_answer",
      "education_college_or_university_degree",
      "education_a_levels_as_levels_or_equivalent",
      "education_o_levels_gcs_es_or_equivalent",
      "education_cs_es_or_equivalent",
      "education_nvq_or_hnd_or_hnc_or_equivalent",
      "education_other_professional_qualifications_eg_nursing_teaching",
      "ra_sinh","proton_pump_inhibitors",
      "systemic_glucocorticoids",
      "antithrombotics",
      "lipid_lowering_drugs",
      "antidepressants",
      "anticoagulants",
      "antihypertensives",
      "antidiabetes",
      "immunosuppressants_excl_corticosteroids",
      "antineoplastic_agents",
      "admission_num",
      "ch_pulmonary",
      "ch_cancer",
      "ch_cerebrovasc",
      "ch_renal",
      "ch_rheumatol",
      "ch_diabetes",
      "ch_dementia",
      "ch_diabetes_comp",
      "ch_mi",
      "ch_peripheralvasc",
      "ch_pepticulcer",
      "ch_liver_mod",
      "ch_heart_cong",
      "ch_cancer_meta",
      "ch_liver_mild",
      "ch_hemiplegia",
      "ch_aids")
  
  second_Var_labels_manual <- 
    c("Epi week_12",
      "Epi week_13",
      "Epi week_14",
      "Epi week_15",
      "Epi week_16",
      "Epi week_17",
      "Epi week_18",
      "Previous Covid-19 infection",
      "Age","Sex","BMI",
      "IMD",
      "Income",
      "Employment",
      "Health",
      "Education",
      "Housing",
      "Crime",
      "Asian or asian british",
      "Black or black british",
      "Chinese",
      "Do not know",
      "Mixed",
      "Other ethnic group",
      "Prefer not to answer (Race)",
      "White",
      "None of the_above",
      "Prefer not to answer",
      "College or university degree",
      "A levels as levels or equivalent",
      "O levels gcs es or equivalent",
      "Cs es or equivalent",
      "Nvq or hnd or hnc or equivalent",
      "Other professional qualifications",
      "RAS inhibitors",
      "Proton pump inhibitors",
      "Systemic glucocorticoids",
      "Antithrombotic",
      "Lipid lowering drugs",
      "Antidepressants",
      "Anticoagulants",
      "Other anti-hypertensives",
      "Diabetes medicines",
      "Immunosuppressants excl corticosteroids",
      "Antineoplastic agents",
      "Admission frequency",
      "COPD",
      "Cancer",
      "Cerebrovascular disease",
      "Chronic kidney disease",
      "Rheumatoid arthritis",
      "Diabetes (uncomplicated)",
      "Dementia",
      "Diabetes (end-organ damage)",
      "Myocardial infarction",
      "Peripheral vascular disease",
      "Peptic ulcer",
      "Liver disease (moderate to severe)",
      "Congestive heart failure",
      "Malignant cancer",
      "Liver disease (mild)",
      "Hemiplegia",
      "AIDS")
  
}

smd_plot <- function(){
  
  first_temp <- 
    first_dose_smd %>% 
    mutate( Var_label = factor( variable, levels = first_raw_names, labels = first_Var_labels_manual)) %>% 
    mutate( Var_label = fct_reorder( Var_label, before)) 
  
  second_temp <- 
    second_dose_smd %>% 
    mutate( Var_label = factor( variable, levels = second_raw_names, labels = second_Var_labels_manual)) %>% 
    mutate( Var_label = fct_reorder( Var_label, before)) 
    
  plot_smd_df <- 
    bind_rows( first = first_temp, second = second_temp, .id = "Cohort") %>% 
    pivot_longer( cols = c( before, after), names_to = "Group", values_to = "Values") %>%  
    mutate( Var_groups = case_when( str_detect(variable, "calendar_scale") ~ "Epi weeks",
                                    str_detect(variable, "previous_infection_status") ~ " ",
                                    str_detect(variable, "antineoplastic_agents") ~ "Medications", #attention: put this condition above others
                                    str_detect(variable, "age|sex|bmi") ~ "Age\nSex\nBMI",
                                    str_detect(variable, "imd") ~ "Deprivation\nIndices",
                                    str_detect(variable, "ethnicity_") ~ "Ethnicity",
                                    str_detect(variable, "education_") ~ "Education",
                                    str_detect(variable, "ch_|admission_num") ~ "Commorbidities\nHospital admissions",
                                    TRUE ~ "Medications")) %>% 
    mutate( Var_groups = factor( Var_groups, levels = c( "Epi weeks"," ", "Age\nSex\nBMI", "Deprivation\nIndices",
                                                         "Ethnicity", "Education", "Medications", "Commorbidities\nHospital admissions"))) %>% 
    mutate( Group = factor( Group, levels = c( "after", "before"), labels = c("After weighting", "Before weighting"))) %>% 
    mutate( Cohort = factor( Cohort, levels = c( "first", "second"), labels = c("One-dose cohort", "Two-doses cohort")))  %>% 
    filter( Var_groups != "Epi weeks") 
  
  ## Plot using ggplot2
  SMD_plot <- 
  ggplot(data = plot_smd_df, mapping = aes( x = Var_label, y = Values, group = Var_label, color = Group)) +
    geom_line( size = 0.25) +
    geom_point( size = 0.6) +
    # scale_x_discrete( labels = dataPlot_longer$label)+
    scale_y_continuous( limits = c(0, 0.2)) +
    geom_hline( yintercept = 0.1, color = "black", size = 0.25) +
    guides( color = guide_legend(reverse = TRUE)) +
    coord_flip() +
    labs( x = "Baseline Characteristics", y = "Absolute standardized mean difference", color = "")+
    ggsci::scale_color_jama() +
    facet_grid( Var_groups ~ Cohort, scales = "free", space='free')+
    theme( 
      panel.background = element_blank(),
      text = element_text(  size = 7),
      axis.text.x = element_text( size = 6, vjust = 0.5),
      axis.line.x = element_line( size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line( size = 0.25),
      axis.ticks.y = element_line( size = 0.25),
      strip.background = element_blank(),
      strip.text.x  =  element_text( face = "bold"),
      strip.text.y  =  element_blank(),
      legend.key = element_rect( fill = "transparent"),
      legend.background = element_rect( fill='transparent', color = 0), #transparent legend bg
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,-5,-5,-5),
      legend.position = "bottom") 
  
  pdf("D:/Document_sync/OneDrive - Nexus365/DPhil_training/Manuscripts/Vaccine_comparative_effectiveness/Nature_communication/Second_round_revision/Figures/ASMD.pdf",
      width = 4.67,
      height = 6,
      family = "Helvetica")
  
  print(SMD_plot)
  
  dev.off()
  
}

baseline_table <- function(){
  
  first_baseline <- 
    first_dose_baseline %>% 
    janitor::clean_names() %>% 
    mutate( Var_label = factor( rowname, levels = first_raw_names, labels = first_Var_labels_manual)) 
  
  
  second_baseline <- 
    second_dose_baseline %>% 
    janitor::clean_names() %>% 
    mutate( Var_label = factor( rowname, levels = first_raw_names, labels = first_Var_labels_manual)) 
  
  write.table(second_baseline, "clipboard", sep="\t", row.names=FALSE)
}

# Display Age and BMI distribution ----------------------------------------
age_func <- function( df, sub_df, Vac){
  
  age_unweight <- 
    df[[sub_df]] %>% 
    mutate( age_cat = cut_interval( age, length = 5)) %>% 
    group_by( .data[[Vac]], age_cat) %>% 
    summarise( n = n()) %>% 
    group_by( .data[[Vac]]) %>% 
    mutate( perc = n/ sum( n))
  
  age_weight <- 
    df[[sub_df]] %>% 
    mutate( age_cat = cut_interval( age, length = 5)) %>% 
    group_by( .data[[Vac]], age_cat) %>% 
    summarise( n = sum( PS_weights)) %>% 
    group_by( .data[[Vac]]) %>% 
    mutate( perc = n/ sum( n))
  
  age_combine <- 
    bind_rows( age_unweight = age_unweight, age_weight = age_weight, .id = "weight") %>% 
    mutate( weight = factor( weight, levels = c("age_unweight", "age_weight"), labels = c( "Before weighting", "After weighting"))) %>% 
    janitor::clean_names()
  
  return(age_combine)
}
obsity_func <- function( df, sub_df, Vac){
  
  Obsity_unweight <- 
    df[[sub_df]] %>% 
    mutate( Obsity = case_when( bmi >= 30 ~ "Yes", TRUE ~ "No")) %>% 
    group_by( .data[[Vac]], Obsity) %>% 
    summarise( n = n()) %>% 
    group_by( .data[[Vac]]) %>% 
    mutate( perc = n/ sum( n))
  
  Obsity_weight <- 
    df[[sub_df]] %>% 
    mutate( Obsity = case_when( bmi >= 30 ~ "Yes", TRUE ~ "No")) %>% 
    group_by( .data[[Vac]], Obsity) %>% 
    summarise( n = sum( PS_weights)) %>% 
    group_by( .data[[Vac]]) %>% 
    mutate( perc = n/ sum( n))
  
  
  Obsity_combine <- 
    bind_rows( Obsity_unweight = Obsity_unweight, Obsity_weight = Obsity_weight, .id = "weight") %>% 
    mutate( weight = factor( weight, levels = c("Obsity_unweight", "Obsity_weight"), labels = c( "Before weighting", "After weighting"))) %>% 
    janitor::clean_names()
    
  
}

first_dose_age <- age_func( df = first_dose_cohort_weighted, sub_df = "PS_probability_trim", Vac = "dmd_code_1")
second_dose_age <- age_func( df = second_dose_cohort_weighted, sub_df = "PS_probability_trim", Vac = "dmd_code_2")

first_dose_obsity <- obsity_func( df = first_dose_cohort_weighted, sub_df = "PS_probability_trim", Vac = "dmd_code_1")
second_dose_obsity <- obsity_func( df = second_dose_cohort_weighted, sub_df = "PS_probability_trim", Vac = "dmd_code_2")


age_combine <- 
  bind_rows( first_dose = first_dose_age, second_dose = second_dose_age, .id = "cohort")  %>% 
  mutate( vaccine = case_when( is.na( dmd_code_1) ~ dmd_code_2,
                              TRUE ~ dmd_code_1)) %>% 
  mutate( cohort  = case_when( cohort == "first_dose" ~ "One-dose cohort",
                                   TRUE ~ "Two-doses cohort")) %>% 
  mutate( )

obsity_combine <- 
  bind_rows( first_dose = first_dose_obsity, second_dose = second_dose_obsity, .id = "cohort")  %>% 
  mutate( vaccine = case_when( is.na( dmd_code_1) ~ dmd_code_2,
                               TRUE ~ dmd_code_1)) %>% 
  mutate( cohort  = case_when( cohort == "first_dose" ~ "One-dose cohort",
                               TRUE ~ "Two-doses cohort"))


age_plot <- 
  ggplot( age_combine, aes( x = age_cat, y = perc, fill = vaccine)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_y_continuous( expand = c(0, 0), limits = c(0, 1)) +
  ggsci::scale_fill_npg( labels = c("ChAdOx1", "BNT162b2")) +
  labs( y = "Proportion", x = "Age", fill = "") +
  guides(fill = guide_legend(reverse = TRUE)) +
  lemon::facet_rep_grid( cols = vars(cohort), rows = vars( weight))+
  
  theme( 
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing.y = unit(1, "pt"),
    text = element_text(  size = 7),
    axis.text.x = element_text( size = 6, angle = 90, vjust = 0.5),
    axis.line.x = element_line( size = 0.25),
    axis.line.y = element_line( size = 0.25),
    axis.ticks.x = element_line( size = 0.25),
    axis.ticks.y = element_line( size = 0.25),
    strip.background = element_blank(),
    strip.text.x  =  element_text( face = "bold"),
    strip.text.y  =  element_text( face = "bold"),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-5,-5,-5,-5),
    legend.key.size = unit(5, 'pt'),
    legend.position = c(0.1,0.95))

pdf("D:/Document_sync/OneDrive - Nexus365/DPhil_training/Manuscripts/Vaccine_comparative_effectiveness/Nature_communication/Second_round_revision/Figures/age_plot.pdf",
    width = 2.28,
    height = 3,
    family = "Helvetica")

print(age_plot)

dev.off()



obsity_plot <- 
  ggplot( obsity_combine, aes( x = obsity, y = perc, fill = vaccine)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_y_continuous( expand = c(0, 0), limits = c(0, 1)) +
  ggsci::scale_fill_npg( labels = c("ChAdOx1", "BNT162b2")) +
  labs( y = "Proportion", x = "Obesity", fill = "") +
  
  guides(fill = guide_legend(reverse = TRUE)) +
  lemon::facet_rep_grid( cols = vars(cohort), rows = vars( weight))+
  
  theme( 
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.spacing.y = unit(1, "pt"),
    text = element_text(  size = 7),
    axis.text.x = element_text( size = 6),
    axis.line.x = element_line( size = 0.25),
    axis.line.y = element_line( size = 0.25),
    axis.ticks.x = element_line( size = 0.25),
    axis.ticks.y = element_line( size = 0.25),
    strip.background = element_blank(),
    strip.text.x  =  element_text( face = "bold"),
    strip.text.y  =  element_text( face = "bold"),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-5,-5,-5,-5),
    legend.key.size = unit(5, 'pt'),
    legend.position = c(0.4,0.95))


pdf("D:/Document_sync/OneDrive - Nexus365/DPhil_training/Manuscripts/Vaccine_comparative_effectiveness/Nature_communication/Second_round_revision/Figures/obsity_plot.pdf",
    width = 3.46,
    height = 3,
    family = "Helvetica")

print(obsity_plot)

dev.off() 

names( first_dose_cohort_weighted$PS_probability)

# KM Curve ----------------------------------------------------------------
library( survminer)

scales::show_col(ggsci::pal_nejm("default")(8))

first_dose_KM_func <- function( df, sub_df){
  
  sample_infection <<- 
    df[[sub_df]] %>% 
    select( eid, exposure, PS_weights) %>% 
    left_join( select( first_dose_link_outcome_cohort$infection_event, eid, result, follow_up_days), by = "eid") 
  
  sample_hospitalization <<- 
    df[[sub_df]] %>% 
    select( eid, exposure, PS_weights) %>% 
    left_join( select( first_dose_link_outcome_cohort$hospitalization_event, eid, result, follow_up_days), by = "eid") 
  
  fit_weighted_infection <- survfit( Surv( follow_up_days, result) ~ exposure, data = sample_infection, weights = PS_weights)
  fit_weighted_hospitalization <- survfit( Surv( follow_up_days, result) ~ exposure, data = sample_hospitalization, weights = PS_weights)
  
  KM_plot_infection <- ggsurvplot( fit_weighted_infection)$data.survplot
  KM_plot_hospitalization <- ggsurvplot( fit_weighted_hospitalization)$data.survplot
  
  output <- bind_rows( Infection = KM_plot_infection, hospitalisation = KM_plot_hospitalization, .id = "outcome") %>% 
    mutate( outcome = factor( outcome, 
                              levels = c( "Infection", "hospitalisation"),
                              labels = c( "Covid-19 infection", "Covid-19 hospitalisation")))
  
  return( output)
  
}
second_dose_KM_fun <- function( df, sub_df){
  
  sample_infection <<- 
    df[[sub_df]] %>% 
    select( eid, exposure, PS_weights) %>% 
    left_join( select( second_dose_link_outcome_cohort$infection_event, eid, result, follow_up_days), by = "eid") 
  
  sample_hospitalization <<- 
    df[[sub_df]] %>% 
    select( eid, exposure, PS_weights) %>% 
    left_join( select( second_dose_link_outcome_cohort$hospitalization_event, eid, result, follow_up_days), by = "eid") 
  
  fit_weighted_infection <- survfit( Surv( follow_up_days, result) ~ exposure, data = sample_infection, weights = PS_weights)
  fit_weighted_hospitalization <- survfit( Surv( follow_up_days, result) ~ exposure, data = sample_hospitalization, weights = PS_weights)
  
  KM_plot_infection <- ggsurvplot(fit_weighted_infection)$data.survplot
  KM_plot_hospitalization <- ggsurvplot(fit_weighted_hospitalization)$data.survplot
  
  df <- bind_rows( Infection = KM_plot_infection, hospitalisation = KM_plot_hospitalization, .id = "outcome") %>% 
    mutate( outcome = factor( outcome, 
                              levels = c( "Infection", "hospitalisation"),
                              labels = c( "Covid-19 infection", "Covid-19 hospitalisation")))
  
  return( df)
  
}

first_dose_KM <- first_dose_KM_func( df = first_dose_cohort_weighted, sub_df = "PS_probability_trim")
second_dose_KM <- second_dose_KM_fun( df = second_dose_cohort_weighted, sub_df = "PS_probability_trim")

df <- bind_rows( one_dose  = first_dose_KM, two_dose = second_dose_KM, .id = "Cohort") %>% 
  mutate( Cohort = factor( Cohort, labels = c("After the first dose", "After the second dose")))
  
KM_plot_func <- function(){
    
  KM_plot <- 
    ggplot( df, aes( x = time, y = (1 - surv), color = exposure)) +
      geom_step( size = 0.45) +
      scale_x_continuous( expand = c(0,0),
                          breaks = seq(0, 32)*7,
                          labels = paste( seq(0, 32))) +
      scale_y_continuous( #expand = c(0,0),
        # limits = c(0,0.008),
      breaks = scales::breaks_extended(7),
      labels = scales::label_percent( scale = 1000, suffix = "")) +
      labs( y = "Cumulative incidence per 1,000 persons", 
            x = "Follow up (weeks)", fill = "Dose", color = "") +
      ggsci::scale_color_npg( labels = c("ChAdOx1", "BNT162b2"))+
      guides(color = guide_legend(reverse = TRUE)) +
      lemon::facet_rep_grid( rows = vars( outcome),
                             cols = vars( Cohort), 
                             scales = "free", space = "free_x",
                             repeat.tick.labels = TRUE)+
    theme( 
      panel.background = element_blank(),
      panel.grid.major.y = element_line( colour = "grey", size = 0.1),
      text = element_text(  size = 7),
      axis.text.x = element_text( size = 6, vjust = 0.5),
      axis.line.x = element_line( size = 0.25),
      axis.line.y = element_line( size = 0.25),
      axis.ticks.x = element_line( size = 0.25),
      axis.ticks.y = element_line( size = 0.25),
      strip.background = element_blank(),
      strip.text.x  =  element_text( face = "bold"),
      strip.text.y  =  element_text( face = "bold"),
      legend.key = element_rect( fill = "transparent"),
      legend.background = element_rect( fill='transparent', color = 0), #transparent legend bg
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,-5,-5,-5),
      legend.position = "bottom") 
  
  
  pdf("D:/Document_sync/OneDrive - Nexus365/DPhil_training/Manuscripts/Vaccine_comparative_effectiveness/Nature_communication/Second_round_revision/Figures/KM_plot.pdf",
      width = 7.08,
      height = 4,
      family = "Helvetica")
  
  print(KM_plot)
  
  dev.off()
  
    
  }
  
  
# Cox Modelling ---------------------------------------------------------------
incidence_first_func <- function( df = first_dose_cohort_weighted, sub_df = "PS_probability_trim"){
  
  df_infection <- 
    select(  df[[sub_df]], eid, issue_date_1, dmd_code_1, PS_weights) %>% 
    left_join( select( first_dose_link_outcome_cohort$infection_event, eid, specdate, result), by = "eid") %>% 
    group_by( dmd_code_1) %>% 
    summarise( numb = n(),
               pys = sum( as.numeric(specdate -issue_date_1)/365),
               event = sum( result),
               incidence = event/pys *1000) 
  
  print(df_infection)
  
  df_hospitalization <- 
    select(  df[[sub_df]], eid, issue_date_1, dmd_code_1, PS_weights) %>% 
    left_join( select( first_dose_link_outcome_cohort$hospitalization_event, eid, specdate, result), by = "eid") %>% 
    group_by( dmd_code_1) %>% 
    summarise( numb = n(),
               pys = sum( as.numeric(specdate -issue_date_1)/365),
               event = sum( result),
               incidence = event/pys *1000) 
  
  print(df_hospitalization)
  
} 
incidence_second_func <- function( df = second_dose_cohort_weighted, sub_df = "PS_probability_trim"){
  
  df_infection <- 
    select( df[[sub_df]], eid, issue_date_2, dmd_code_2, PS_weights) %>% 
    left_join( select( second_dose_link_outcome_cohort$infection_event, eid, specdate, result), by = "eid") %>% 
    group_by( dmd_code_2) %>% 
    summarise( numb = n(),
               pys = sum( as.numeric(specdate -issue_date_2)/365),
               event = sum( result),
               incidence = event/pys *1000) 
  
  print(df_infection)
  
  df_hospitalization <- 
    select( df[[sub_df]], eid, issue_date_2, dmd_code_2, PS_weights) %>% 
    left_join( select( second_dose_link_outcome_cohort$hospitalization_event, eid, specdate, result), by = "eid") %>% 
    group_by( dmd_code_2) %>% 
    summarise( numb = n(),
               pys = sum( as.numeric(specdate -issue_date_2)/365),
               event = sum( result),
               incidence = event/pys *1000) 
  
  print(df_hospitalization)
  
} 

incidence_first_func()
incidence_second_func()

Cox_first_func <- function( df, sub_df){
  
  formu_string <- paste( "Surv(tstart, tstop, outcome_status == 1)", "~", "dmd_code_1", "+", paste( 1, collapse=" + "))
  formu_string_subgroup <- paste( "Surv(tstart, tstop, outcome_status == 1)", "~", "follow_cat_ref", "+", paste( 1, collapse=" + "))
  
  df_infection <- 
    select( df[[sub_df]], eid, dmd_code_1, PS_weights) %>% 
    left_join( select( first_dose_counting_infection_cohort, eid, outcome_status , tstart, tstop, follow_cat_ref), by = "eid")
  
  unadjust_fit_model_infection <- coxph( as.formula( formu_string), df_infection) # attention: no weight
  fit_model_infection <- coxph( as.formula( formu_string), df_infection, weights = PS_weights)
  fit_model_infection_subgroup <- coxph( as.formula( formu_string_subgroup), df_infection, weights = PS_weights)
  
  df_hospitalization <- 
    select( df$PS_probability, eid, dmd_code_1, PS_weights) %>% 
    left_join( select( first_dose_counting_hospitalization_cohort, eid, outcome_status , tstart, tstop, follow_cat_ref), by = "eid")
  
  unadjust_fit_model_hospitalization <- coxph( as.formula( formu_string), df_hospitalization)# attention: no weight
  fit_model_hospitalization <- coxph( as.formula( formu_string), df_hospitalization, weights = PS_weights)
  
  temp_func <- function( raw_model){
    
    Cox_results <- 
      summary( raw_model)$conf.int %>% as_tibble( rownames = "Vars") %>% janitor::clean_names() %>%   
      mutate( HR_CI = paste(format(round(exp_coef, 2), nsmall = 2), " (",
                            format(round(lower_95, 2), nsmall = 2), " - ",
                            format(round(upper_95, 2), nsmall = 2), ")",sep = "")) 
    return( Cox_results)
    
  }
  
  output <- map_df( list(
                    unadjust_fit_model_infection = unadjust_fit_model_infection,
                    unadjust_fit_model_hospitalization = unadjust_fit_model_hospitalization,
                    fit_model_infection = fit_model_infection,
                    fit_model_infection_subgroup = fit_model_infection_subgroup,
                    fit_model_hospitalization = fit_model_hospitalization),
                    temp_func, .id = "models")
  
  return( output)
  
}
Cox_second_func <- function( df, sub_df){
  
  formu_string <- paste( "Surv(tstart, tstop, outcome_status == 1)", "~", "dmd_code_2", "+", paste( 1, collapse=" + "))
  formu_string_subgroup <- paste( "Surv(tstart, tstop, outcome_status == 1)", "~", "follow_cat_ref", "+", paste( 1, collapse=" + "))
  
  df_infection <- 
    select( df[[sub_df]], eid, dmd_code_2, PS_weights) %>% 
    left_join( select( second_dose_counting_infection_cohort, eid, outcome_status , tstart, tstop, follow_up_days, follow_cat_ref), by = "eid")
  
  unadjust_fit_model_infection <- coxph( as.formula( formu_string), df_infection) # attention: no weight
  fit_model_infection <- coxph( as.formula( formu_string), df_infection, weights = PS_weights)
  fit_model_infection_subgroup <- coxph( as.formula( formu_string_subgroup), df_infection, weights = PS_weights)
  
  df_hospitalization <- 
    select( df[[sub_df]], eid, dmd_code_2, PS_weights) %>% 
    left_join( select( second_dose_counting_hospitalization_cohort, eid, outcome_status , tstart, tstop, follow_up_days, follow_cat_ref), by = "eid")
  
  unadjust_fit_model_hospitalization <- coxph( as.formula( formu_string), df_hospitalization)# attention: no weight
  fit_model_hospitalization <- coxph( as.formula( formu_string), df_hospitalization, weights = PS_weights)
  fit_model_hospitalization_subgroup <- coxph( as.formula( formu_string_subgroup), df_hospitalization, weights = PS_weights)
  
  temp_func <- function( raw_model){
    
    Cox_results <- 
      summary( raw_model)$conf.int %>% as_tibble( rownames = "Vars") %>% janitor::clean_names() %>%   
      mutate( HR_CI = paste(format(round(exp_coef, 2), nsmall = 2), " (",
                            format(round(lower_95, 2), nsmall = 2), " - ",
                            format(round(upper_95, 2), nsmall = 2), ")",sep = "")) 
    return( Cox_results)
    
  }
  
  output <- map_df( list( unadjust_fit_model_infection = unadjust_fit_model_infection,
                          unadjust_fit_model_hospitalization = unadjust_fit_model_hospitalization,
                          fit_model_infection = fit_model_infection,
                          fit_model_infection_subgroup = fit_model_infection_subgroup,
                          fit_model_hospitalization = fit_model_hospitalization,
                          fit_model_hospitalization_subgroup = fit_model_hospitalization_subgroup),
                    temp_func, .id = "models")
  
  return( output)
  
}

first_HR_results <- Cox_first_func( df = first_dose_cohort_weighted, sub_df = "PS_probability_trim")
second_HR_results <- Cox_second_func( df = second_dose_cohort_weighted, sub_df = "PS_probability_trim")

# Subgroup analyses  -------------------------------------------------------
subgroup_data_infection <- 
  select( second_dose_cohort_weighted$PS_probability_trim, eid, dmd_code_2, PS_weights, age, sex, ethnicity, bmi) %>% 
  left_join( select( second_dose_counting_infection_cohort, eid, outcome_status , tstart, tstop, follow_up_days, follow_cat_ref), by = "eid") %>% 
  mutate( age_index = case_when( age < 75 ~ "<75", age >= 75 ~ ">=75"),
          sex_index = case_when( sex == "Male" ~ "Male", sex == "Female" ~ "Female"),
          ethnicity_index = case_when( ethnicity == "White" ~ "White", ethnicity != "White" ~ "Others"),
          bmi_index = case_when( bmi < 25 ~ "Normal", bmi >= 25 ~ "Overweight"))


subgroup_data_hospitalization <- 
  select( second_dose_cohort_weighted$PS_probability_trim, eid, dmd_code_2, PS_weights, age, sex, ethnicity, bmi) %>% 
  left_join( select( second_dose_counting_hospitalization_cohort, eid, outcome_status , tstart, tstop, follow_up_days, follow_cat_ref), by = "eid") %>% 
  mutate( age_index = case_when( age < 75 ~ "<75", age >= 75 ~ ">=75"),
          sex_index = case_when( sex == "Male" ~ "Male", sex == "Female" ~ "Female"),
          ethnicity_index = case_when( ethnicity == "White" ~ "White", ethnicity != "White" ~ "Others"),
          bmi_index = case_when( bmi < 25 ~ "Normal", bmi >= 25 ~ "Overweight"))
  
#summary statistics and HR in each subgroups
temp_func <- function( raw_model){
    
    Cox_results <- 
      summary( raw_model)$conf.int %>% as_tibble( rownames = "Vars") %>% janitor::clean_names() %>%   
      mutate( HR_CI = paste(format(round(exp_coef, 2), nsmall = 2), " (",
                            format(round(lower_95, 2), nsmall = 2), " - ",
                            format(round(upper_95, 2), nsmall = 2), ")",sep = "")) 
    return( Cox_results)
    
  }
summary_func <- function( df, condition){
    
    subgroup_data_temp <- 
      df %>% 
      filter( !!rlang::parse_expr( condition))
    
    statis <- 
      subgroup_data_temp %>% 
      group_by( eid) %>% 
      arrange( desc(tstart)) %>% 
      filter( row_number() == 1) %>% 
      group_by( dmd_code_2) %>% 
      summarise( numb = sum(PS_weights),
                 event = sum( outcome_status * PS_weights),
                 pys = sum( as.numeric(follow_up_days) * PS_weights/365),
                 incidence = event/pys *1000,
                 epitools::pois.exact( event, pt = pys, conf.level = 0.95) *1000) 
    return( statis)
    
}
HR_func <- function( df, condition){
  
  subgroup_data_temp <- 
    df %>% 
    filter( !!rlang::parse_expr( condition))
  
  sub_cox_model <- coxph( Surv(tstart, tstop, outcome_status == 1) ~ dmd_code_2, data = subgroup_data_temp, weights = PS_weights)
  HRs <- temp_func( sub_cox_model)
  
  return( HRs)
  
}
  
condtion_list <- c( 'age_index == "<75"','age_index == ">=75"',
                    'sex_index == "Male"', 'sex == "Female"',
                    'ethnicity_index == "White"', 'ethnicity_index == "Others"', 
                    'bmi_index == "Normal"', 'bmi_index == "Overweight"') 
 
ynames <- c( ">= 50 and < 75 yrs",
             ">= 75 yrs",
             "Male",
             "Female",
             "White",
             "Other ethnic groups",
             "Normal weight",
             "Overweight or obesity")

names( condtion_list) <- ynames
  
summary_data_subgroup_infection <- 
  map_df( condtion_list, summary_func, df = subgroup_data_infection, .id = "source") 

summary_data_subgroup_hospitalization <- 
  map_df( condtion_list, summary_func, df = subgroup_data_hospitalization, .id = "source") 

HR_data_subgroup_infection <- 
  map_df( condtion_list, HR_func, df = subgroup_data_infection,.id = "source") 

HR_data_subgroup_hospitalization <- 
  map_df( condtion_list, HR_func, df = subgroup_data_hospitalization,.id = "source") 

#summary statistics and HR in each time windows
summary_data_time_window_infection <- 
    select( second_dose_cohort_weighted$PS_probability_trim, eid, PS_weights) %>% 
    left_join( second_dose_counting_infection_cohort, by = "eid") %>% 
    ungroup() %>% 
    mutate( gaps = tstop - tstart) %>% 
    group_by( dmd_code_2, follow_cat) %>% 
    summarise( numb = sum( PS_weights),
               event = sum( outcome_status * PS_weights),
               pys = sum( as.numeric( gaps) * PS_weights/365),
               incidence = event/pys *1000,
               epitools::pois.exact( event, pt = pys, conf.level = 0.95)*1000) %>% 
    ungroup() 


summary_data_time_window_hospitalization <- 
  select( second_dose_cohort_weighted$PS_probability_trim, eid, PS_weights) %>% 
  left_join( second_dose_counting_hospitalization_cohort, by = "eid") %>% 
  ungroup() %>% 
  mutate( gaps = tstop - tstart) %>% 
  group_by( dmd_code_2, follow_cat) %>% 
  summarise( numb = sum( PS_weights),
             event = sum( outcome_status * PS_weights),
             pys = sum( as.numeric( gaps) * PS_weights/365),
             incidence = event/pys *1000,
             epitools::pois.exact( event, pt = pys, conf.level = 0.95) *1000) %>% 
  ungroup() 

  
HR_data_time_window_infection <- 
  second_HR_results %>% 
  filter( str_detect(models,  "_infection_subgroup"))


HR_data_time_window_hospitalization <- 
  second_HR_results %>% 
  filter( str_detect(models,  "_hospitalization_subgroup"))

infection_func <- function(){
  
  # plot incidence section
  incidence_plot_df <- 
    bind_rows( summary_data_subgroup_infection = summary_data_subgroup_infection, 
               summary_data_time_window_infection = summary_data_time_window_infection, .id = "Section") %>% 
    mutate( New_label = case_when( follow_cat == "1" ~ "0-4 weeks",
                                   follow_cat == "2" ~ "4-8 weeks",
                                   follow_cat == "3" ~ "8-12 weeks",
                                   follow_cat == "4" ~ "12-16 weeks",
                                   follow_cat == "5" ~ "16-20 weeks",
                                   follow_cat == "6" ~ "20-24 weeks",
                                   follow_cat == "7" ~ ">24 weeks",
                                   TRUE ~ source),
            New_label = factor( New_label, c(ynames, "0-4 weeks", "4-8 weeks", "8-12 weeks", "12-16 weeks", "16-20 weeks", "20-24 weeks", ">24 weeks"))) %>% 
    mutate( Section = factor( Section, levels = c( "summary_data_subgroup_infection", "summary_data_time_window_infection"), labels = c( "Sub-population", "Sub-follow-up window")))
  
  
  incidence_plot <-   
    ggplot( incidence_plot_df, aes(x = incidence, y = fct_rev(New_label), color = dmd_code_2))+
    geom_point( shape = 16, size = 1) +
    geom_linerange( aes( xmin = lower , xmax = upper), size = 0.25) +
    scale_x_continuous( limits = c( 0, 120), breaks = seq(0, 120, 20)) +
    # scale_y_discrete( position = "right") +
    ggsci::scale_color_npg( labels = c("ChA", "BNT")) +
    guides( color = guide_legend( reverse = TRUE, nrow = 1)) +
    labs( x = "Incidence Rate: Per 1000 PYs", y = "", color = "")+
    facet_grid( rows = vars( Section), scales = "free", switch = "y")+
    theme(
      text = element_text(  size = 7),
      axis.line.x = element_line( size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line( size = 0.25),
      axis.ticks.y = element_line( size = 0.25),
      panel.border = element_blank(),
      panel.spacing.y = unit(2, "pt"),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey", linetype = "dotted", size = 0.25),
      strip.background = element_blank(),
      strip.text = element_text( hjust = 0.5, face = "bold"),
      strip.placement = "outside",
      legend.position = "none",
      legend.key = element_rect( fill = "transparent"),
      legend.background = element_rect( fill='transparent', color = 0), #transparent legend bg
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,-5,-5,-5))
  
  # plot HR section
  HR_plot_df <-
    bind_rows( HR_data_subgroup_infection = HR_data_subgroup_infection, 
               HR_data_time_window_infection = HR_data_time_window_infection, .id = "Section") %>% 
    mutate( New_label = case_when( vars == "follow_cat_ref1" ~ "0-4 weeks",
                                   vars == "follow_cat_ref2" ~ "4-8 weeks",
                                   vars == "follow_cat_ref3" ~ "8-12 weeks",
                                   vars == "follow_cat_ref4" ~ "12-16 weeks",
                                   vars == "follow_cat_ref5" ~ "16-20 weeks",
                                   vars == "follow_cat_ref6" ~ "20-24 weeks",
                                   vars == "follow_cat_ref7" ~ ">24 weeks",
                                   TRUE ~ source),
            New_label = factor( New_label, c(ynames, "0-4 weeks", "4-8 weeks", "8-12 weeks", "12-16 weeks", "16-20 weeks", "20-24 weeks", ">24 weeks")))
  
  
  HR_plot <- 
    ggplot( HR_plot_df, aes(x = exp_coef, y = fct_rev( New_label)))+
    geom_point( shape = 15, size = 0.8)+
    geom_errorbar( aes( xmin = lower_95, xmax = upper_95), width = 0.2, size = 0.25) +
    scale_x_continuous( trans = scales::log2_trans(), limits = c( 0.25, 2), breaks = c( 0.25 ,0.5,  1,  2)) +
    geom_vline( xintercept = 1, linetype="dashed", size = 0.25) +
    labs( x = "Hazard Ratio: BNT vs ChA", y = "")+
    facet_grid( rows = vars(Section), scales = "free")+
    
    theme(
      text = element_text(  size = 7),
      axis.line.x = element_line( size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line( size = 0.25),
      axis.ticks.y = element_line( size = 0.25),
      axis.text.y = element_blank(),
      panel.border = element_blank(),
      panel.spacing.y = unit(2, "pt"),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey", linetype = "dotted", size = 0.25),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "top",
      legend.background = element_blank(),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,-5,-5,-5))
  
  plot_infection <- cowplot::plot_grid(incidence_plot, HR_plot, rel_widths = c(2.5, 1))
  title <- cowplot::ggdraw() + cowplot::draw_label("Covid-19 infection", fontface='bold', size = 6)
  output_plot_infection <- cowplot::plot_grid( title, plot_infection, ncol=1, rel_heights=c(0.05, 1))
  
  pdf("D:/Document_sync/OneDrive - Nexus365/DPhil_training/Manuscripts/Vaccine_comparative_effectiveness/Nature_communication/Second_round_revision/Figures/output_plot_infection.pdf",
      width = 4.67,
      height = 3,
      family = "Helvetica")
  
  print(output_plot_infection)
  
  dev.off() 
  
  
}
hospitalization_func <- function(){
  
  # plot incidence section
  incidence_plot_df <- 
    bind_rows( summary_data_subgroup_hospitalization = summary_data_subgroup_hospitalization, 
               summary_data_time_window_hospitalization = summary_data_time_window_hospitalization, .id = "Section") %>% 
    mutate( New_label = case_when( follow_cat == "1" ~ "0-4 weeks",
                                   follow_cat == "2" ~ "4-8 weeks",
                                   follow_cat == "3" ~ "8-12 weeks",
                                   follow_cat == "4" ~ "12-16 weeks",
                                   follow_cat == "5" ~ "16-20 weeks",
                                   follow_cat == "6" ~ "20-24 weeks",
                                   follow_cat == "7" ~ ">24 weeks",
                                   TRUE ~ source),
            New_label = factor( New_label, c(ynames, "0-4 weeks", "4-8 weeks", "8-12 weeks", "12-16 weeks", "16-20 weeks", "20-24 weeks", ">24 weeks"))) %>% 
    mutate( Section = factor( Section, levels = c( "summary_data_subgroup_hospitalization", "summary_data_time_window_hospitalization"), labels = c( "Sub-population", "Sub-follow-up window")))
  
  
  incidence_plot <-   
    ggplot( incidence_plot_df, aes(x = incidence, y = fct_rev(New_label), color = dmd_code_2))+
    geom_point( shape = 16, size = 1) +
    geom_linerange( aes( xmin = lower , xmax = upper), size = 0.25) +
    scale_x_continuous( limits = c( 0, 12), breaks = seq(0, 12, 2)) +
    # scale_y_discrete( position = "right") +
    ggsci::scale_color_npg( labels = c("ChA", "BNT")) +
    guides( color = guide_legend( reverse = TRUE, nrow = 1)) +
    labs( x = "Incidence Rate: Per 1000 PYs", y = "", color = "")+
    facet_grid( rows = vars( Section), scales = "free", switch = "y")+
    theme(
      text = element_text(  size = 7),
      axis.line.x = element_line( size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line( size = 0.25),
      axis.ticks.y = element_line( size = 0.25),
      panel.border = element_blank(),
      panel.spacing.y = unit(2, "pt"),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey", linetype = "dotted", size = 0.25),
      strip.background = element_blank(),
      strip.text = element_text( hjust = 0.5, face = "bold"),
      strip.placement = "outside",
      legend.position = "none",
      legend.key = element_rect( fill = "transparent"),
      legend.background = element_rect( fill='transparent', color = 0), #transparent legend bg
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,-5,-5,-5))
  
  # plot HR section
  HR_plot_df <-
    bind_rows( HR_data_subgroup_hospitalization = HR_data_subgroup_hospitalization, 
               HR_data_time_window_hospitalization = HR_data_time_window_hospitalization, .id = "Section") %>% 
    mutate( New_label = case_when( vars == "follow_cat_ref1" ~ "0-4 weeks",
                                   vars == "follow_cat_ref2" ~ "4-8 weeks",
                                   vars == "follow_cat_ref3" ~ "8-12 weeks",
                                   vars == "follow_cat_ref4" ~ "12-16 weeks",
                                   vars == "follow_cat_ref5" ~ "16-20 weeks",
                                   vars == "follow_cat_ref6" ~ "20-24 weeks",
                                   vars == "follow_cat_ref7" ~ ">24 weeks",
                                   TRUE ~ source),
            New_label = factor( New_label, c(ynames, "0-4 weeks", "4-8 weeks", "8-12 weeks", "12-16 weeks", "16-20 weeks", "20-24 weeks", ">24 weeks"))) %>% 
    mutate( lower_95 = case_when( lower_95 < 0.25 ~ 0.25, TRUE ~ lower_95),
            upper_95 = case_when( upper_95 > 2 ~ 2, TRUE ~ upper_95),
            exp_coef = case_when( exp_coef < 0.25 ~ 0.25, 
                                  exp_coef > 2 ~ 2, 
                                  TRUE ~ exp_coef)) 
  
  HR_plot <- 
    ggplot( HR_plot_df, aes(x = exp_coef, y = fct_rev( New_label)))+
    geom_point( shape = 15, size = 0.8)+
    geom_errorbar( aes( xmin = lower_95, xmax = upper_95), width = 0.2, size = 0.25) +
    scale_x_continuous( trans = scales::log2_trans(), limits = c( 0.25, 2), breaks = c( 0.25 ,0.5,  1,  2)) +
    geom_vline( xintercept = 1, linetype="dashed", size = 0.25) +
    labs( x = "Hazard Ratio: BNT vs ChA", y = "")+
    facet_grid( rows = vars(Section), scales = "free")+
    
    theme(
      text = element_text(  size = 7),
      axis.line.x = element_line( size = 0.25),
      axis.line.y = element_blank(),
      axis.ticks.x = element_line( size = 0.25),
      axis.ticks.y = element_line( size = 0.25),
      axis.text.y = element_blank(),
      panel.border = element_blank(),
      panel.spacing.y = unit(2, "pt"),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey", linetype = "dotted", size = 0.25),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "top",
      legend.background = element_blank(),
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-5,-5,-5,-5))
  
  plot_hospitalisation <- cowplot::plot_grid(incidence_plot, HR_plot, rel_widths = c(2.5, 1))
  title <- cowplot::ggdraw() + cowplot::draw_label("Covid-19 hospitalisation", fontface='bold', size = 6)
  output_plot_hospitalisation <- cowplot::plot_grid( title, plot_hospitalisation, ncol=1, rel_heights=c(0.05, 1))
  
  pdf("D:/Document_sync/OneDrive - Nexus365/DPhil_training/Manuscripts/Vaccine_comparative_effectiveness/Nature_communication/Second_round_revision/Figures/output_plot_hospitalisation.pdf",
      width = 4.67,
      height = 3,
      family = "Helvetica")
  
  print(output_plot_hospitalisation)
  
  dev.off() 
  
  
}


# Sensitivity analyses ----------------------------------------------------

# Mactching data
matching_data_func <- function( data){
  
  set.seed(1)
  start.time <- Sys.time()
  listMatch <- Matching::Match(Tr       = second_dose_cohort_weighted$PS_probability$exposure,      # Need to be in 0,1
                               ## logit of PS,i.e., log(PS/(1-PS)) as matching scale
                               X        = log(second_dose_cohort_weighted$PS_probability$P_treatment/second_dose_cohort_weighted$PS_probability$P_control),
                               ## 1:1 matching
                               M        = 1,
                               ## caliper = 0.2 * SD(logit(PS))
                               caliper  = 0.2,
                               replace  = FALSE,
                               ties     = FALSE,
                               version  = "fast")
  print(Sys.time() - start.time)
  
  ## Extract matched data
  PS_imputed_macthed <- second_dose_cohort_weighted$PS_probability[unlist(listMatch[c("index.treated","index.control")]), ]
  second_PS_imputed_macthed <- second_dose_cohort_weighted$PS_probability[unlist(listMatch[c("index.treated","index.control")]), ]
  
  print(table( second_PS_imputed_macthed$dmd_code_2))
  
  return( PS_imputed_macthed)
  
}
first_linked_matching_cohort <- matching_data_func( data = first_linked_weighting_cohort)
second_linked_matching_cohort <- matching_data_func( data = second_linked_weighting_cohort)

#reugular cox regression
first_reugular_cox <- function(){
  
  library(survival)
  
  formu_string <- paste( "Surv(tstart, tstop, outcome_status == 1)", "~", "dmd_code_1", "+", paste( 1, collapse=" + "))
  
  df_infection <- 
    select( PS_imputed_macthed, eid, dmd_code_1) %>% 
    left_join( select( first_dose_counting_infection_cohort, eid, outcome_status , tstart, tstop, follow_cat_ref), by = "eid")
  
  df_hospitalization <- 
    select( PS_imputed_macthed, eid, dmd_code_1) %>% 
    left_join( select( first_dose_counting_hospitalization_cohort, eid, outcome_status , tstart, tstop, follow_cat_ref), by = "eid")
  
  fit_model_infection <- coxph( as.formula( formu_string), df_infection)
  fit_model_hospitalization <- coxph( as.formula( formu_string), df_hospitalization)
  
  temp_func( fit_model_hospitalization)
  
  df_infection <- 
    select(  PS_imputed_macthed, eid, issue_date_1, dmd_code_1) %>% 
    left_join( select( first_dose_link_outcome_cohort$infection_event, eid, specdate, result), by = "eid") %>% 
    group_by( dmd_code_1) %>% 
    summarise( numb = n(),
               pys = sum( as.numeric(specdate -issue_date_1)/365),
               event = sum( result),
               incidence = event/pys *1000) 
  
  df_hospitalization <- 
    select(  PS_imputed_macthed, eid, issue_date_1, dmd_code_1) %>% 
    left_join( select( first_dose_link_outcome_cohort$hospitalization_event, eid, specdate, result), by = "eid") %>% 
    group_by( dmd_code_1) %>% 
    summarise( numb = n(),
               pys = sum( as.numeric(specdate -issue_date_1)/365),
               event = sum( result),
               incidence = event/pys *1000)

  
}
second_reugular_cox <- function(){
  
  library(survival)
  
  formu_string <- paste( "Surv(tstart, tstop, outcome_status == 1)", "~", "dmd_code_2", "+", paste( 1, collapse=" + "))
  
  df_infection <- 
    select( second_PS_imputed_macthed, eid, dmd_code_2) %>% 
    left_join( select( second_dose_counting_infection_cohort, eid, outcome_status , tstart, tstop, follow_cat_ref), by = "eid")
  
  df_hospitalization <- 
    select( second_PS_imputed_macthed, eid, dmd_code_2) %>% 
    left_join( select( second_dose_counting_hospitalization_cohort, eid, outcome_status , tstart, tstop, follow_cat_ref), by = "eid")
  
  fit_model_infection <- coxph( as.formula( formu_string), df_infection)
  fit_model_hospitalization <- coxph( as.formula( formu_string), df_hospitalization)
  
  temp_func( fit_model_hospitalization)
  
  df_infection <- 
    select(  second_PS_imputed_macthed, eid, issue_date_2, dmd_code_2) %>% 
    left_join( select( second_dose_link_outcome_cohort$infection_event, eid, specdate, result), by = "eid") %>% 
    group_by( dmd_code_2) %>% 
    summarise( numb = n(),
               pys = sum( as.numeric(specdate -issue_date_2)/365),
               event = sum( result),
               incidence = event/pys *1000) 
  
  df_hospitalization <- 
    select(  second_PS_imputed_macthed, eid, issue_date_2, dmd_code_2) %>% 
    left_join( select( second_dose_link_outcome_cohort$hospitalization_event, eid, specdate, result), by = "eid") %>% 
    group_by( dmd_code_2) %>% 
    summarise( numb = n(),
               pys = sum( as.numeric(specdate -issue_date_2)/365),
               event = sum( result),
               incidence = event/pys *1000)
  
  
}

survival_func <- function( input_data){
  
  first_fit_weighted <- survfit( survival::Surv( follow_up_days_infection, infection_outcomes_after_dose_1) ~ first_vaccine, data = PS_imputed_macthed)
  
  sur_plot_temp <- 
    survminer::ggsurvplot(first_fit_weighted,
                          pval = FALSE, 
                          size = 0.8,
                          conf.int = TRUE,
                          ylim = c(0, 0.005),
                          xlim = c(0, 105),
                          break.time.by = 7,
                          
                          censor.shape = 124,
                          censor.size = 2,
                          risk.table = FALSE, # Add risk table
                          # risk.table.col = "strata", # Change risk table color by groups
                          # linetype = "strata", # Change line type by groups
                          # surv.median.line = "hv", # Specify median survival
                          fun = "event",
                          palette = c("#E7B800", "#2E9FDF"))
  
  
  # sur_plot <- 
  sur_plot_temp$plot + 
    scale_x_continuous( #expand = c(0.02,0.02),
      breaks = scales::breaks_width(7)) +
    scale_y_continuous( #expand = c(0,0),
      limits = c(0,0.005),
      labels = scales::label_percent( scale = 1000, suffix = "")) +
    # geom_label(  aes( x = 0, y = 0.004), label = forest_plot_text, inherit.aes = FALSE, family = "serif", hjust = 0) +
    labs( y = "Cumulative incidence", 
          x = "Follow-up (days)", fill = "Dose", color = "Vaccine") +
    guides( fill = FALSE) +
    ggsci::scale_color_lancet(name = "Vaccine", labels = c("AstraZeneca", "Pfizer"))+
    ggsci::scale_fill_lancet()+
    theme( panel.grid.minor = element_blank(),
           text = element_text( family = "serif"),
           axis.title.x = element_text( family = "serif"),
           axis.title.y = element_text( family = "serif"),
           legend.position = "bottom",
           legend.margin = margin(0,0,0,0, "cm"),
           legend.box.margin=margin(-10,0,0,0),
           plot.margin = margin(0.2, 0, 0, 0, "cm"))
  
  
  ggsave(filename = "D:/DPhil/UK_Biobank_opioid_application/UKB_based_projects/Sub_project/EMA_vaccine_safety/Figures/first_hospital_adj.png",
         plot = sur_plot,
         width = 10,
         height = 7,
         units = "cm",
         scale = 1.3)
  
}




#######################supplementary analysis######################
# Vaccination disctribution ----------------------------------------------------
plot_dose <- function(){
  
  vaccine_cohort_plot_only <- 
    vaccine_cohort_raw %>% 
    arrange( issue_date) %>% 
    group_by( eid) %>% 
    mutate( dose_seq = row_number()) %>% 
    ungroup() %>% 
    select( eid, dmd_code, issue_date, dose_seq) 
  
  dose_calendar_data <- 
    vaccine_cohort_plot_only %>% 
    group_by( issue_date, dose_seq) %>% 
    summarise( numb = n()) %>% 
    ungroup() %>% 
    filter( issue_date >= as.Date( "2020-12-01"), issue_date <= as.Date( "2021-12-01"))
  
  ggplot( data = dose_calendar_data, aes( x = issue_date, y = numb, fill = as.character(dose_seq)))+
    geom_col( position = position_stack( reverse = TRUE), width = 1)+
    scale_x_date( breaks = scales::breaks_width("1 month"),
                  limits = c(as.Date("2020/12/01"), NA),
                  labels = scales::date_format("%y/%m/%d"))+
    hrbrthemes::theme_ipsum() +
    labs( y = "Number", x = "Date", fill = "Dose") +
    theme( text = element_text( family = "serif"),
           axis.title.x = element_text( family = "serif"),
           axis.title.y = element_text( family = "serif"))
}

plot_week_type <- function(){
  
  vaccine_cohort_plot_only <- 
    vaccine_cohort_raw %>% 
    arrange( issue_date) %>% 
    group_by( eid) %>% 
    mutate( dose_seq = row_number()) %>% 
    ungroup() %>% 
    select( eid, dmd_code, issue_date, dose_seq) %>% 
    filter( issue_date >= as.Date( "2020-12-01"), issue_date <= as.Date( "2021-11-01")) 
  
  dose_dmd_data <- 
    vaccine_cohort_plot_only %>% 
    mutate( calendar_scale_week = as.factor(isoweek( issue_date)), # epi calendar week, starting from monday rather than sunday 
            calendar_scale_num = isoweek( issue_date)) %>% 
    group_by( calendar_scale_week, dose_seq, dmd_code) %>% 
    summarise( numb = n()) %>% 
    ungroup() %>% 
    mutate( dose_seq_label = factor( dose_seq, 
                                     levels = c( 1, 2, 3, 4),
                                     labels = c( "The first dose", "The second dose", "The third dose", "The fourth dose"))) %>% 
    filter( dmd_code %in% c("AstraZeneca", "Pfizer"), dose_seq %in% c(1,2)) %>% 
    mutate( dmd_code = fct_drop(dmd_code),
            dmd_code = factor( dmd_code, levels = c("AstraZeneca", "Pfizer"),labels = c("ChAdOx1", "BNT162b2" ))) %>% 
    mutate( calendar_scale_week = fct_shift( calendar_scale_week, -5))
  
  ann_background  <- 
    dose_dmd_data %>% 
    count( dose_seq_label) %>% 
    mutate( x_min = c(6.5, 16.5),
            x_max = c(13.5, 23.5),
            y_min = c(0, 0),
            y_max= c(40000, 40000))

  scales::show_col(ggsci::pal_nejm("default")(8))
  
  vaccination_distribution <- 
    ggplot( data = dose_dmd_data, aes( x = calendar_scale_week, y = numb, fill = dmd_code))+
    geom_col( position = position_stack( reverse = FALSE), width = 0.8, color = "black", size = 0.4 ) +
    scale_y_continuous( expand = c(0, 0),
                        limits = c(0, 40000))+
    # scale_x_date( breaks = bimonthly,
    #               limits = c(as.Date("2020/12/01"), NA),
    #               labels = scales::date_format("%d/%m/%y"))+
    geom_rect( data = ann_background, aes( xmin = x_min, xmax=x_max, ymin=y_min, ymax=y_max), alpha=0.5, fill="grey" ,inherit.aes = FALSE) +
    guides(fill = guide_legend(reverse = TRUE))+
    ggsci::scale_fill_npg() +
    # hrbrthemes::theme_ipsum() +
    lemon::facet_rep_grid( rows = vars(dose_seq_label), repeat.tick.labels = TRUE) +
    # geom_vline( data = filter(dose_dmd_data, dose_seq_label == "First dose"), aes(xintercept = as.Date("2021/01/15"))) +
    # geom_vline( data = filter(dose_dmd_data, dose_seq_label == "First dose"), aes(xintercept = as.Date("2021/03/15"))) +
    # geom_vline( data = filter(dose_dmd_data, dose_seq_label == "Second dose"), aes(xintercept = as.Date("2021/03/15"))) +
    # geom_vline( data = filter(dose_dmd_data, dose_seq_label == "Second dose"), aes(xintercept = as.Date("2021/05/15"))) +
    
    labs( y = "Number of vaccinations", x = "\nCalendar week", fill = "") +
    theme( 
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      text = element_text(  size = 7),
      axis.text.x = element_text( size = 6, vjust = 0.5),
      axis.line.x = element_line( size = 0.25),
      axis.line.y = element_line( size = 0.25),
      axis.ticks.x = element_line( size = 0.25),
      axis.ticks.y = element_line( size = 0.25),
      strip.background = element_blank(),
      legend.position = c(0.8,0.8),
    ) 
  
  
  pdf("D:/Document_sync/OneDrive - Nexus365/DPhil_training/Manuscripts/Vaccine_comparative_effectiveness/Nature_communication/Second_round_revision/Figures/vaccination_distribution.pdf",
      width = 7.08,
      height = 4,
      family = "Helvetica")
  
  print(vaccination_distribution)
  
  dev.off()
  
}



two_dose_gap_data <- 
  vaccine_cohort %>% 
  filter( dose_gap > 0, dose_gap < 100) %>% 
  filter( dmd_code %in% c("AstraZeneca", "Pfizer")) %>% 
  mutate( dose_gap_cat = cut_interval(as.numeric(dose_gap), length  = 7, right = FALSE)) %>% 
  mutate( dose_gap_cat_combined = fct_collapse(dose_gap_cat,
                                               "<8" = c("[0,7)", "[7,14)", "[14,21)", "[21,28)", "[28,35)", "[35,42)", "[42,49)", "[49,56)"),
                                               "8-9" = c("[56,63)"),
                                               "9-10" = c("[63,70)"),
                                               "10-11" = c("[70,77)"),
                                               "11-12" = c("[77,84)"),
                                               other_level = ">=12")) %>% 
  mutate( dose_gap_cat_combined_broad = fct_collapse(dose_gap_cat,
                                                     "<8" = c("[0,7)", "[7,14)", "[14,21)", "[21,28)", "[28,35)", "[35,42)", "[42,49)", "[49,56)"),
                                                     "8-10" = c("[56,63)", "[63,70)", "[70,77)", "[77,84)"),
                                                     "10-12" = c("[70,77)", "[77,84)"),
                                                     other_level = ">=12")) 



ggplot( two_dose_gap_data, aes(x = dose_gap))+
  geom_histogram(binwidth = 0.5)+
  scale_x_continuous( breaks = scales::breaks_width(7)
  )+
  hrbrthemes::theme_ipsum() +
  labs( y = "Number", x = "Gaps between two sequential doses") +
  theme( text = element_text( family = "serif"))


plot_temp <- function(){
  
  label_week <- c( "<1 week", paste( seq(1, length(levels( two_dose_gap_data$dose_gap_cat))-1), "week"))
  
  
  Basic_stats <- 
    two_dose_gap_data %>% 
    group_by( dmd_code, dose_gap_cat) %>% 
    summarise( numb = n()) %>% 
    ungroup()
  
  
  ggplot( data = Basic_stats, aes( x = dose_gap_cat, y = numb)) +
    geom_col( width = 0.7) +
    scale_x_discrete(labels = label_week)+
    lemon::facet_rep_grid( rows = vars(dmd_code), repeat.tick.labels = TRUE) +
    hrbrthemes::theme_ipsum() +
    labs( y = "Number", x = "Gaps between two sequential doses") +
    theme( text = element_text( family = "serif"),
           axis.title.x = element_text( family = "serif"),
           axis.title.y = element_text( family = "serif"),
           strip.text =  element_text( family = "serif"),
           axis.text.x = element_text( angle = 45, vjust = 0.5, size = 8))
  
  
}
plot_temp()



