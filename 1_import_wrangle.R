# packages ----------------------------------------------------------------

library(tidyverse)
library(readr)

# data import and clean ---------------------------------------------------

source("locs.R") # filenames and locations not shared

d <- read_csv(dataset) # bulk of PQIP data csv file
vars_fct <- read_csv(vars_fct) # list of categorical variable names

d_clean <- d %>% 
  mutate(across(.cols = all_of(vars_fct$var), ~as.factor(.)),
         S04WhatWasTheRecoveryCare = fct_relevel(S04WhatWasTheRecoveryCare,
                                                 c("NON","MIL","MOD","SEV","UOT","USD")),
         S16PainAtSiteOfSurgery = fct_relevel(S16PainAtSiteOfSurgery,
                                              c("No","Mld","Mod","Sev")),
         S16HowSatisfiedWithPainTherapyAfterSurgery = fct_relevel(S16HowSatisfiedWithPainTherapyAfterSurgery,
                                                                  c("VD","D","S","VS")),
         S01AgeYears = S01AgeYears + (S01AgeMonths/12),
         # assume that planned thoracoscopic NA is equivalent to 2)
         S02PlannedProcedureSurgeryModeThoracoscopic = if_else(is.na(S02PlannedProcedureSurgeryModeThoracoscopic), 2,
                                                               S02PlannedProcedureSurgeryModeThoracoscopic)) %>% 
  # patient has diabetes values 1 and 2 are wrong %>% 
  mutate(S02PatientHasDiabetes = as.character(S02PatientHasDiabetes)) %>% 
  filter(S02PatientHasDiabetes != "1" & S02PatientHasDiabetes != "2") %>% 
  mutate(S02PatientHasDiabetes = as.factor(S02PatientHasDiabetes)) %>% 
  # cancer variable is a recurrent problem later, so fix now
  mutate(S02CancerDiagnosis = case_when(S02CancerDiagnosis == "Y" ~ "Y",
                                        S02CancerDiagnosis == "N" ~ "N",
                                        is.na(S02CancerDiagnosis) ~ "N")) %>% 
  mutate(S02CancerDiagnosis = as.factor(S02CancerDiagnosis))

# make new variables

d_clean <- d_clean %>% 
  mutate(new_grading = as.factor(str_replace_all(S02PlannedProcedureOperation, "[:digit:]|[:punct:]", "")),
         new_grading = fct_relevel(new_grading, c("Maj","Xma","Com")),
         new_mode_open = case_when(S02PlannedProcedureSurgeryModeOpen == 1 & 
                                     (S03ActualProcedureSurgeryMode == "Opn" |
                                        is.na(S03ActualProcedureSurgeryMode)) ~ 1,
                                   S02PlannedProcedureSurgeryModeOpen == 1 &
                                     (S03ActualProcedureSurgeryMode != "Opn" &
                                     !is.na(S03ActualProcedureSurgeryMode)) ~ 0,
                                   S02PlannedProcedureSurgeryModeOpen == 2 &
                                     S03ActualProcedureSurgeryMode == "Opn" ~ 1,
                                   S02PlannedProcedureSurgeryModeOpen == 2 &
                                     (S03ActualProcedureSurgeryMode != "Opn" |
                                        is.na(S03ActualProcedureSurgeryMode)) ~ 0),
         new_mode_lap = case_when(S02PlannedProcedureSurgeryModeLaparoscopic == 1 & 
                                     (S03ActualProcedureSurgeryMode == "Lap" |
                                        is.na(S03ActualProcedureSurgeryMode)) ~ 1,
                                  S02PlannedProcedureSurgeryModeLaparoscopic == 1 &
                                     (S03ActualProcedureSurgeryMode != "Lap" &
                                        !is.na(S03ActualProcedureSurgeryMode)) ~ 0,
                                  S02PlannedProcedureSurgeryModeLaparoscopic == 2 &
                                     S03ActualProcedureSurgeryMode == "Lap" ~ 1,
                                  S02PlannedProcedureSurgeryModeLaparoscopic == 2 &
                                    (S03ActualProcedureSurgeryMode != "Lap" |
                                       is.na(S03ActualProcedureSurgeryMode)) ~ 0),
         new_mode_rob = case_when(S02PlannedProcedureSurgeryModeRobotic == 1 & 
                                    (S03ActualProcedureSurgeryMode == "Rob" |
                                       is.na(S03ActualProcedureSurgeryMode)) ~ 1,
                                  S02PlannedProcedureSurgeryModeRobotic == 1 &
                                    (S03ActualProcedureSurgeryMode != "Rob" &
                                       !is.na(S03ActualProcedureSurgeryMode)) ~ 0,
                                  S02PlannedProcedureSurgeryModeRobotic == 2 &
                                    S03ActualProcedureSurgeryMode == "Rob" ~ 1,
                                  S02PlannedProcedureSurgeryModeRobotic == 2 &
                                    (S03ActualProcedureSurgeryMode != "Rob" |
                                       is.na(S03ActualProcedureSurgeryMode)) ~ 0),
         new_mode_tho = case_when(S02PlannedProcedureSurgeryModeThoracoscopic == 1 & 
                                    (S03ActualProcedureSurgeryMode == "Tho" |
                                       is.na(S03ActualProcedureSurgeryMode)) ~ 1,
                                  S02PlannedProcedureSurgeryModeThoracoscopic == 1 &
                                    (S03ActualProcedureSurgeryMode != "Tho" &
                                       !is.na(S03ActualProcedureSurgeryMode)) ~ 0,
                                  S02PlannedProcedureSurgeryModeThoracoscopic == 2 &
                                    S03ActualProcedureSurgeryMode == "Tho" ~ 1,
                                  (S02PlannedProcedureSurgeryModeThoracoscopic == 2 |
                                    is.na(S02PlannedProcedureSurgeryModeThoracoscopic)) &
                                    (S03ActualProcedureSurgeryMode != "Tho" |
                                       is.na(S03ActualProcedureSurgeryMode)) ~ 0),
         new_rec_sev = case_when(S04WhatWasTheRecoveryCare == "SEV" ~ "SEV",
                                 S04WhatWasTheRecoveryCare == "NON" |
                                   S04WhatWasTheRecoveryCare == "MIL" |
                                   S04WhatWasTheRecoveryCare == "MOD" ~ "NMM",
                                 S04WhatWasTheRecoveryCare == "UOT" |
                                   S04WhatWasTheRecoveryCare == "USD" |
                                   is.na(S04WhatWasTheRecoveryCare) ~ "MIS"),
         new_d1_sev = case_when(S16PainAtSiteOfSurgery == "Sev" ~ "SEV",
                                S16PainAtSiteOfSurgery == "No" |
                                  S16PainAtSiteOfSurgery == "Mld" |
                                  S16PainAtSiteOfSurgery == "Mod" ~ "NMM",
                                is.na(S16PainAtSiteOfSurgery) ~ "MIS"),
         new_d3_sev = case_when(S17SeverePain <= 3 ~ "SEV",
                                S17SeverePain > 3 ~ "NMM",
                                is.na(S17SeverePain) ~ "MIS")) %>% 
  dplyr::select(-c(S02PlannedProcedureSurgeryModeOpen,
                   S02PlannedProcedureSurgeryModeLaparoscopic,
                   S02PlannedProcedureSurgeryModeRobotic,
                   S02PlannedProcedureSurgeryModeThoracoscopic,
                   S03ActualProcedureSurgeryMode))

# add baseline opioid use fields

addin <- read_csv(add) # csv containing discharge fields
addin <- addin %>% 
  dplyr::select(CaseId,
         S07DischargedOnOpioids) %>% 
  mutate(new_preopioids = if_else(
           S07DischargedOnOpioids == "OPOP" |
           S07DischargedOnOpioids == "OPWOP", 1, 0)
         ) %>% 
  dplyr::select(-S07DischargedOnOpioids)

# from April 2019 - 14636 NA; approx 10000 present
# New = new prescription, previously naive
# NOP = no opioid prescription, previously naive
# OPOP = opioids preoperatively, opioid prescrtion
# OPWOP = opioids preoperatively withtout opioid prescription

d_clean_bound <- d_clean %>% 
  left_join(addin, by = "CaseId")

# remove unnecessary fields

d_cut <- d_clean_bound %>% 
  dplyr::select(-c(
    #CaseId,
            S01AgeMonths,
            S02PlannedProcedureOperation,
            contains("_NK"),
            S04WhatWasTheRecoveryCare,
            S16PainAtSiteOfSurgery,
            S17SeverePain
            ))

# variables and datasets -------------------------------------------------------------

vars_all <- colnames(d_cut)

# baseline pain variables
vars_blpain <- read_csv(bl_pain)

# PREOP dataset = set1

vars_preop <- d_cut %>% 
  dplyr::select(starts_with(c("S01","S02", "new_mode", "S03HowMany")) |
                  new_grading |
                  new_preopioids |
                  starts_with(vars_blpain$var)) %>% 
  colnames

set1 <- d_cut %>% select(c(all_of(vars_preop),new_d1_sev)) %>% 
  mutate(across(starts_with("new_"), ~as.factor(.)),
         across(starts_with("EQ5D"), ~ as.ordered(.)),
         S02RespiratoryHistoryFindings = fct_collapse(S02RespiratoryHistoryFindings, DLE = c("DLE","DAR")),
         S02PatientHasHistoryOfCerebrovascularDisease = fct_collapse(S02PatientHasHistoryOfCerebrovascularDisease, Y = c("YNH","YWH")),
         S02PatientHasDiabetes = fct_collapse(S02PatientHasDiabetes, IDDM = c("T1","T2I"),
                                              NIDDM = c("T2D","T2N")),
         S02PatientsASAGrade = as.ordered(fct_collapse(S02PatientsASAGrade, "4" = c("4","5"))),
         S02PatientsSmokingHistory = fct_collapse(S02PatientsSmokingHistory, NS = c("NS","NK"),
                                                  EX = c("XG6", "XL6")),
         S02PatientsCurrentAlcoholConsumption = fct_collapse(S02PatientsCurrentAlcoholConsumption, G2 = c("34","G5"),
                                                             NoA = c("NoA", "NK")),
         S02PlannedPostoperativeDestination = fct_collapse(S02PlannedPostoperativeDestination, L2C = c("EC","L2C")),
         S03HowManyOperationsInPast30Days = fct_collapse(S03HowManyOperationsInPast30Days, "2" = c("2","G2")),
         S13WhatIsYourCurrentOccupation = fct_collapse(S13WhatIsYourCurrentOccupation, L1 = c("L1","L2")),
         across(starts_with("EQ5D"), ~as.ordered(fct_collapse(., "4" = c("4","5"))))) %>% 
  mutate(S01Gender = fct_relevel(S01Gender, "M"),
         S02SurgicalSpecialty = fct_relevel(S02SurgicalSpecialty, "Abdominal - Lower gastrointestinal"),
         S02RespiratoryHistoryFindings = fct_relevel(S02RespiratoryHistoryFindings, "N"),
         S02PatientHasHistoryOfCerebrovascularDisease = fct_relevel(S02PatientHasHistoryOfCerebrovascularDisease, "N"),
         S02PatientHasDiabetes = fct_relevel(S02PatientHasDiabetes, "N"),
         S02PatientsSmokingHistory = fct_relevel(S02PatientsSmokingHistory, "NS"),
         S02PatientsCurrentAlcoholConsumption = fct_relevel(S02PatientsCurrentAlcoholConsumption, "NoA"),
         S02PlannedPostoperativeDestination = fct_relevel(S02PlannedPostoperativeDestination, "WC"),
         S03HowManyOperationsInPast30Days = fct_relevel(S03HowManyOperationsInPast30Days, "1"),
         S13WhatIsYourCurrentOccupation = fct_relevel(S13WhatIsYourCurrentOccupation, "L4"))

# compare those with/without the primary outcome

missing_outcome <- set1 %>% mutate(missing_outcome = case_when(
  new_d1_sev == "MIS" ~ 1,
  new_d1_sev == "NMM" | new_d1_sev == "SEV" ~ 0))

d_cut %>% filter(new_d1_sev == "MIS") %>% select(S16NotAbleToCompleteBauerSatisfactionScore) %>% table(., useNA = "always")

missing_table <- CreateTableOne(data = missing_outcome, strata = "missing_outcome", test = FALSE, smd = TRUE,
                                argsNonNormal = c("S01AgeYears",
                                                  "S14FeelingOfGeneralWellBeing",
                                                  "S14ModeratePain",
                                                  "S14SeverePain",
                                                  "S14FeelingWorriedOrAnxious",
                                                  "S14FeelingSadOrDepressed",
                                                  "S15Past30DaysHowManyDaysTotallyUnableToCarryOutUsualActivitiesBecauseOfHealth"))


# intra op variables
vars_intraop <- d_cut %>% 
  dplyr::select(starts_with(c("S03"))) %>%
  select(!starts_with("S03HowMany")) %>% 
  colnames

# immediate postop variables
vars_imm <- d_cut %>% 
  dplyr::select(c(starts_with("S04"), new_rec_sev)) %>% 
  colnames

# 24hrs post op
vars_postop <- d_cut %>% 
  dplyr::select(starts_with(c("S05", "new_d1_sev"))) %>% 
  colnames

# dataset up to 24hours = set2
set2 <- d_cut %>% select(all_of(c(vars_intraop, vars_imm, vars_postop)))

# explore -----------------------------------------------------------------

library(DescTools)

d_cut %>% 
  select(S01AgeYears,
         S02SerumCreatinine,
         S02SerumSodium,
         S02SerumPotassium,
         S02SerumUrea,
         S02WhiteCellCount,
         S02Haemoglobin) %>% 
  summary()

# outliers in Hb both, creatinine both, potassium sodium both, urea high, wcc both

# winsorize

transformed <- d_cut %>% 
  mutate(across(all_of(c("S02SerumCreatinine",
                         "S02SerumSodium",
                         "S02SerumPotassium",
                         "S02WhiteCellCount",
                         "S02Haemoglobin")),
                ~Winsorize(., probs = c(0.01, 0.99), na.rm = TRUE)),
         S02SerumUrea = Winsorize(S02SerumUrea, probs = c(0, 0.99), na.rm = TRUE))

# after Winsor

transformed %>% 
  select(S01AgeYears,
         S02SerumCreatinine,
         S02SerumSodium,
         S02SerumPotassium,
         S02SerumUrea,
         S02WhiteCellCount,
         S02Haemoglobin) %>% 
  summary()

# assess relationship with outcome

# physiology/laboratory 
transformed %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome),
         outcome.num = as.numeric(outcome) - 1) %>% 
  dplyr::select(outcome.num,
                S01AgeYears,
                S02SerumCreatinine,
                S02SerumSodium,
                S02SerumPotassium,
                S02SerumUrea,
                S02WhiteCellCount,
                S02Haemoglobin) %>% 
  pivot_longer(all_of(c("S01AgeYears",
                        "S02SerumCreatinine",
                        "S02SerumSodium",
                        "S02SerumPotassium",
                        "S02SerumUrea",
                        "S02WhiteCellCount",
                        "S02Haemoglobin")), names_to = "predictors") %>% 
  #filter(predictors == "S02WhiteCellCount") %>% 
  ggplot(aes(x = value, y = outcome.num)) + 
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~predictors, scales = "free_x")

# try RCS

var_rcs <- c("S01AgeYears",
             "S02SerumCreatinine",
             "S02SerumSodium",
             "S02SerumPotassium",
             "S02SerumUrea",
             "S02WhiteCellCount",
             "S02Haemoglobin")

rcs.data <- transformed %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome),
         outcome.num = as.numeric(outcome) - 1) %>% 
  dplyr::select(outcome.num,
                all_of(var_rcs))

# use loop to try different transforms
for(i in rcs.data[,c(2,3,6)]){
  mRCS <- ols(outcome.num ~ pol(i,3), data = rcs.data)
  print(rcs.data %>% 
          mutate(x.rcs = fitted(mRCS)) %>% 
          ggplot() +
          geom_point(aes(x = i, y = outcome.num)) +
          geom_smooth(aes(i, outcome.num), method = "loess") +
          geom_point(aes(x = i, y = x.rcs), color = "red", size = 0.5))
}

# followed by PROMs

var_proms <- c("S14FeelingOfGeneralWellBeing",
               "S14ModeratePain",
               "S14SeverePain",
               "S14FeelingWorriedOrAnxious",
               "S14FeelingSadOrDepressed",
               "compo",
               "S15Past30DaysHowManyDaysTotallyUnableToCarryOutUsualActivitiesBecauseOfHealth")

rcs.data.proms <- transformed %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome),
         outcome.num = as.numeric(outcome) - 1,
         compo = S14FeelingOfGeneralWellBeing +                                              
           S14ModeratePain +                                                
           S14SeverePain +
           S14FeelingWorriedOrAnxious +
           S14FeelingSadOrDepressed) %>% 
  dplyr::select(outcome.num,
                all_of(var_proms))

for(i in rcs.data.proms[,c(8)]){
  mRCS <- ols(outcome.num ~ pol(i,4), data = rcs.data.proms)
  print(rcs.data %>% 
          mutate(x.rcs = fitted(mRCS)) %>% 
          ggplot() +
          geom_point(aes(x = i, y = outcome.num)) +
          geom_smooth(aes(i, outcome.num), method = "loess") +
          geom_point(aes(x = i, y = x.rcs), color = "red", size = 0.5))
}

problem <- rcs.data[,c(1,6)]
problem <- problem %>% mutate(x = log(S02SerumUrea))
mRCS <- ols(outcome.num ~ rcs(x), data = problem)
problem %>% 
  mutate(x.rcs = fitted(mRCS)) %>% 
  ggplot() +
  geom_point(aes(x = x, y = outcome.num)) +
  geom_smooth(aes(x, outcome.num), method = "loess") +
  geom_point(aes(x = x, y = x.rcs), color = "red", size = 0.5)

# plotting rcs ------------------------------------------------------

rcs.data <- transformed %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome),
         outcome.num = as.numeric(outcome) - 1) %>% 
  dplyr::select(outcome.num,
                x = log_creat) %>% 
  na.omit

# log transform for days unable
rcs.data <- rcs.data %>% 
  mutate(x = log(S15Past30DaysHowManyDaysTotallyUnableToCarryOutUsualActivitiesBecauseOfHealth+1))



# resulting non-linear ----------------------------------------------------

nonlinear.rcs5 <- c("S01AgeYears",
                    "S02SerumSodium",
                    "S02SerumUrea",
                    "S02WhiteCellCount",
                    "S14FeelingOfGeneralWellBeing",
                    "S14FeelingSadOrDepressed")

nonlinear.pol3 <- c("S02SerumCreatinine",
                    "compo")

nonlinear.other <- c("S15Past30DaysHowManyDaysTotallyUnableToCarryOutUsualActivitiesBecauseOfHealth")

# transformed dataset
nonlinear.data <- transformed %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome),
         outcome.num = as.numeric(outcome) - 1,
         compo = S14FeelingOfGeneralWellBeing +                                              
           S14ModeratePain +                                                
           S14SeverePain +
           S14FeelingWorriedOrAnxious +
           S14FeelingSadOrDepressed) %>% 
  dplyr::select(outcome.num,
                all_of(c(nonlinear.rcs5,
                         nonlinear.pol3,
                         nonlinear.other)))

for(i in nonlinear.data[,c(2:7)]){
  mRCS <- ols(outcome.num ~ rcs(i), data = nonlinear.data)
  print(nonlinear.data %>% 
          mutate(x.rcs = fitted(mRCS)) %>% 
          ggplot() +
          geom_point(aes(x = i, y = outcome.num)) +
          geom_smooth(aes(i, outcome.num), method = "loess") +
          geom_point(aes(x = i, y = x.rcs), color = "red", size = 0.5))
}

for(i in nonlinear.data[,c(8:9)]){
  mRCS <- ols(outcome.num ~ pol(i,3), data = nonlinear.data)
  print(nonlinear.data %>% 
          mutate(x.rcs = fitted(mRCS)) %>% 
          ggplot() +
          geom_point(aes(x = i, y = outcome.num)) +
          geom_smooth(aes(i, outcome.num), method = "loess") +
          geom_point(aes(x = i, y = x.rcs), color = "red", size = 0.5))
}

for(i in nonlinear.data[,c(10)]){
  mRCS <- ols(outcome.num ~ pol(log(i+1),3), data = nonlinear.data)
  print(nonlinear.data %>% 
          mutate(x.rcs = fitted(mRCS)) %>% 
          ggplot() +
          geom_point(aes(x = log(i+1), y = outcome.num)) +
          geom_smooth(aes(log(i+1), outcome.num), method = "loess") +
          geom_point(aes(x = log(i+1), y = x.rcs), color = "red", size = 0.5))
}

# now to sort categories --------------------------------------------------

cat.data <- transformed %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome),
         across(starts_with("EQ5D"), ~as.factor(.)),
         S03HowManyOperationsInPast30Days = as.factor(S03HowManyOperationsInPast30Days)) %>%
  select(where(is.factor) & !(starts_with(c("m6","y1"))))

cat.data <- transformed %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome),
         outcome.num = as.numeric(outcome) - 1) %>%
  #  mutate(x.cmo = fct_collapse(x, "3"  = c("3","4")),
  #         x.3l = fct_collapse(x, "3" = c("2","3","4"))) %>%
  dplyr::select(outcome.num,
                x = S02PatientsASAGrade) %>% 
  na.omit

cat.data %>% 
  ggplot() +
  geom_smooth(aes(as.numeric(x), outcome.num), method = "loess") +
  # geom_smooth(aes(as.numeric(as.character(x.cmo)), outcome.num), method = "lm", color = "red") +
  # geom_smooth(aes(as.numeric(as.character(x.3l)), outcome.num), method = "lm", color = "green") +
  ylim(c(0,1))
