
# packages ----------------------------------------------------------------

library(caret)
library(MASS)
library(pROC)
library(tidyverse)


# list to keep IDs --------------------------------------------------------

cases <- list()

# 1 predict recovery from preop ---------------------------------------------

# select only those with outcome

set1 <- d_cut %>% 
  filter(new_rec_sev != "MIS") %>% 
  mutate(new_rec_sev = as.factor(new_rec_sev)) %>% 
  dplyr::select(c(CaseId, all_of(vars_preop), new_rec_sev))

# patients now = 

nrow(set1)

# check missingness of preop variables

colSums(is.na(set1))

# hundreds to thousands for bloods;  15000 for opioids
# repeating with baseline variables - around 2000 missing

# separate into opioids and the rest
# use those who have full set of the rest

set1_full_o <- na.omit(set1)
set1_full_n <- set1 %>% dplyr::select(-new_preopioids) %>% na.omit

nrow(set1_full_o)
nrow(set1_full_n)

# table of outcomes
table(set1_full_o$new_rec_sev)
table(set1_full_n$new_rec_sev)

# model data

cases$set1_full_o <- set1_full_o$CaseId
cases$set1_full_n <- set1_full_n$CaseId

model1n_data <- set1_full_n %>% 
  mutate(outcome = if_else(new_rec_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_rec_sev)
model1o_data <- set1_full_o %>% 
  mutate(outcome = if_else(new_rec_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_rec_sev)


# 2 predict recovery from pre and intraop ---------------------------------------------

# select only those with outcome

set2 <- d_cut %>% 
  filter(new_rec_sev != "MIS") %>% 
  mutate(new_rec_sev = as.factor(new_rec_sev)) %>% 
  dplyr::select(c(CaseId, all_of(vars_preop),all_of(vars_intraop), new_rec_sev))

# patients now = 25938

nrow(set2)

# check missingness of pre and intraop variables

colSums(is.na(set2))

# hundreds to thousands for bloods; 14000 for cancer
# 2000 for wound cath, gapapentin etc.
# 13000 for GA types
# 12000 for incision, 10000 soiling
# 2000 for baseline variables

# cancer na is cancer == N
# drop variables with 22000 missing

miss_intra <- as.data.frame(colSums(is.na(set2)))
miss20k_intra <- miss_intra %>% filter(`colSums(is.na(set2))` > 20000) %>% rownames()

set2_full <- set2 %>% 
  dplyr::select(-all_of(miss20k_intra))

# explored incision data. only 1 patient has a difference between 1/2s and the factor
# has UA = 2, Other = 1, factor = UA; mode reported as laparoscopic, so keep as Other
# therefore drop the factor

set2_full <- set2_full %>% dplyr::select(-S03WhatSurgicalIncisionDidThisPatientHave)

colSums(is.na(set2_full))

# 13000 have missing info on GA details - and only 2 for e.g. GA.

miss13k_intra <- miss_intra %>% filter(`colSums(is.na(set2))` == 13722) %>% rownames()

set2_full <- set2_full %>% dplyr::select(-all_of(miss13k_intra))

colSums(is.na(set2_full))

# now peritoneal soiling 9202
# need to turn NA into an actual value, and remove MD

set2_full <- set2_full %>% 
  mutate(S03DegreeOfPeritonealSoiling = if_else(is.na(S03DegreeOfPeritonealSoiling),
                                                "NOTA",
                                                as.character(S03DegreeOfPeritonealSoiling))) %>% 
  filter(S03DegreeOfPeritonealSoiling != "MD") %>% 
  mutate(S03DegreeOfPeritonealSoiling = as.factor(S03DegreeOfPeritonealSoiling))

# use those who have full set of the rest
# factoring in opioids again

set2_full_o <- na.omit(set2_full)
set2_full_n <- set2_full %>% dplyr::select(-new_preopioids) %>% na.omit

nrow(set2_full_o) # 6867
nrow(set2_full_n) # 18259

# table of outcomes
table(set2_full_o$new_rec_sev)
table(set2_full_n$new_rec_sev)

# model data

cases$set2_full_o <- set2_full_o$CaseId
cases$set2_full_n <- set2_full_n$CaseId

model2o_data <- set2_full_o %>% 
  mutate(outcome = if_else(new_rec_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_rec_sev)
model2n_data <- set2_full_n %>% 
  mutate(outcome = if_else(new_rec_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_rec_sev)

# 3p predict d1 from preop ---------------------------------------------

# select only those with outcome

set3 <- d_cut %>% 
  filter(new_d1_sev != "MIS") %>% 
  mutate(new_d1_sev = as.factor(new_d1_sev))

set3_pre <- set3 %>% 
  dplyr::select(c(CaseId, all_of(vars_preop), new_d1_sev)) 

set3_imm <- set3 %>% 
  dplyr::select(c(CaseId, all_of(vars_preop), all_of(vars_intraop), all_of(vars_imm), new_d1_sev))

# repeat cleaning steps on preop variables

set3_pre_full_o <- na.omit(set3_pre)
set3_pre_full_n <- set3_pre %>% dplyr::select(-new_preopioids) %>% na.omit

nrow(set3_pre_full_o) #6862 -> 6415
nrow(set3_pre_full_n) #18227 -> 17136

cases$set3_pre_full_o <- set3_pre_full_o$CaseId
cases$set3_pre_full_n <- set3_pre_full_n$CaseId

model3pn_data <- set3_pre_full_n %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d1_sev)
model3po_data <- set3_pre_full_o %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d1_sev)

# 3pi predict d1 from preop and intraop ---------------------------------------------

set3_intra <- set3 %>% 
  dplyr::select(c(CaseId, all_of(vars_preop), all_of(vars_intraop), new_d1_sev)) 

# repeat cleaning steps

set3i_full <- set3_intra %>% dplyr::select(-S03WhatSurgicalIncisionDidThisPatientHave)

colSums(is.na(set3i_full))

miss_3i <- as.data.frame(colSums(is.na(set3i_full)))
miss17k_3i <- miss_3i %>% filter(`colSums(is.na(set3i_full))` > 17000) %>% rownames()

miss10k_3i <- miss_3i %>% filter(`colSums(is.na(set3i_full))` == 10838) %>% rownames()

set3i_full <- set3i_full %>% dplyr::select(-all_of(miss10k_3i), -all_of(miss17k_3i))

colSums(is.na(set3i_full))

set3i_full <- set3i_full %>% 
  mutate(S03DegreeOfPeritonealSoiling = if_else(is.na(S03DegreeOfPeritonealSoiling),
                                                "NOTA",
                                                as.character(S03DegreeOfPeritonealSoiling))) %>% 
  filter(S03DegreeOfPeritonealSoiling != "MD") %>% 
  mutate(S03DegreeOfPeritonealSoiling = as.factor(S03DegreeOfPeritonealSoiling))

# use those who have full set of the rest

set3pi_full_o <- na.omit(set3i_full)
set3pi_full_n <- set3i_full %>% dplyr::select(-new_preopioids) %>% na.omit

nrow(set3pi_full_o) # 5919 -> 5542
nrow(set3pi_full_n) # 15800 -> 14880

cases$set3pi_full_o <- set3pi_full_o$CaseId
cases$set3pi_full_n <- set3pi_full_n$CaseId

model3pio_data <- set3pi_full_o %>% 
  na.omit %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d1_sev)

model3pin_data <- set3pi_full_n %>% 
  na.omit %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d1_sev)


# 3imm predict d1 from pre, intra, immediate ------------------------------------

# repeat cleaning steps

set3imm_full <- set3_imm %>% dplyr::select(-S03WhatSurgicalIncisionDidThisPatientHave)

colSums(is.na(set3imm_full))

miss_3imm <- as.data.frame(colSums(is.na(set3imm_full)))
miss17k_3imm <- miss_3imm %>% filter(`colSums(is.na(set3imm_full))` > 17000) %>% rownames()

miss10k_3imm <- miss_3imm %>% filter(`colSums(is.na(set3imm_full))` == 10838) %>% rownames()

set3imm_full <- set3imm_full %>% dplyr::select(-all_of(miss10k_3imm), -all_of(miss17k_3imm))

colSums(is.na(set3imm_full))

set3imm_full <- set3imm_full %>% 
  mutate(S03DegreeOfPeritonealSoiling = if_else(is.na(S03DegreeOfPeritonealSoiling),
                                                "NOTA",
                                                as.character(S03DegreeOfPeritonealSoiling))) %>% 
  filter(S03DegreeOfPeritonealSoiling != "MD") %>% 
  mutate(S03DegreeOfPeritonealSoiling = as.factor(S03DegreeOfPeritonealSoiling))

# use those who have full set of the rest

set3imm_full_o <- na.omit(set3imm_full)
set3imm_full_n <- set3imm_full %>% dplyr::select(-new_preopioids) %>% na.omit

nrow(set3imm_full_o) #5919 -> 5542
nrow(set3imm_full_n) #15800 -> 14880

cases$set3imm_full_o <- set3imm_full_o$CaseId
cases$set3imm_full_n <- set3imm_full_n$CaseId

model3immo_data <- set3imm_full_o %>% 
  na.omit %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d1_sev)
model3immn_data <- set3imm_full_n %>% 
  na.omit %>% 
  mutate(outcome = if_else(new_d1_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d1_sev)


# 4p predict d3 from preop ---------------------------------------------------

# select only those with outcome

set4p <- d_cut %>% 
  filter(new_d3_sev != "MIS") %>% 
  mutate(new_d3_sev = as.factor(new_d3_sev)) %>% 
  dplyr::select(c(CaseId, all_of(vars_preop), new_d3_sev))

# patients now = 

nrow(set4p) # 16233

# check missingness of preop variables

colSums(is.na(set4p))

# hundreds to thousands for bloods;  15000 for opioids

# separate into opioids and the rest
# use those who have full set of the rest

set4p_full_o <- na.omit(set4p)
set4p_full_n <- set4p %>% dplyr::select(-new_preopioids) %>% na.omit

nrow(set4p_full_o) # 4940
nrow(set4p_full_n) # 14752

cases$set4p_full_o <- set4p_full_o$CaseId
cases$set4p_full_n <- set4p_full_n$CaseId

# table of outcomes
table(set4p_full_o$new_d3_sev)
table(set4p_full_n$new_d3_sev)

# model data

model4po_data <- set4p_full_o %>% 
  mutate(outcome = if_else(new_d3_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d3_sev)
model4pn_data <- set4p_full_n %>% 
  mutate(outcome = if_else(new_d3_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d3_sev)

# 4pi predict d3 from preop and intraop ---------------------------------------------

set4_intra <- d_cut %>% 
  filter(new_d3_sev != "MIS") %>% 
  mutate(new_d3_sev = as.factor(new_d3_sev)) %>% 
  dplyr::select(c(CaseId, all_of(vars_preop), all_of(vars_intraop), new_d3_sev)) 

# repeat cleaning steps

set4i_full <- set4_intra %>% dplyr::select(-S03WhatSurgicalIncisionDidThisPatientHave)

colSums(is.na(set4i_full))

miss_4i <- as.data.frame(colSums(is.na(set4i_full)))
miss14k_4i <- miss_4i %>% filter(`colSums(is.na(set4i_full))` > 14000) %>% rownames()

miss9k_4i <- miss_4i %>% filter(`colSums(is.na(set4i_full))` == 9129) %>% rownames()

set4i_full <- set4i_full %>% dplyr::select(-all_of(miss9k_4i), -all_of(miss14k_4i))

colSums(is.na(set4i_full))

set4i_full <- set4i_full %>% 
  mutate(S03DegreeOfPeritonealSoiling = if_else(is.na(S03DegreeOfPeritonealSoiling),
                                                "NOTA",
                                                as.character(S03DegreeOfPeritonealSoiling))) %>% 
  filter(S03DegreeOfPeritonealSoiling != "MD") %>% 
  mutate(S03DegreeOfPeritonealSoiling = as.factor(S03DegreeOfPeritonealSoiling))

# use those who have full set of the rest

set4pi_full_o <- na.omit(set4i_full)
set4pi_full_n <- set4i_full %>% dplyr::select(-new_preopioids) %>% na.omit

nrow(set4pi_full_o) # 4580 -> 4290
nrow(set4pi_full_n) # 12812 -> 12048

cases$set4pi_full_o <- set4pi_full_o$CaseId
cases$set4pi_full_n <- set4pi_full_n$CaseId

model4pio_data <- set4pi_full_o %>% 
  mutate(outcome = if_else(new_d3_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d3_sev)
model4pin_data <- set4pi_full_n %>% 
  mutate(outcome = if_else(new_d3_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d3_sev)

# 4imm predict d3 from pre, intra, immediate ------------------------------------


set4_imm <- d_cut %>% 
  filter(new_d3_sev != "MIS") %>% 
  mutate(new_d3_sev = as.factor(new_d3_sev)) %>% 
  dplyr::select(c(CaseId, all_of(vars_preop), all_of(vars_intraop), all_of(vars_imm), new_d3_sev)) 

# repeat cleaning steps

set4imm_full <- set4_imm %>% dplyr::select(-S03WhatSurgicalIncisionDidThisPatientHave)

colSums(is.na(set4imm_full))

miss_4imm <- as.data.frame(colSums(is.na(set4imm_full)))
miss14k_4imm <- miss_4imm %>% filter(`colSums(is.na(set4imm_full))` > 14000) %>% rownames()

miss9k_4imm <- miss_4imm %>% filter(`colSums(is.na(set4imm_full))` == 9129) %>% rownames()

set4imm_full <- set4imm_full %>% dplyr::select(-all_of(miss9k_4imm), -all_of(miss14k_4imm))

colSums(is.na(set4imm_full))

set4imm_full <- set4imm_full %>% 
  mutate(S03DegreeOfPeritonealSoiling = if_else(is.na(S03DegreeOfPeritonealSoiling),
                                                "NOTA",
                                                as.character(S03DegreeOfPeritonealSoiling))) %>% 
  filter(S03DegreeOfPeritonealSoiling != "MD") %>% 
  mutate(S03DegreeOfPeritonealSoiling = as.factor(S03DegreeOfPeritonealSoiling))

# use those who have full set of the rest

set4imm_full_o <- na.omit(set4imm_full)
set4imm_full_n <- set4imm_full %>% dplyr::select(-new_preopioids) %>% na.omit

nrow(set4imm_full_o) # 4580 -> 4290
nrow(set4imm_full_n) # 12812 -> 12048

cases$set4imm_full_o <- set4imm_full_o$CaseId
cases$set4imm_full_n <- set4imm_full_n$CaseId

model4immo_data <- set4imm_full_o %>% 
  na.omit %>% 
  mutate(outcome = if_else(new_d3_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d3_sev)
model4immn_data <- set4imm_full_n %>% 
  na.omit %>% 
  mutate(outcome = if_else(new_d3_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d3_sev)
# 4pp predict d3 from pre, intra, immediate and 24hrs ------------------------------------


set4_pp <- d_cut %>% 
  filter(new_d3_sev != "MIS") %>% 
  mutate(new_d3_sev = as.factor(new_d3_sev)) %>% 
  dplyr::select(c(CaseId, all_of(vars_preop), all_of(vars_intraop), all_of(vars_imm), all_of(vars_postop), new_d1_sev, new_d3_sev)) 

# repeat cleaning steps

set4pp_full <- set4_pp %>% dplyr::select(-S03WhatSurgicalIncisionDidThisPatientHave)

colSums(is.na(set4pp_full))

miss_4pp <- as.data.frame(colSums(is.na(set4pp_full)))
miss14k_4pp <- miss_4pp %>% filter(`colSums(is.na(set4pp_full))` > 14000) %>% rownames()

miss9k_4pp <- miss_4pp %>% filter(`colSums(is.na(set4pp_full))` == 9129) %>% rownames()

set4pp_full <- set4pp_full %>% dplyr::select(-all_of(miss9k_4pp), -all_of(miss14k_4pp))

colSums(is.na(set4pp_full))

set4pp_full <- set4pp_full %>% 
  mutate(S03DegreeOfPeritonealSoiling = if_else(is.na(S03DegreeOfPeritonealSoiling),
                                                "NOTA",
                                                as.character(S03DegreeOfPeritonealSoiling))) %>% 
  filter(S03DegreeOfPeritonealSoiling != "MD") %>% 
  mutate(S03DegreeOfPeritonealSoiling = as.factor(S03DegreeOfPeritonealSoiling))

# use those who have full set of the rest

set4pp_full_o <- na.omit(set4pp_full)
set4pp_full_n <- set4pp_full %>% dplyr::select(-new_preopioids) %>% na.omit

nrow(set4pp_full_o) # 4580 -> 4290
nrow(set4pp_full_n) # 12812 -> 12047

cases$set4pp_full_o <- set4pp_full_o$CaseId
cases$set4pp_full_n <- set4pp_full_n$CaseId

model4ppo_data <- set4pp_full_o %>% 
  na.omit %>% 
  mutate(outcome = if_else(new_d3_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d3_sev)
model4ppn_data <- set4pp_full_n %>% 
  na.omit %>% 
  mutate(outcome = if_else(new_d3_sev == "SEV", 1, 0)) %>% 
  mutate(outcome = as.factor(outcome)) %>% 
  dplyr::select(-new_d3_sev)
# everything from here can be looped --------------------------------------

source('runmodel.R')


# all truth ---------------------------------------------------------------

y_true <- d_cut %>% 
  dplyr::select(CaseId, new_rec_sev, new_d1_sev, new_d3_sev) %>% 
  mutate(across(starts_with("new"), ~if_else(. == "SEV", 1, 0))) %>% 
  mutate(across(starts_with("new"), ~as.factor(.)))

# save models -------------------------------------------------------------

# models_1n is those with no opioid
# now doing models_1o

# models_1n/o is predict recovery from preop
# models_2n/o is predict recovery from preop and intraop
# models_3p   is predict day 1    from preop
# models_3pi  is predict day 1    from preop and intraop
# models_3pim is predict day 1    from preop and intraop and immediate postop
# models_4p   is predict day 3    from preop
# models_4pi  is predict day 3    from preop and intraop
# models_4imm is predict day 3    from preop and intraop and immediate postop
# models_4pp  is predict day 3    from preop and intraop and immediate postop and d1


models_4ppn <- testmodels

save(models_4ppn, file = "models_4ppn.RData")

cms_4ppn <- cms_test
rocs_4ppn <- rocs_test

save(cms_4ppn, rocs_4ppn, file = "perf_4ppn.RData")

rm(models_test)
rm(testmodels)
rm(cms_test)
rm(rocs_test)
file.remove("models_test.RData")
file.remove("performance_test.RData")



# new 1-12-21 -------------------------------------------------------------

# what about using same, reduced dataset to model the preoperative only

newdata <- model3immo_data %>% 
  filter(CaseId %in% model3po_data$CaseId) %>% 
  select(c(CaseId, all_of(vars_preop), outcome))

# what about formal comparison of AUROC


load("~/Library/Mobile Documents/com~apple~CloudDocs/pqip-pain/3immo_performance.RData")

roc.3immo <- cms_test[2][[1]][[9]]

load("~/Library/Mobile Documents/com~apple~CloudDocs/pqip-pain/3pin_performance.RData")

roc.3pin <- cms_test[1][[1]][[9]]

load("~/Library/Mobile Documents/com~apple~CloudDocs/pqip-pain/3pn_performance.RData")

roc.3p <- cms_test[4][[1]][[9]]

ci.auc(roc.3immo)
ci.auc(roc.3pin)
ci.auc(roc.3p)

roc.test(roc.3immo,roc.3pin)
roc.test(roc.3immo,roc.3p)

# for primary outcome, compare best pre intra post to best pre, etc

# so the one on pre and intra is not actually worse. the following is the model:

load("~/Library/Mobile Documents/com~apple~CloudDocs/pqip-pain/3pin_models.RData")
load("~/Library/Mobile Documents/com~apple~CloudDocs/pqip-pain/3immo_models.RData")

# pre op
load("~/Library/Mobile Documents/com~apple~CloudDocs/pqip-pain/3pn_models.RData")

###
# for imm it's 2, for pi it's 1
y_pred <- as.data.frame(pred_test[2][[1]]) %>% 
  mutate(pred2 = if_else(pred_test[2][[1]] >=0.5,
                                            1, 0)) %>% mutate(pred2 = as.factor(pred2))
cm <- confusionMatrix(y_pred$pred2,y_true,positive = "1")
cm

# for pre-op it's number 4

y_pred <- as.data.frame(pred_test[4][[1]]) %>% 
  mutate(pred2 = if_else(pred_test[4][[1]] >=0.5,
                         1, 0)) %>% mutate(pred2 = as.factor(pred2))
cm <- confusionMatrix(y_pred$pred2,y_true,positive = "1")
cm


# hosmer ------------------------------------------------------------------

library(ResourceSelection)

hoslem.test(as.numeric(y_pred$pred2)-1,as.numeric(y_true)-1, g = 10)
