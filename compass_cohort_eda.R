##################################################################
##               Understanding the COMPASS Cohort               ##
##################################################################

# now to understand the COMPASS cohort and compare with table 1 from the original
# paper.


# libraries:

library(pacman)
p_load("tidyverse","Hmisc","rms","survival","haven","readxl","naniar","lubridate",
       "arsenal", "haven")



df = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_total.csv')

# df_1 = df[!is.na(df$age), ]


# write_csv(df_1,
#          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_total.csv')


# now to only limit to COMPASS eligible patients.

dfc = df %>% filter(compass_e == 1)

# prepare a simple table 1. for the data.

vars = c( "age", "sex","bmi","csmok", "diabetes", "priormi", 
          "pvd", "egfr")

factorvars = c( "sex","csmok", "diabetes", "priormi", 
"pvd")



t = tableone::CreateTableOne(vars = vars, factorVars = factorvars,
                             data = dfc)
 
# now looking at this simple table, we need to work on getting some variables together here.
# then we can get the table like the original paper.


dfc$tobacco_use = with(dfc, ifelse(csmok == 0,  0 , 1))

dfc %>% count(tobacco_use)

# heart failure 



hf = c("428.9","I50.9")

dfc$hf = with(dfc, ifelse((icd901 %in% hf | icd902 %in% hf | icd903 %in% hf | 
          icd904 %in% hf | icd905 %in% hf |
        icd906 %in% hf | icd907 %in% hf | icd908 %in% hf | icd909 %in% hf | icd910 %in% hf)
                          , 1, 0)) 


dfc %>% count(hf)

dfc$diabetes_c = with(dfc, ifelse(diabetes %in% c(1,2), 1, 0))

dfc %>% count(diabetes_c)

# now to group CKD according to the efgr.

dfc$ckd = with(dfc, ifelse(egfr > 60, 0, 
                    ifelse(egfr < 60 & egfr > 30, 1, 2)))

dfc %>% count(ckd)

describe(dfc$tcol)


dfc$chol_mmol = dfc$tcol*0.0555

summary(dfc$chol_mmol)

# prior_mi

dfc$prior_mi = with(dfc, ifelse(priormi == 0, 0 ,1))

dfc %>% count(prior_mi)


# now to create the table like t1 in original paper.


vars = c( "age", "sex","bmi","csmok", 
          "pvd", "egfr", "ckd", "chol_mmol","diabetes_c","hf","tobacco_use",
          "prior_mi")

factorvars = c( "sex","csmok", 
                "pvd","ckd", "diabetes_c","hf","tobacco_use",
                "prior_mi")



t = tableone::CreateTableOne(vars = vars, factorVars = factorvars,
                             data = dfc)

# table without medications

# Overall      
# n                     15586        
# age (mean (SD))       67.07 (7.12) 
# sex = 1 (%)             181 ( 1.2) 
# bmi (mean (SD))       30.03 (5.37) 
# csmok (%)                          
# 0                   2870 (18.4) 
# 1                   4064 (26.1) 
# 2                    743 ( 4.8) 
# 3                   7909 (50.7) 
# pvd = 1 (%)            3825 (24.6) 
# egfr (mean (SD))      74.18 (20.40)
# ckd (%)                            
# 0                  11680 (74.9) 
# 1                   3660 (23.5) 
# 2                    246 ( 1.6) 
# chol_mmol (mean (SD))  9.03 (2.44) 
# diabetes_c = 1 (%)     7894 (50.6) 
# hf = 1 (%)             7311 (46.9) 
# tobacco_use = 1 (%)   12716 (81.6) 
# prior_mi = 1 (%)       6355 (40.8) 



# additional part of this table is to look at medications that patients
# were on prior to CABG.
# medications included in the table are: 
# 1. ACE/ARB 2. CCB 3. Diuretics 4. BB 5. statins/lipid_lowering agents

# now to get the medications information.

p = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\my_outpat_compass.sas7bdat")

glimpse(p)

# need to get crosswalk to join surgery date for 
# patients into the main dataset.

# also have got the localdrugsid labels here.

d = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\allmeds_localdrugsid.sas7bdat")

# now get the croswalk.

c = read_sas("P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_crosswalk.sas7bdat")

dfc2 = dfc %>% dplyr::select(scrssn, surgdate)

names(c) = tolower(names(c))

dfc3 = left_join(dfc2, c, by = "scrssn")

glimpse(dfc3)

p$mine = with(p, ifelse(PatientSID %in% dfc3$patientsid, 1, 0))

p %>% count(mine)

p2 = p %>% filter(mine == 1)

names(p2) = tolower(names(p2))

p3 = left_join(p2, dfc3, by = "patientsid")

p3$filldatetime = as_date(p3$filldatetime)

p3$surgdate = as_date(p3$surgdate)

p3$days = (p3$surgdate %--% p3$filldatetime)/ddays(1)

summary(p3$days)

p4 = p3[!is.na(p3$days), ]

p5 = p4 %>% filter(days < 0 & days > -180)

glimpse(p5)

glimpse(d)

names(d) = tolower(names(d))

p6 = left_join(d, p5, by = "localdrugsid")

glimpse(p6)


# now using the drug class, we can split this into many tables, 1 for each class

bb = p6 %>% filter(drugclass == "CV100")

lipid = p6 %>% filter(drugclass == "CV350")

ccb = p6 %>% filter(drugclass == "CV200")

ace_arb = p6 %>% filter(drugclass %in% c("CV800", "C805"))

bb$bb = 1
lipid$lipid = 1
ccb$ccb = 1
ace_arb$ace_arb = 1

bb2 = bb[!duplicated(bb$scrssn), ]

lipid2 = lipid[!duplicated(lipid$scrssn), ]

ccb2 = ccb[!duplicated(ccb$scrssn), ]

ace_arb2 = ace_arb[!duplicated(ace_arb$scrssn), ]

# now to flag the patients in the main dataset dfc

dfc$lipid = with(dfc, ifelse(scrssn %in% lipid2$scrssn, 1, 0))

dfc$bb = with(dfc, ifelse(scrssn %in% bb2$scrssn, 1, 0))

dfc$ccb = with(dfc, ifelse(scrssn %in% ccb2$scrssn, 1, 0))

dfc$ace_arb = with(dfc, ifelse(scrssn %in% ace_arb2$scrssn, 1, 0))

# almost final table here:

dfc$cvd = factor(dfc$cvd)

vars = c( "age", "sex","bmi","csmok", "cvd","curdiur",
          "pvd", "egfr", "ckd", "chol_mmol","diabetes_c","hf","tobacco_use",
          "prior_mi", "lipid","bb","ace_arb","ccb")

factorvars = c( "sex","csmok", "curdiur",
                "pvd","ckd", "diabetes_c","hf","tobacco_use",
                "prior_mi", "ace_arb", "bb","ccb","lipid")



t = tableone::CreateTableOne(vars = vars, factorVars = factorvars,
                             data = dfc)


# final table with medications prior to surgery.

# Overall      
# n                     15586        
# age (mean (SD))       67.07 (7.12) 
# sex = 1 (%)             181 ( 1.2) 
# bmi (mean (SD))       30.03 (5.37) 
# csmok (%)                          
# 0                   2870 (18.4) 
# 1                   4064 (26.1) 
# 2                    743 ( 4.8) 
# 3                   7909 (50.7) 
# pvd = 1 (%)            3825 (24.6) 
# egfr (mean (SD))      74.18 (20.40)
# ckd (%)                            
# 0                  11680 (74.9) 
# 1                   3660 (23.5) 
# 2                    246 ( 1.6) 
# chol_mmol (mean (SD))  9.03 (2.44) 
# diabetes_c = 1 (%)     7894 (50.6) 
# hf = 1 (%)             7311 (46.9) 
# tobacco_use = 1 (%)   12716 (81.6) 
# prior_mi = 1 (%)       6355 (40.8) 
# lipid = 1 (%)         13080 (83.9) 
# bb = 1 (%)            11140 (71.5) 
# ace_arb = 1 (%)        7445 (47.8) 
# ccb = 1 (%)            4998 (32.1) 

# save this table so that we can then do comparison using 
# independent samples t-test and chi2 test.

# the end-points that we need to see are ---
# mortality, stroke, PCI/CABG, acute limb ischemia, emergency vascular surgery,
# VTE, cancer, cardiac arrest on admission/out of hospital.

# save this dataset of compass with preoperative medications.

write_csv(dfc,
          'P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds.csv')


# see the prevalence of COMPASS eligible patients per year of study.
# also see the prevalence of comorbidities in young and old COMPASS patients.

# now to look at COMPASS e patients per year.
# use the main df after COMPASS exclusion.

dim(df)

glimpse(df)


df$surg_year = year(df$surgdate)

summary(df$surgdate)


prop = df %>% group_by(surg_year) %>% summarise(prop = mean(compass_e))

prop


# > prop
# # A tibble: 9 x 2
# surg_year  prop
# <dbl> <dbl>
#   1      2010 0.707
# 2      2011 0.712
# 3      2012 0.741
# 4      2013 0.768
# 5      2014 0.791
# 6      2015 0.811
# 7      2016 0.837
# 8      2017 0.855
# 9      2018 0.841


glimpse(prop)


p = lm(prop ~ surg_year, data = prop)

summary(p)

# averaged 2 % increase per year.

# Call:
#   lm(formula = prop ~ surg_year, data = prop)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.024119 -0.003436  0.002892  0.006674  0.012654 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -39.460291   3.264304  -12.09 6.05e-06 ***
#   surg_year     0.019983   0.001621   12.33 5.30e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01255 on 7 degrees of freedom
# Multiple R-squared:  0.956,	Adjusted R-squared:  0.9497 
# F-statistic:   152 on 1 and 7 DF,  p-value: 5.302e-06


count = df %>% group_by(surg_year) %>%
  summarise(total = sum(compass_e))

count

table = table(df$surg_year, df$compass_e)

str(table)

table = table %>% tbl_df()

dfc$young = with(dfc, ifelse(age < 65, 1, 0))

dfc %>% count(young)

# among the patients that were young and COMPASS evaluable, what is the
# prevalence of COMPASS eligiblity over time.

df_young = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_young.csv')

glimpse(df_young)

table(df_young$compass_e)

df_young$surg_year = year(df_young$surgdate)

prop_young = df_young %>% group_by(surg_year) %>%
  summarise(prop = mean(compass_e))

prop_young # among young the prevalence of eligiblity remains the same.

# figure for baseline characteristics between trial and COMPASS.

# number of hospcodes.
# get the compass_meds dataaset, which is the main compass dataset now.

dfc = read_csv('P:\\ORD_Deo_202008039D\\COMPASS_CABG\\sas_data\\compass_meds.csv')

dfc %>% count(hospcode)

length(unique(dfc$hospcode))


dfc$young = with(dfc, ifelse(age < 65, 1, 0))

dfc %>% count(young)


# tests for table - comparing COMPASS and our patients.

# dm 

t = c(1448, 15586)

dm = prop.test(x = c(609, 7894), n = c(1448, 15586))
dm

# prior mi 

p_mi = prop.test(x = c(528, 6355), n = t)

p_mi

# PAD 

pad_c = prop.test(x = c(249, 3825), n = c(1448, 15586))

pad_c

# hf 

hf_c = prop.test(x = c(351, 7311), n = c(1448, 15586))

hf_c

# ckd 

ckd_c = prop.test(x = c(202, 3660), n = c(1448, 11586))

ckd_c

# ace 

ace_c = prop.test(x = c(799, 7445), c = t)

ace_c

binconf(202,1448, method = "wilson")
binconf(3660, 15586, method = "wilson")

ccb_c = prop.test(x = c(261, 4998), n = t)

ccb_c

beta_c = prop.test(x = c(1316, 11140), n = t)
beta_c

lipid_c = prop.test(x = c(1294, 13080), n = t)
lipid_c


# looking at inclusion criteria comorbidities by year.


dfc$surg_year = year(dfc$surgdate)


vars = c( "age", "sex","bmi","csmok", "cvd","curdiur",
          "pvd", "egfr", "ckd", "chol_mmol","diabetes_c","hf","tobacco_use",
          "prior_mi")

factorvars = c( "sex","csmok", "curdiur",
                "pvd","ckd", "diabetes_c","hf","tobacco_use",
                "prior_mi")



t_1 = tableone::CreateTableOne(vars = vars, factorVars = factorvars,
                             data = dfc, strata = c("surg_year"))


# details regarding the operative procedure.

summary(dfc$cpbt)

# if cpbt == 0 , then OPCAB done.

dfc$opcab = with(dfc, ifelse(cpbt == 0, 1, 0))

dfc %>% count(opcab)

prop.table(table(dfc$opcab))


dfc %>% count(ltm)

summary(dfc$ltm)

# LMCA disease 

dfc$lm_stenosis = with(dfc, ifelse(ltm > 50, 1, 0))

dfc %>% count(lm_stenosis)

prop.table(table(dfc$lm_stenosis))

prop.test(x = c(353, 3937), n = c(1448, 14665))


prop.test

# TVD
# tvd disease 

dfc$tvd = with(dfc, ifelse((cpt01 == 33533 |cpt01 %in% c(33518,33519,33521,33522,33523)), 1, 0))

dfc %>% count(tvd)

